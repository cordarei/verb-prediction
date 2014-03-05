include("common.jl")


type TrainingData
    fs::Vector{Int}
    vs::Vector{Int}
    doffs::Vector{Int}
    as::Vector{Int}
    afs::Vector{Int}
    adoffs::Vector{Int}
end

function findrange(afs, i, rng::Ranges)
    fst = indexof(i, afs, rng)
    if fst == 0
        return first(rng):first(rng)-1 # return empty range beginning at `first(rng)`
    else
        lst = indexof(i, afs, reverse(rng))
        return fst:lst
    end
end

function readtrain(filename::String)
    numverbs = 0
    numargs = 0
    doffs = [1]
    adoffs = [1]
    #do one pass to count everything
    open(filename) do f
        for line in eachline(f)
            ss = split(line)
            if ss[1] == "document"
                dlen = int(ss[2])
                adlen = int(ss[2])
                numverbs += dlen
                numargs += adlen
                push!(doffs, numverbs + 1)
                push!(adoffs, numargs + 1)
            end
        end
    end

    # allocate storage and read in data
    fs = Array(Int, numverbs)
    vs = Array(Int, numverbs)
    as = Array(Int, numargs)
    afs = Array(Int, numargs)

    i = 1
    j = 1
    open(filename) do f
        while !eof(f)
            line = chomp(readline(f))
            ss = split(line)
            @assert ss[1] = "document"
            n = int(ss[2])
            m = int(ss[3])
            # first the verbs
            for x = 1:n
                line = chomp(readline(f))
                ss = split(line)
                vs[i] = int(ss[1])
                i += 1
            end
            # then the args
            for x = 1:m
                line = chomp(readline(f))
                ss = split(line)
                as[j] = int(ss[1])
                afs[j] = int(ss[2])
                j += 1
            end
        end
    end
    @assert(i == doffs[end])
    @assert(i == numverbs + 1)
    @assert(j == adoffs[end])
    @assert(j == numargs + 1)

    return TrainingData(fs, vs, doffs, as, afs, adoffs)
end


type Counts
    doctopiccounts::Matrix{Int} #KxD
    verbtopiccounts::Matrix{Int} #KxV
    topicverbtotals::Vector{Int} #K
    argtopiccounts::Matrix{Int} #KxA
    topicargtotals::Vector{Int} #K
end

function argterm(A, k, as, Y, counts::Counts)
    #calculate argument term
    y = 1.
    base = Y*A + counts.topicargtotals[k]
    for i = 1:length(as)
        y *= (base + i - 1)
        a = as[i]
        if rfind(a, as) == i
            base2 = Y + counts.argtopiccounts[k,a]
            for j = 1:countin(a, as, 1:i)
                y *= (base2 + j - 1)
            end
        end
    end
    return y
end

const samplebuffer = Array(Float64, 0)
function sample_topic(K, V, A, v, as, d, α, β, Y, counts::Counts, dec_k=0)
    if length(samplebuffer) != K
        resize!(samplebuffer, K)
    end

    if dec_k > 0
        counts.doctopiccounts[dec_k,d] -= 1
        counts.verbtopiccounts[dec_k,v] -= 1
        counts.topicverbtotals[dec_k] -= 1
        for a in as
            counts.argtopiccounts[dec_k,a] -= 1
            counts.topicargtotals[dec_k] -= 1
        end
    end

    ks = samplebuffer
    for k = 1:K
        #calculate argument term
        y = 1.
        base = Y*A + counts.topicargtotals[k]
        for i = 1:length(as)
            y *= (base + i - 1)
            a = as[i]
            if rfind(a, as) == i
                base2 = Y + counts.argtopiccounts[k,a]
                for j = 1:countin(a, as, 1:i)
                    y *= (base2 + j - 1)
                end
            end
        end

        ks[k] =
            (counts.doctopiccounts[k,d] + α[k]) *
            (counts.verbtopiccounts[k,v] + β) /
            (counts.topicverbtotals[k] + V*β) *
            y
    end

    k = sample(weights(ks))::Int

    counts.doctopiccounts[k,d] += 1
    counts.verbtopiccounts[k,v] += 1
    counts.topicverbtotals[k] += 1
    for a in as
        counts.argtopiccounts[dec_k,a] += 1
        counts.topicargtotals[dec_k] += 1
    end

    k
end

function loglikelihood(K, D, V, A, data::TrainingData, counts::Counts, α, β, Y)
    α0 = sum(α)
    l = 0.
    l += D * (lgamma(α0) - sum(lgamma, α))
    l += K * (lgamma(V * β) - V * lgamma(β))

    for d=1:D
        l -= lgamma(data.doffs[d+1] - data.doffs[d] + α0)
        for k=1:K
            l += lgamma(counts.doctopiccounts[k,d] + α[k])
        end
    end

    for k=1:K
        l -= lgamma(counts.topicverbtotals[k] + V * β)
        for v=1:V
            l += lgamma(counts.verbtopiccounts[k,v] + β)
        end

        l -= lgamma(counts.topicargtotals[k] + A * Y)
        for a=1:A
            l += lgamma(counts.argtopiccounts[k,a] + Y)
        end
    end

    l
end

function run()
    OUT = STDOUT
    @debug showprogress("Starting up", "... Started.\n")
    # for debugging purposes
    @debug dumpfile = open("dump", "w")

    # parameters
    args = getargs()
    K = args["K"]::Int
    α = (args["alpha"]::Float64 / K) * ones(K)
    α0 = sum(α)
    β = args["beta"]::Float64
    Y = β
    iters = args["iters"]::Int
    burnin = args["burnin"]::Int
    estimationinterval = args["priorinterval"]::Int
    prioriters = args["prioriters"]::Int
    R = args["particles"]::Int

    printargs(OUT, args)
    println(OUT)

    # vocab
    vocab = readvocab("verb-vocab")
    V = vocab.size
    argvocab = readvocab("arg-vocab")
    A = argvocab.size

    println(OUT, "V = $V")
    println(OUT, "A = $A")

    # training data
    data = readtrain("train")
    D = length(data.doffs) - 1
    doctotals = Array(Int, D)
    for d = 1:D
        doctotals[d] = data.doffs[d + 1] - data.doffs[d]
    end

    println(OUT, "D = $D")

    # counts
    counts = Counts(zeros(Int, K, D), zeros(Int, K, V), zeros(Int, K), zeros(Int, K, A), zeros(Int, K))
    hist = DirichletHistogram()
    betahist = DirichletHistogram()
    gammahist = DirichletHistogram()

    # random initialization
    @debug showprogress("Intializing:", 0, endof(data.vs))
    @debug curnth = 0
    for d = 1:D
        ifirst = data.doffs[d]
        ilast = data.doffs[d+1] - 1
        afirst = data.adoffs[d]
        alast = data.adoffs[d+1] - 1
        for i = ifirst:ilast
            rng = findrange(data, i, afirst:alast)
            afirst = last(rng) + 1
            @inbounds data.fs[i] = sample_topic(K, V, A, data.vs[i], sub(data.as, rng), d, α, β, Y, counts)
        end

        @debug if ilast > curnth * fld(endof(data.vs), 20) || ilast == endof(data.vs)
            curnth += 1
            showprogress("Intializing:", ilast, endof(data.vs))
        end
    end

    # do training
    for x = 1:iters
        @debug showprogress("Training:", x, iters)

        for d = 1:D
            ifirst = data.doffs[d]
            ilast = data.doffs[d+1] - 1
            afirst = data.adoffs[d]
            alast = data.adoffs[d+1] - 1
            for i = ifirst:ilast
                rng = findrange(data, i, afirst:alast)
                afirst = last(rng) + 1
                @inbounds data.fs[i] = sample_topic(K, V, A, data.vs[i], sub(data.as, rng), d, α, β, Y, counts, data.fs[i])
            end
        end

        # optimize priors
        α_converged = true
        β_converged = true
        Y_converged = true
        if x > burnin && 0 == x % estimationinterval
            dirichlet_histogram!(counts.doctopiccounts, doctotals, hist)
            (_, α_converged) = dirichlet_estimate!(hist, prioriters, α)
            α0 = sum(α)
            # TODO: estimate β prior?
            # dirichlet_histogram!(counts.verbtopiccounts', reshape(sum(counts.verbtopiccounts, 2), K), betahist)
            # (β0, β_converged) = dirichlet_estimate(betahist, prioriters, β*V)
            # β = β0 / V
            # estimate γ prior
            # dirichlet_histogram!(counts.argtopiccounts', reshape(sum(counts.argtopiccounts, 2), K), gammahist)
            # (Y0, Y_converged) = dirichlet_estimate(gammahist, prioriters, Y*A)
            # Y = Y0 / A
        end

        @debug if x % 10 == 0 || x == iters
            println(dumpfile, "# x = $x")
            l = loglikelihood(K, D, V, A, data, counts, α, β, Y)
            println(dumpfile, "L = $l")
            println(dumpfile, "α0 = $(α0)")
            println(dumpfile, "α = [")
            for a in α
                println(dumpfile, "  $a")
            end
            println(dumpfile, "]")
            if !α_converged println(dumpfile, "# α estimation did not converge!") end
            println(dumpfile, "β = $β")
            if !β_converged println(dumpfile, "# β estimation did not converge!") end
            println(dumpfile, "γ = $Y")
            if !Y_converged println(dumpfile, "# γ estimation did not converge!") end
            println(dumpfile)
        end
    end

    # save word-topic distributions
    @debug showprogress("Saving model", ".")
    if !isdir("topics")
        mkdir("topics")
    end
    cd("topics") do
        dumptopics(counts.verbtopiccounts, vocab, β, 20)
    end
    if !isdir("argtopics")
        mkdir("argtopics")
    end
    cd("argtopics") do
        dumptopics(counts.argtopiccounts, argvocab, Y, 20)
    end

    # save model (per-topic α, token counts)
    @debug showprogress("Saving model", "..")
    open("model", "w") do f
        println(f, "# Topic α Nk")
        topictotals = sum(counts.verbtopiccounts, 2)
        @debug assert(topictotals == sum(counts.doctopiccounts, 2))
        for k = 1:K
            println(f, "$k\t$(α[k])\t$(topictotals[k])")
        end
        println(f, "# β = $β")
        println(f, "# γ = $Y")
    end
    @debug showprogress("Saving model", "... DONE.\n")

    # try to free up some memory
    data = nothing # we only need the counts for the evaluation
    gc()

    # do evaluation
    println(OUT)
    println(OUT, "# Evaluation:")

    totallogprob = 0.
    testlength = 0
    d = 1

    # allocate local particle counts for test
    particlecounts = [
        Counts(
            zeros(Int, K, 1),
            copy(counts.verbtopiccounts),
            copy(counts.topicverbtotals),
            copy(counts.argtopiccounts),
            copy(counts.topicargtotals))
        for r=1:R
    ]

    open("test") do f
        while !eof(f)
            line = chomp(readline(f))
            ss = split(line)
            @assert ss[1] == "document"
            @debug showprogress("Testing document:", d)

            n_d = int(ss[2])
            fs = Array(Int, n_d, R)
            vs = Array(Int, n_d)
            vn = Array(Int, n_d)
            nargs = int(ss[3])
            as = Array(Int, nargs)
            an = Array(Int, nargs)
            afs = Array(Int, nargs)

            testlength += n_d

            for i = 1:n_d
                line = chomp(readline(f))
                ss = split(line)
                vs[i] = int(ss[1])
                vn[i] = int(ss[2])
            end
            for i = 1:nargs
                line = chomp(readline(f))
                ss = split(line)
                as[i] = int(ss[1])
                an[i] = int(ss[2])
                afs[i] = int(ss[3])
            end

            # do L2R approx.
            for i = 1:n_d
                prob = 0.
                v = vs[i]

                rng = findrange(afs, i, 1:nargs)
                while an[last(rng)] > vn[i]
                    rng = first(rng):last(rng)-1
                end
                verbargs = sub(as, rng)

                for r = 1:R
                    newcounts = particlecounts[r]
                    #resample
                    afirst = 1
                    for j = 1:i-1
                        rng = findrange(afs, i, afirst:nargs)
                        while an[last(rng)] > vn[i]
                            rng = first(rng):last(rng)-1
                        end
                        afirst = last(rng) + 1
                        @inbounds fs[j,r] = sample_topic(K, V, A, vs[j], sub(as, rng), 1, α, β, Y, newcounts, fs[j,r])
                    end

                    #calculate p(w)
                    for k = 1:K
                        y = argterm(A, k, verbargs, Y, counts)
                        @inbounds prob +=
                            ((newcounts.verbtopiccounts[k,v] + β) /
                            (newcounts.topicverbtotals[k] + β*V)) *
                            ((newcounts.doctopiccounts[k] + α[k]) /
                            (i - 1 + α0)) *
                            y
                    end

                    @inbounds fs[i,r] = sample_topic(K, V, A, v, verbargs, 1, α, β, Y, newcounts)
                end

                #TODO: find v w/ highest prob?

                prob /= R
                totallogprob += log2(prob)

                println(OUT, "score($d,$i) = $prob # $(vocab[v])")
            end

            d += 1

            # reset particle counts to train counts for next document
            for r=1:R
                particlecounts[r].doctopiccounts[:] = 0
                particlecounts[r].verbtopiccounts[:] = counts.verbtopiccounts
                particlecounts[r].topicverbtotals[:] = counts.topicverbtotals
                particlecounts[r].argtopiccounts[:] = counts.argtopiccounts
                particlecounts[r].topicargtotals[:] = counts.topicargtotals
            end

        end
        @debug showprogress("Testing document:", "$(d-1) DONE.\n")

        println(OUT)
        println(OUT, "# Results:")
        println(OUT, "total log probability = $totallogprob")

        ppl = 2^(-totallogprob / testlength)

        println(OUT, "perplexity = $ppl")
        println(OUT, "normalized perplexity = $(ppl / vocab.size)")
    end

    @debug close(dumpfile)
end

run()

# @time run()

# using Base.Profile
# Profile.init(10^8, 0.001)

# precompile(run, ())

# @profile run()

# open("profile", "w") do f
#     Profile.print(f, C=true, cols=5000)
# end
# open("profile_flat", "w") do f
#     Profile.print(f, format=:flat, C=true, cols=5000)
# end
