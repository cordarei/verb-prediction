include("common.jl")


type TrainingData
    fs::Vector{Int}
    vs::Vector{Int}
    vfs::Vector{Int} # indexes into fs
    doffs::Vector{Int} # document offsets into fs
    vdoffs::Vector{Int} # document offsets into vs
end

function readtrain(filename::String)
    numvars = 0
    numverbs = 0
    doffs = [1]
    vdoffs = [1]
    #do one pass to count everything
    open(filename) do f
        for line in eachline(f)
            ss = split(line)
            if ss[1] == "document"
                dlen = int(ss[2])
                vdlen = int(ss[3])
                numvars += dlen
                numverbs += vdlen
                push!(doffs, numvars + 1)
                push!(vdoffs, numverbs + 1)
            end
        end
    end

    # allocate storage and read in data
    fs = Array(Int, numvars)
    vs = Array(Int, numverbs)
    vfs = Array(Int, numverbs)
    i = 1
    d = 1
    open(filename) do f
        while !eof(f)
            line = chomp(readline(f))
            ss = split(line)
            @assert ss[1] == "document"
            n = int(ss[2])
            m = int(ss[3])
            for x = 1:m
                line = chomp(readline(f))
                ss = split(line)
                vs[i] = int(ss[1])
                vfs[i] = int(ss[3]) + doffs[d] - 1
                i += 1
            end
            d += 1
        end
    end
    @assert(i == vdoffs[end])
    @assert(i == numverbs + 1)

    return TrainingData(fs, vs, vfs, doffs, vdoffs)
end


type Counts
    doctopiccounts::Matrix{Int} #KxD
    wordtopiccounts::Matrix{Int} #KxV
    topicwordtotals::Vector{Int} #K
end

const samplebuffer = Array(Float64, 0)
function sample_topic(K, V, vs, d, α, β, counts::Counts, dec_k=0)
    if length(samplebuffer) != K
        resize!(samplebuffer, K)
    end

    if dec_k > 0
        counts.doctopiccounts[dec_k,d] -= 1
        for v in vs
            counts.wordtopiccounts[dec_k,v] -= 1
            counts.topicwordtotals[dec_k] -= 1
        end
    end

    ks = samplebuffer
    for k = 1:K
        wordfactor = 1.
        base = β*V + counts.topicwordtotals[k]
        for i = 1:length(vs)
            wordfactor *= 1 / (base + i - 1)
            v = vs[i]
            if rfind(v, vs) == i
                base2 = β + counts.wordtopiccounts[k,v]
                for j = 1:countin(v, vs, 1:i)
                    wordfactor *= (base2 + j - 1)
                end
            end
        end
        ks[k] = (counts.doctopiccounts[k,d] + α[k]) * wordfactor
            # (counts.wordtopiccounts[k,v] + β) /
            # (counts.topicwordtotals[k] + V*β)
    end

    k = sample(weights(ks))::Int

    counts.doctopiccounts[k,d] += 1
    for v in vs
        counts.wordtopiccounts[k,v] += 1
        counts.topicwordtotals[k] += 1
    end

    k
end

function loglikelihood(K, D, V, data::TrainingData, counts::Counts, α, β)
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
        l -= lgamma(counts.topicwordtotals[k] + V * β)
        for v=1:V
            l += lgamma(counts.wordtopiccounts[k,v] + β)
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
    iters = args["iters"]::Int
    burnin = args["burnin"]::Int
    estimationinterval = args["priorinterval"]::Int
    prioriters = args["prioriters"]::Int
    R = args["particles"]::Int

    printargs(OUT, args)
    println(OUT)

    # vocab
    vocab = readvocab("vocab")
    V = vocab.size

    println(OUT, "V = $V")

    # training data
    data = readtrain("train")
    D = length(data.doffs) - 1
    doctotals = Array(Int, D)
    for d = 1:D
        doctotals[d] = data.doffs[d + 1] - data.doffs[d]
    end

    println(OUT, "D = $D")

    # counts
    counts = Counts(zeros(Int, K, D), zeros(Int, K, V), zeros(Int, K))
    hist = DirichletHistogram()
    betahist = DirichletHistogram()

    # random initialization
    @debug showprogress("Intializing:", 0, endof(data.fs))
    @debug curnth = 0
    for d = 1:D
        ifirst = data.doffs[d]
        ilast = data.doffs[d+1] - 1
        vfirst = data.vdoffs[d]
        vlast = data.vdoffs[d+1] - 1
        for i = ifirst:ilast
            rng = findrange(data.vfs, i, vfirst:vlast)
            vfirst = last(rng) + 1
            @myinbounds data.fs[i] = sample_topic(K, V, sub(data.vs, rng), d, α, β, counts)
        end
        @debug if ilast > curnth * fld(endof(data.fs), 20) || ilast == endof(data.fs)
            curnth += 1
            showprogress("Intializing:", ilast, endof(data.fs))
        end
    end

    @debug begin
        @assert sum(counts.doctopiccounts) == data.doffs[end] - 1
        @assert sum(counts.wordtopiccounts) == data.vdoffs[end] - 1
        @assert reshape(sum(counts.doctopiccounts, 1), D) == (data.doffs[2:end] .- data.doffs[1:end-1])
        @assert reshape(sum(counts.wordtopiccounts, 2), K) == counts.topicwordtotals
    end

    # do training
    for x = 1:iters
        @debug showprogress("Training:", x, iters)

        for d = 1:D
            ifirst = data.doffs[d]
            ilast = data.doffs[d+1] - 1
            vfirst = data.vdoffs[d]
            vlast = data.vdoffs[d+1] - 1
            for i = ifirst:ilast
                rng = findrange(data.vfs, i, vfirst:vlast)
                vfirst = last(rng) + 1
                @myinbounds data.fs[i] = sample_topic(K, V, sub(data.vs, rng), d, α, β, counts, data.fs[i])
                # @myinbounds data.fs[i] = sample_topic(K, V, data.vs[i], d, α, β, counts, data.fs[i])
            end
        end

        # optimize priors
        α_converged = true
        β_converged = true
        if x > burnin && 0 == x % estimationinterval
            dirichlet_histogram!(counts.doctopiccounts, doctotals, hist)
            (_, α_converged) = dirichlet_estimate!(hist, prioriters, α)
            α0 = sum(α)
            # TODO: estimate β prior?
            # dirichlet_histogram!(counts.wordtopiccounts', reshape(sum(counts.wordtopiccounts, 2), K), betahist)
            # (β0, β_converged) = dirichlet_estimate(betahist, prioriters, β*V)
            # β = β0 / V
        end

        @debug if x % 10 == 0 || x == iters
            println(dumpfile, "# x = $x")
            l = loglikelihood(K, D, V, data, counts, α, β)
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
            println(dumpfile)
        end
    end

    # save word-topic distributions
    @debug showprogress("Saving model", ".")
    if !isdir("topics")
        mkdir("topics")
    end
    cd("topics") do
        dumptopics(counts.wordtopiccounts, vocab, β, 20)
    end
    @debug showprogress("Saving model", "..")
    open("model", "w") do f
        println(f, "# Topic α Nk")
        topictotals = reshape(sum(counts.wordtopiccounts, 2), K)
        @debug begin
            @assert sum(counts.doctopiccounts) == data.doffs[end] - 1
            @assert sum(counts.wordtopiccounts) == data.vdoffs[end] - 1
            @assert reshape(sum(counts.doctopiccounts, 1), D) == (data.doffs[2:end] .- data.doffs[1:end-1])
            @assert topictotals == counts.topicwordtotals
        end
        for k = 1:K
            println(f, "$k\t$(α[k])\t$(topictotals[k])")
        end
        println(f, "# β = $β")
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
            copy(counts.wordtopiccounts),
            copy(counts.topicwordtotals))
        for r=1:R
    ]
    vbuf = Array(Int, 0) # allocate a buffer for collecting verbs sharing common frame variable for sampling
    vbufend = 0

    open("test") do f
        while !eof(f)
            line = chomp(readline(f))
            ss = split(line)
            @assert ss[1] == "document"

            n_d = int(ss[2])
            fs = Array(Int, n_d, R)
            @debug fill!(fs, 0)
            nverbs = int(ss[3])
            vs = Array(Int, nverbs)
            vfs = Array(Int, nverbs)
            vns = Array(Int, nverbs)

            testlength += nverbs
            # cheat and just allocate the max size needed
            if length(vbuf) < nverbs
                resize!(vbuf, nverbs)
            end

            @debug showprogress("Testing document:", d)

            for i = 1:nverbs
                line = chomp(readline(f))
                ss = split(line)
                vs[i] = int(ss[1])
                vns[i] = int(ss[2])
                vfs[i] = int(ss[3])
            end

            # sort by token index (we want to evaluate verbs in LR order)
            idxs = [1:nverbs]
            sort!(idxs, by= i -> vns[i])
            vs[:] = vs[idxs]
            vfs[:] = vfs[idxs]
            vns[:] = vns[idxs]

            # do L2R approx.
            for i = 1:nverbs
                prob = 0.
                v = vs[i]
                fi = vfs[i]

                for r = 1:R
                    newcounts = particlecounts[r]
                    #resample
                    for j = 1:n_d
                        vbufend = 0
                        for x = 1:i-1
                            if vfs[x] == j
                                vbufend += 1
                                vbuf[vbufend] = vs[x]
                            end
                        end
                        if vbufend > 0
                            @myinbounds fs[j,r] = sample_topic(K, V, sub(vbuf, 1:vbufend), 1, α, β, newcounts, fs[j,r])
                        end
                    end
                end

                vbufend = 0
                for x = 1:i # include vs[i]
                    if vfs[x] == fi
                        vbufend += 1
                        vbuf[vbufend] = vs[x]
                    end
                end

                for r = 1:R
                    newcounts = particlecounts[r]

                    #calculate p(w)
                    for k = 1:K
                        @myinbounds prob +=
                            ((newcounts.wordtopiccounts[k,v] + β) /
                            (newcounts.topicwordtotals[k] + β*V)) *
                            ((newcounts.doctopiccounts[k] + α[k]) /
                            (i - 1 + α0))
                    end

                    if vbufend > 1
                        # correct for double-counted verbs in `vbuf[1:end-1]`:
                        #   add 1 count for new verb, then decrement as normal
                        newcounts.wordtopiccounts[fs[fi,r],v] += 1
                        newcounts.topicwordtotals[fs[fi,r]] += 1
                    end
                    @myinbounds fs[fi,r] = sample_topic(K, V, sub(vbuf, 1:vbufend), 1, α, β, newcounts, fs[fi,r])
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
                particlecounts[r].wordtopiccounts[:] = counts.wordtopiccounts
                particlecounts[r].topicwordtotals[:] = counts.topicwordtotals
            end

        end
        @debug showprogress("Testing document:", "$(d-1) DONE.\n")

        println(OUT)
        println(OUT, "# Results:")
        println(OUT, "total log probability = $totallogprob")
        println(OUT, "test length = $testlength")

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
