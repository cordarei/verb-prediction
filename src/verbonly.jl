include("common.jl")

type TrainingData
    fs::Vector{Int}
    vs::Vector{Int}
    doffs::Vector{Int}
end

function readtrain(filename::String)
    numtokens = 0
    doffs = [1]
    #do one pass to count everything
    open(filename) do f
        for line in eachline(f)
            ss = split(line)
            if ss[1] == "document"
                dlen = int(ss[2])
                numtokens += dlen
                push!(doffs, numtokens + 1)
            end
        end
    end

    # allocate storage and read in data
    fs = Array(Int, numtokens)
    vs = Array(Int, numtokens)
    i = 1
    open(filename) do f
        for line in eachline(f)
            ss = split(line)
            if ss[1] != "document"
                vs[i] = int(ss[1])
                i += 1
            end
        end
    end
    @assert(i == doffs[end])
    @assert(i == numtokens + 1)
    @debugonly numtokens
    @debugonly length(doffs)

    return TrainingData(fs, vs, doffs)
end

type Counts
    doctopiccounts::Matrix{Int} #KxD
    wordtopiccounts::Matrix{Int} #KxV
    topicwordtotals::Vector{Int} #K
end

const samplebuffer = Array(Float64, 0)
function sample_topic(K, V, v, d, α, β, counts::Counts, dec_k=0)
    if length(samplebuffer) != K
        resize!(samplebuffer, K)
    end

    if dec_k > 0
        counts.doctopiccounts[dec_k,d] -= 1
        counts.wordtopiccounts[dec_k,v] -= 1
        counts.topicwordtotals[dec_k] -= 1
    end

    ks = samplebuffer
    for k = 1:K
        ks[k] =
            (counts.doctopiccounts[k,d] + α[k]) *
            (counts.wordtopiccounts[k,v] + β) /
            (counts.topicwordtotals[k] + V*β)
    end

    k = sample(weights(ks))::Int

    counts.doctopiccounts[k,d] += 1
    counts.wordtopiccounts[k,v] += 1
    counts.topicwordtotals[k] += 1

    k
end

function logmultibeta(alpha)
    sum(lgamma, alpha) - lgamma(sum(alpha))
end

function loglikelihood2(K, D, V, data::TrainingData, counts::Counts, α, β)
    l = 0.
    for d=1:D
        l += logmultibeta(counts.doctopiccounts[:,d] + α)
        l -= logmultibeta(α)
    end
    for k=1:K
        l += logmultibeta(counts.wordtopiccounts[k,:] + β)
        l -= logmultibeta(ones(V)*β)
    end
    l
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

    # vocab
    vocab = readvocab("vocab")
    @debug V = vocab.size

    # training data
    data = readtrain("train")
    D = length(data.doffs) - 1
    doctotals = Array(Int, D)
    for d = 1:D
        doctotals[d] = data.doffs[d + 1] - data.doffs[d]
    end

    # counts
    counts = Counts(zeros(Int, K, D), zeros(Int, K, V), zeros(Int, K))
    hist = DirichletHistogram()

    # random initialization
    for d = 1:D
        ifirst = data.doffs[d]
        ilast = data.doffs[d+1] - 1
        for i = ifirst:ilast
            @inbounds data.fs[i] = sample_topic(K, V, data.vs[i], d, α, β, counts)
            @debugonlyif in(i, 1:fld(endof(data.fs),10):endof(data.fs)) fld(100*i, endof(data.fs))
        end
    end

    #check doffs
    @debugonlynoshow for d=1:D
        nd = data.doffs[d+1] - data.doffs[d]
        zd = sum(counts.doctopiccounts[:,d])
        if (nd <= 0) 
            @show data.doffs[d-1:d+2]
        end
        if (nd != zd)
            @show (d, nd, zd)
        end
        @assert(sum(counts.doctopiccounts[:,d]) == (data.doffs[d+1] - data.doffs[d]))
    end
    @debugonly loglikelihood(K, D, V, data, counts, α, β)
    flush_cstdio()

    # do training
    for x = 1:iters
        @debugonly x
        @debugonlyif (0 == x % 5) loglikelihood(K, D, V, data, counts, α, β)

        for d = 1:D
            ifirst = data.doffs[d]
            ilast = data.doffs[d+1] - 1
            for i = ifirst:ilast
                @inbounds data.fs[i] = sample_topic(K, V, data.vs[i], d, α, β, counts, data.fs[i])
            end
        end

        if x > burnin && 0 == x % estimationinterval
            dirichlet_histogram!(counts.doctopiccounts, doctotals, hist)
            dirichlet_estimate!(hist, prioriters, α)
            α0 = sum(α)
            # @debugonly α
            @debugonly α0
            @debugonly maximum(α)
            @debugonly maximum(α) / mean(α)
            # TODO: estimate β prior?
        end
        flush_cstdio()
    end

    # save word-topic distributions
    if !isdir("topics")
        mkdir("topics")
    end
    cd("topics") do
        dumptopics(counts.wordtopiccounts, vocab, β, 20)
    end

    # try to free up some memory
    data = nothing # we only need to counts for the evaluation
    gc()

    # do evaluation
    totallogprob = 0
    testlength = 0
    d = 1
    @debugonlynoshow lastshown = 1
    open("test") do f
        open("evaluation", "w") do outf
        while !eof(f)
            line = chomp(readline(f))
            ss = split(line)
            assert(ss[1] == "document")

            n_d = int(ss[2])
            fs = Array(Int, n_d, R)
            vs = Array(Int, n_d)

            testlength += n_d

            @debugonlynoshow if d / lastshown >= 10
                lastshown = d
                print("$(d)...")
            end
            flush_cstdio()

            for i = 1:n_d
                line = chomp(readline(f))
                vs[i] = int(line)
            end

            # do L2R approx.
            particlecounts = [
                Counts(
                    zeros(Int, K, 1),
                    copy(counts.wordtopiccounts),
                    copy(counts.topicwordtotals))
                for r=1:R
            ]

            for i = 1:n_d
                prob = 0
                for r = 1:R
                    newcounts = particlecounts[r]
                    #resample
                    for j = 1:i-1
                        @inbounds fs[j,r] = sample_topic(K, V, vs[j], 1, α, β, newcounts, fs[j,r])
                    end

                    #calculate p(w)
                    v = vs[i]
                    for k = 1:K
                        @inbounds prob +=
                            ((newcounts.wordtopiccounts[k,v] + β) /
                            (newcounts.topicwordtotals[k] + β*V)) *
                            ((newcounts.doctopiccounts[k] + α[k]) /
                            (i - 1 + α0))
                    end

                    @inbounds fs[i,r] = sample_topic(K, V, v, 1, α, β, newcounts)
                end

                #TODO: find v w/ highest prob?

                prob /= R
                totallogprob += log2(prob)

                println(outf, "$d $i $prob")
            end

            d += 1
        end
        end

        ppl = 2^(-totallogprob / testlength)
        println("\ntest perplexity: $ppl")
    end
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
