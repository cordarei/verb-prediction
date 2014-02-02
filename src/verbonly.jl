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
        l += logmultibeta(counts.doctopiccounts[:,d] + values(α))
        l -= logmultibeta(α)
    end
    for k=1:K
        l += logmultibeta(counts.wordtopiccounts[k,:] + β)
        l -= logmultibeta(ones(V)*β)
    end
    l
end

function loglikelihood(K, D, V, data::TrainingData, counts::Counts, α, β)
    l = 0.
    l += D * (lgamma(sum(α)) - sum(lgamma, α))
    l += K * (lgamma(V * β) - V * lgamma(β))

    for d=1:D
        l -= lgamma(data.doffs[d+1] - data.doffs[d] + sum(α))
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
    # parameters (TODO: read from config/args)
    K = 10
    α = 0.1
    β = 0.01
    iters = 20
    burnin = 20
    prioriters = 5
    R = 20

    # vocab
    vocab = readvocab("vocab")
    @debug V = vocab.size

    # priors
    α = weights(α * ones(K))

    # training data
    data = readtrain("train")
    D = length(data.doffs) - 1

    # counts
    counts = Counts(zeros(Int, K, D), zeros(Int, K, V), zeros(Int, K))

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
    for d=1:D
        nd = data.doffs[d+1] - data.doffs[d]
        zd = sum(counts.doctopiccounts[:,d])
        @debugonlyif (nd <= 0) data.doffs[d-1:d+2]
        @debugonlyif (nd != zd) (d, nd, zd)
        @assert(sum(counts.doctopiccounts[:,d]) == (data.doffs[d+1] - data.doffs[d]))
    end
    @debugonly loglikelihood(K, D, V, data, counts, α, β)
    @debugonly loglikelihood2(K, D, V, data, counts, α, β)

    # do training
    for x = 1:iters
        @debugonly x
        @debugonlyif (0 == (x - 1) % 5) loglikelihood(K, D, V, data, counts, α, β)

        for d = 1:D
            ifirst = data.doffs[d]
            ilast = data.doffs[d+1] - 1
            for i = ifirst:ilast
                @inbounds data.fs[i] = sample_topic(K, V, data.vs[i], d, α, β, counts, data.fs[i])
            end
        end

        #TODO: optimize α prior
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
                        prob +=
                            ((newcounts.wordtopiccounts[k,v] + β) /
                            (newcounts.topicwordtotals[k] + β*V)) *
                            ((newcounts.doctopiccounts[k] + α[k]) /
                            (i - 1 + sum(α)))
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
        println("test perplexity: $ppl")
    end
end

run()

# using Base.Profile
# Profile.init(10^8, 0.001)

# precompile(run, ())

# @profile run()

# open("profile", "w") do f
#     Profile.print(f, C=true, cols=500)
# end
# open("profile_flat", "w") do f
#     Profile.print(f, format=:flat, C=true, cols=500)
# end
