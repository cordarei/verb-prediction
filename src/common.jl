using Base.Collections
using StatsBase

import Base: getindex, push!, collect, start, next, done

# debug printing
const DEBUG = (!haskey(ENV, "DEBUG") ? false : (ENV["DEBUG"] == "1" || ENV["DEBUG"] == "true"))
macro debug(ex)
    if DEBUG
        :(@show $(esc(ex)))
    else
        esc(ex)
    end
end
macro debugonly(ex)
    if DEBUG
        :(@show $(esc(ex)))
    end
end
macro debugonlynoshow(ex)
    if DEBUG
        # :(@show $(esc(ex)))
        ex
    end
end
macro debugonlyif(cond, ex)
    if DEBUG
        quote
            if $(esc(cond))
                @show $(esc(ex))
            end
        end
    end
end

# Vocab
# conversions between word <-> index

type Vocab
    w2id::Dict{UTF8String, Int}
    ws::Vector{UTF8String}
    size::Int

    function Vocab(dict::Dict{UTF8String, Int})
        ws = Array(UTF8String, length(dict))
        for w in keys(dict)
            ws[dict[w]] = w
        end
        new(dict, ws, length(dict))
    end
end

getindex(v::Vocab, i::Int) = v.ws[i]
getindex(v::Vocab, s::UTF8String) = v.w2id[s]

function readvocab(filename::String)
    vocab = Dict{UTF8String, Int}()
    open(filename) do f
        for line in eachline(f)
            ss = split(line, "\t")
            word, wid = (ss[1], int(ss[2]))
            vocab[word] = wid
        end
    end
    Vocab(vocab)
end


# extra methods for StatsBase.WeightVector
getindex(wv::WeightVec, i) = wv.values[i]
start(wv::WeightVec) = start(wv.values)
next(wv::WeightVec, i) = next(wv.values, i)
done(wv::WeightVec, i) = done(wv.values, i)


# TopN
# keep largest N items by score

type TopN{V, T<:Real}
    N::Int
    q::PriorityQueue{V, T}

    TopN(n) = new(n, PriorityQueue{V, T}())
end

function push!(topn::TopN, item, score)
    enqueue!(topn.q, item, score)
    if (length(topn.q) > topn.N)
        return dequeue!(topn.q)
    end
end

function collect(topn::TopN)
    items = collect(topn.q)
    sort!(items, by=x->x[2], rev=true)
    items
end


# dump topic-word distributions
function dumptopics(wordtopiccounts::Matrix{Int}, vocab::Vocab, β, n)
    topictotals = sum(wordtopiccounts, 2)
    for k = 1:size(wordtopiccounts,1)
        topwords = TopN{UTF8String,Float64}(n)
        for w = 1:vocab.size
            pw = (wordtopiccounts[k,w] + β) / (topictotals[k] + β*vocab.size)
            push!(topwords, vocab[w], pw)
        end
        open("topic-$k", "w") do f
            for pair in collect(topwords)
                w,p = pair
                println(f, "$w\t$p")
            end
        end
    end
end


function dirichlet_histogram(observationcounts::Matrix{Int}, totals::Vector{Int})
    (K, D) = size(observationcounts)
    maxNk = maximum(observationcounts)
    maxN = maximum(totals)

    Nk = zeros(Int, maxNk, K)
    N = zeros(Int, maxN)

    for d = 1:D
        for k = 1:K
            if observationcounts[k,d] > 0
                Nk[observationcounts[k,d], k] += 1
            end
        end
        N[totals[d]] += 1
    end

    (Nk, N)
end

function dirichlet_estimate!(observationcounts::Matrix{Int}, totals::Vector{Int}, α::Vector{Float64}, iters::Int)
    K = size(observationcounts, 1)
    (Nk, N) = dirichlet_histogram(observationcounts, totals)

    for i = 1:iters
        D = 0
        S = 0
        α0 = sum(α)

        for n = 1:length(N)
            D += 1 / (n - 1 + α0)
            S += N[n]*D
        end

        for k = 1:K
            D = 0
            Sk = 0
            for n = 1:size(Nk,1)
                D += 1 / (n - 1 + α[k])
                Sk += Nk[n,k]*D
            end
            α[k] *= Sk/S
        end
    end

    α
end
