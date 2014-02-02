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
