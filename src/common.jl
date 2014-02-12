using Base.Collections
using StatsBase
using ArgParse

import Base: getindex, setindex!, resize!, push!, collect, start, next, done

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
        :($(esc(ex)))
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


# Command-line Arguments

function getargs()
    s = ArgParseSettings()

    @add_arg_table s begin
        "-K"
            arg_type = Int
            default = 200 # from comment in Mallet code
        "--alpha", "-a"
            help = "The value of α0; each α_k = alpha / K"
            arg_type = Float64
            default = 50.
        "--beta", "-b"
            arg_type = Float64
            default = 0.01
        "--iters", "-i"
            arg_type = Int
            default = 2000
        "--burnin"
            arg_type = Int
            default = 50
        "--prioriters"
            arg_type = Int
            default = 200
        "--priorinterval"
            arg_type = Int
            default = 10
        "--particles", "-R"
            arg_type = Int
            default = 20
    end

    parse_args(s)
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


# Estimating Dirichlet parameters

type DirichletHistogram
    Nk::Matrix{Int} # Nk x K => C_k(n)
    N::Array{Int,1} # Nd => C(n)
    maxNk::Int
    maxN::Int
    K::Int

    DirichletHistogram() = new(Array(Int,0,0), Array(Int, 0), 0, 0, 0)
end

getindex(dh::DirichletHistogram, i1::Int, i2::Int) = dh.Nk[i1,i2]
getindex(dh::DirichletHistogram, i::Int) = dh.N[i]

setindex!(dh::DirichletHistogram, x::Int, i1::Int, i2::Int) = setindex!(dh.Nk, x, i1,i2)
setindex!(dh::DirichletHistogram, x::Int, i::Int) = setindex!(dh.N, x, i)

function resize!(dh::DirichletHistogram, K, maxNk, maxN)
    if size(dh.Nk, 1) < maxNk || size(dh.Nk, 2) < K
        dh.Nk = Array(Int, maxNk, K)
    end
    if length(dh.N) < maxN
        resize!(dh.N, maxN)
    end
    dh.maxNk = maxNk
    dh.maxN = maxN
    dh.K = K
end

function dirichlet_histogram!(observationcounts::Matrix{Int}, totals::Vector{Int}, dh::DirichletHistogram)
    (K, D) = size(observationcounts)
    maxNk = maximum(observationcounts)
    maxN = maximum(totals)
    resize!(dh, K, maxNk, maxN)
    fill!(dh.Nk, 0)
    fill!(dh.N, 0)

    for d = 1:D
        for k = 1:K
            @inbounds n = observationcounts[k,d]
            if n > 0
                @inbounds dh[n,k] += 1
            end
        end
        @inbounds dh[totals[d]] += 1
    end

    dh
end

function dirichlet_histogram(observationcounts::Matrix{Int}, totals::Vector{Int})
    (K, D) = size(observationcounts)
    dirichlet_histogram!(observationcounts, totals, DirichletHistogram(K, D))
end

function dirichlet_estimate!(dh::DirichletHistogram, iters::Int, α::Vector{Float64})
    K = dh.K

    for i = 1:iters
        D = 0
        S = 0
        α0 = sum(α)

        for n = 1:dh.maxN
            D += 1 / (n - 1 + α0)
            @inbounds S += dh[n]*D
        end

        for k = 1:K
            D = 0
            Sk = 0
            for n = 1:dh.maxNk
                @inbounds D += 1 / (n - 1 + α[k])
                @inbounds Sk += dh[n,k]*D
            end
            @inbounds α[k] *= Sk/S
        end
    end

    α
end
