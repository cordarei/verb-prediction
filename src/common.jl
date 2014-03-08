using Base.Collections
using StatsBase
using ArgParse

import Base: getindex, setindex!, resize!, push!, collect, start, next, done

# debug printing
const DEBUG = (!haskey(ENV, "DEBUG") ? false : (ENV["DEBUG"] == "1" || ENV["DEBUG"] == "true"))
macro debugshow(ex)
    if DEBUG
        :(@show $(esc(ex)))
    else
        esc(ex)
    end
end
macro debug(ex)
    if DEBUG
        :($(esc(ex)))
    end
end

macro myinbounds(ex)
    :(@inbounds $(esc(ex)))
    # esc(ex)
end

macro showrec(ex)
    if typeof(ex) == Expr
        rec = Expr(:block)
        for a in ex.args
            push!(rec.args, :(@showrec $(esc(a))))
        end
        push!(rec.args, :(@show $(esc(ex))))
        return rec
    else
        return :(@show $(esc(ex)))
    end
end

macro checknan(ex)
    # quote
    #     x = $(esc(ex))
    #     if isnan(x) || x == Inf
    #         @showrec $(esc(ex))
    #         @assert !isnan(x)
    #     end
    #     # @assert !isnan($(esc(ex)))
    #     $(esc(ex))
    # end
    esc(ex)
end

function printover(io::IO, s)
    if !is(typeof(io), Base.TTY)
        return
    end
    print(io, "\r")
    print(io, s)
end

function showprogress(msg, x, n)
    nstr = dec(n)
    xstr = lpad(dec(x), length(nstr), ' ')
    progstr = @sprintf "%s %s / %s (%.0f%%)" msg xstr nstr 100*(x/n)
    printover(STDERR, progstr)
    if x >= n
        println(STDERR, " DONE.")
    end
end

function showprogress(msg, x)
    progstr = string(msg, " ", x)
    printover(STDERR, progstr)
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

function printargs(io::IO, args)
    println(io, "# Parameters:")
    for (arg, val) in args
        println(io, "$arg = $val")
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


# some missing utilities
function indexof{T}(x::T, a::AbstractVector{T}, rng::Ranges)
    for i in rng
        if x == a[i]
            return i
        end
    end
    return 0
end
rfind{T}(x::T, a::AbstractVector{T}) = indexof(x, a, endof(a):-1:1)
rfind{T}(x::T, a::AbstractVector{T}, start::Integer) = indexof(x, a, start:-1:1)

function findrange(a, i, rng::Ranges)
    fst = indexof(i, a, rng)
    if fst == 0
        return first(rng):first(rng)-1 # return empty range beginning at `first(rng)`
    else
        lst = indexof(i, a, reverse(rng))
        return fst:lst
    end
end

function countin{T}(x::T, a::AbstractVector{T}, rng::Ranges)
    count = 0
    for i in rng
        if x == a[i]
            count += 1
        end
    end
    count
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
            @myinbounds n = observationcounts[k,d]
            if n > 0
                @myinbounds dh[n,k] += 1
            end
        end
        @myinbounds dh[totals[d]] += 1
    end

    dh
end

#
# Optimize the concentration and base measure of an asymmetric Dirichlet prior
#
function dirichlet_estimate!(dh::DirichletHistogram, iters::Int, α::Vector{Float64})
    K = dh.K
    converged = false

    for i = 1:iters
        D = 0.
        S = 0.
        α0 = @checknan sum(α)
        maxstep = 0.

        for n = 1:dh.maxN
            D += @checknan 1 / (n - 1 + α0)
            @myinbounds S += @checknan dh[n]*D
        end

        for k = 1:K
            D = 0.
            Sk = 0.
            for n = 1:dh.maxNk
                @myinbounds D += @checknan 1 / (n - 1 + α[k])
                @myinbounds Sk += @checknan dh[n,k]*D
            end

            @myinbounds prev = α[k]
            @myinbounds α[k] *= @checknan Sk/S
            @myinbounds α[k] = max(α[k], eps()) #DFH

            @myinbounds step = α[k] - prev
            maxstep = max(maxstep, step)
        end

        converged = maxstep < 1e-9
        if converged
            break
        end
    end

    (α, converged)
end

#
# Optimize the concentration of a symmetric (uniform) Dirichlet prior
#
function dirichlet_estimate(dh::DirichletHistogram, iters::Int, β0::Float64)
    K = dh.K
    converged = false

    for i = 1:iters
        D = 0.
        S = 0.
        for n = 1:dh.maxN
            D += 1 / (n - 1 + β0)
            @myinbounds S += dh[n]*D
        end

        β = β0/K
        D = 0.
        Snum = 0.
        summed_counts = sum(dh.Nk, 2)
        for n = 1:dh.maxNk
            D += 1 / (n - 1 + β)
            @myinbounds Snum = summed_counts[n]*D
        end

        old = β0
        # MALLET has `β` on the RHS here rather than `β0`, but it doesn't work for me (goes to 0 pretty quickly)
        β0 = β0 * Snum / S

        converged = β0 - old < 1e-9

        if converged
            break
        end
    end

    (β0, converged)
end
