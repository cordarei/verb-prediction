#!/usr/bin/env julia

using DataArrays
using DataFrames

const corpora = ["01-indomain", "02-main.dev", "03-main.test"]
const verbs = ["all", "recursive", "root"]
const models = ["lda", "verbonly", "verbargs", "verbchain"]

function findindex(f::Function, coll)
    for i = 1:length(coll)
        if f(coll[i])
            return i
        end
    end
    return 0
end
findindex(x, coll) = findindex(y -> y == x, coll)

logs = split(readchomp(`find $(pwd()) -name log`))
logs = filter(x -> success(`grep -q perplexity $x`), logs)

type Params
    corpus::String
    verbs::String
    model::String
    topics::Int
end
type Stats
    D::Int
    V::Int
    K::Int
end
type Results
    perplexity::Float64
    normPerplexity::Float64
end
sortindex(p::Params) = (findindex(p.corpus, corpora), findindex(p.verbs, verbs), findindex(p.model,models), p.topics)
Base.isless(p1::Params, p2::Params) = sortindex(p1) < sortindex(p2)

function path2keys(path)
    parts = split(path, "/")
    idx = findindex(x -> x in corpora, parts)
    @assert idx > 0
    Dict([
        ("corpus", parts[idx]),
        ("verbs", parts[idx+1]),
        ("model", parts[idx+2]),
        ("topics", parts[idx+3]),
        ("run", parts[idx+4])
    ])
end

function path2df(path)
    parts = split(path, "/")
    idx = findindex(x -> x in corpora, parts)
    @assert idx > 0
    df = DataFrame()
    df[:corpus] = parts[idx]
    df[:verbs] = parts[idx + 1]
    df[:model] = parts[idx + 2]
    df[:topics] = int(parts[idx + 3])
    df[:run] = int(parts[idx + 4][4:end])
    df[:perplexity] = NA
    df[:normalized_perplexity] = NA
    open(convert(ASCIIString, path)) do f
        for line in eachline(f)
            line = chomp(line)
            ss = split(line, " = ")
            if ss[1] == "perplexity"
                df[:perplexity] = float(ss[2])
            elseif ss[1] == "normalized perplexity"
                df[:normalized_perplexity] = float(ss[2])
            end
        end
    end
    return df
end

function readresults(path)
    d = Dict{ASCIIString, Any}()
    open(convert(ASCIIString, path)) do f
        for line in eachline(f)
            line = chomp(line)
            ss = split(line, " = ")
            if ss[1] in ["K", "V", "D"]
                push!(d, ss[1], int(ss[2]))
            elseif contains(ss[1], "perplexity")
                push!(d, ss[1], float(ss[2]))
            end
        end
    end
    d
end

function path2results(path)
    keys = path2keys(path)
    all = merge(readresults(path), keys)
    return (
        Params(all["corpus"], all["verbs"], all["model"], all["K"]),
        Stats(all["D"], all["V"], all["K"]),
        Results(all["perplexity"], all["normalized perplexity"])
        )
end

function param2vec(p::Params)
    [p.corpus, p.verbs, p.model, string(p.topics)]
end
function formatppl(ppl::Float64)
    @sprintf("%.3f",ppl)
end

function maketablerow(r, normalized=false)
    hcat(transpose(param2vec(r[1])), [ formatppl(normalized ? r[3].normPerplexity : r[3].perplexity) ])
end

function maketablemat(results, normalized=false)
    sort!(results, by=x -> x[1])
    out = maketablerow(results[1], normalized)
    for i = 2:endof(results)
        out = vcat(out,maketablerow(results[i], normalized))
    end
    out
end

function doit(filename)
    # results = map(path2results, logs)
    # println("# Perplexity:")
    # print(maketablemat(results))
    # println()
    # println("# Normalized:")
    # print(maketablemat(results, true))
    results = vcat(map(path2df, logs))
    writetable(filename, results)
end
