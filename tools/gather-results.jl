#!/usr/bin/env julia

using DataArrays
using DataFrames

const corpora = ["01-indomain", "02-main.dev", "03-main.test"]
const verbs = ["all", "root", "recursive"]
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
rundirs = [dirname(l) for l in logs]

function path2df(path)
    path = convert(ASCIIString, path)
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

    open(joinpath(path, "log")) do f
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

    open(joinpath(path, "model")) do f
        zerotopics = 0
        for line in eachline(f)
            ss = split(chomp(line))
            if ss[3] == "0"
                zerotopics += 1
            end
        end
        df[:zerotopics] = zerotopics
        df[:percentzerotopics] = fld(zerotopics*100, df[:topics])
    end

    return df
end

sortindex(df::DataFrame, row) = (
    findindex(df[:corpus][row], corpora), findindex(df[:verbs][row], verbs), findindex(df[:model][row], models), df[:topics][row], df[:run][row]
)

function doit(filename)
    results = vcat(map(path2df, rundirs))

    key = [:corpus, :verbs, :model, :topics]
    results = join(
        results,
        by(
            results,
            key,
            df -> DataFrame(
                average_perplexity=mean(df[:perplexity]),
                var_ppl=var(df[:perplexity]),
                dev_ppl=std(df[:perplexity]))
            ),
        on=key
        )

    idxs = sort([1:nrow(results)], by=row -> sortindex(results, row))
    for c in 1:ncol(results)
        results[c][:] = results[c][idxs]
    end

    results = results[[:corpus, :verbs, :model, :topics, :perplexity, :zerotopics, :average_perplexity, :var_ppl, :dev_ppl]]

    writetable(filename, results)
end
