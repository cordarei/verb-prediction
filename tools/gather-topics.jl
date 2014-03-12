#!/usr/bin/env julia

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

modelfiles = split(readchomp(`find $(pwd()) -name model`))
modelfiles = filter(mf->!contains(mf, "lda"), modelfiles)

type Key
    corpus::String
    verbs::String
    model::String
    numtopics::Int
    run::Int
end

type Topic
    key::Key
    topicid::Int
    α::Float64
    tokens::Int
    words::Vector{(String, Float64)}
end

function path2key(path)
    parts = split(path, "/")
    idx = findindex(x -> x in corpora, parts)
    @assert idx > 0
    Key(
        parts[idx],
        parts[idx + 1],
        parts[idx + 2],
        int(parts[idx + 3]),
        int(parts[idx + 4][4:end])
    )
end

function gettopics(modelpath)
    key = path2key(modelpath)
    topics = Array(Topic, 0)
    open(modelpath) do mf
        for line in eachline(mf)
            if line[1] != '#'
                ss = split(line)
                k = int(ss[1])
                α = float64(ss[2])
                n = int(ss[3])
                if n == 0
                    continue
                end
                ws = (String,Float64)[]
                open(joinpath(dirname(modelpath), "topics/topic-$k")) do tf
                    for l in eachline(tf)
                        ss = split(l)
                        push!(ws, (ss[1], float64(ss[2])))
                    end
                end
                push!(topics, Topic(key, k, α, n, ws))
            end
        end
    end
    return topics
end

key2string(key::Key) = "$(key.corpus):$(key.verbs):$(key.model):$(key.numtopics):$(key.run)"
function topicstring(topic::Topic)
    join([topic.key.corpus, topic.key.verbs, topic.key.model, string(topic.key.numtopics), string(topic.key.run), string(topic.topicid)], ":")
end

function doit()
    for path in modelfiles
        for topic in gettopics(convert(UTF8String, path))
            # str = @sprintf("%s\t%d\t%.3f\t%d", key2string(topic.key), topic.topicid, topic.α, topic.tokens)
            # str = @sprintf("%s\t%d\t%d", key2string(topic.key), topic.topicid, topic.tokens)
            ws = join([@sprintf("%s\t%f", tpl[1], tpl[2]) for tpl in topic.words], "\t")
            println(join([topicstring(topic), string(topic.tokens), ws], "\t"))
            # println("$str\t$ws")
        end
    end
end
