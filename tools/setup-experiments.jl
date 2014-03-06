#!/usr/bin/env julia

work_dir = ENV["VWORK"]
src_dir = ENV["VSRC"]
corpus_dir = ENV["CORPUS_ROOT"]
experiments_dir = ENV["EXPERIMENTS_ROOT"]
julia = ENV["JULIA"]

# corpora = ["02-main.dev"]
corpora = ["01-indomain", "03-main.test"]
# corpora = ["01-indomain", "02-main.dev", "03-main.test"]
verbs = ["root", "recursive"]
# verbs = ["all", "root", "recursive"]
topics = ["XX"]
# topics = [50, 100, 500, 1000, 2000]
models = ["lda", "verbonly", "verbargs", "verbchain"]

# make sure we are not overwriting anything
for c in corpora
    @assert !isdir(c)
    @assert !isfile(c)
end

function makedata(dir, corpus, verbs, model)
    pp = "$(work_dir)/preprocess/target/universal/stage/bin/preprocess" 
    jo = "JAVA_OPTS='-Xmx20G -Xms20G'"
    opts = "--corpus $(joinpath(corpus_dir, corpus)) --verbs $(verbs) --model $(model) --trimVocab 5"
    cmd = "$jo $pp $opts"

"""#!/bin/bash

cd "$dir"
$cmd
"""
end

function makerunner(dir, model, numtopics)
    dbg = "DEBUG=1 " # dbg = ""
    script = "$(src_dir)/$(model).jl"
    opts = "-K $(numtopics) -a 20 -b 0.001 -i 2000 --burnin 0 --priorinterval 5 -R 100"
    cmd = "$(dbg)$(julia) $script $opts 2>&1 | tee log >/dev/null"

"""#!/bin/bash

set -o pipefail
cd "$dir"
$cmd
"""
end

# dir structure: <corpus>/<verbs>/<model>/<params>/<run#>
for c in corpora
    mkpath(c)
    cd(c) do
        for v in verbs
            mkpath(v)
            cd(v) do
                for m in models
                    mkpath(m)
                    cd(m) do
                        open("makedata.sh", "w") do f
                            print(f, makedata(pwd(), c, v, m))
                        end
                        run(`chmod +x makedata.sh`)

                        ts = (c == "03-main.test" ? ["XX"] : map(string, topics))

                        for t in ts
                            mkpath(t)
                            cd(t) do
                                for r in [1, 2, 3]
                                    rp = "run$r"
                                    mkpath(rp)
                                    cd(rp) do
                                        open("run.sh", "w") do f
                                            print(f, makerunner(pwd(), m, t))
                                        end
                                        run(`chmod +x run.sh`)
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end
    end
end
