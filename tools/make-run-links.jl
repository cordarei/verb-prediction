#!/usr/bin/env julia

runscripts = split(readchomp(`find $(pwd()) -name run.sh`))
for rs in runscripts
    cd(dirname(rs)) do
        for f in ["train", "test", "vocab", "verb-vocab", "arg-vocab"]
            if isfile("../../$f")
                run(`ln -s ../../$f $f`)
            end
        end
    end
end
