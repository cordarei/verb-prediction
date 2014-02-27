#!/usr/bin/env julia

trainfiles = open(ARGS[1]) do f map(chomp, readlines(f)) end
testfiles = open(ARGS[2]) do f map(chomp, readlines(f)) end

run(`mkdir -p train`)
run(`mkdir -p test`)

for file in trainfiles
    run(`ln -s $file train/$(basename(file))`)
end

for file in testfiles
    run(`ln -s $file test/$(basename(file))`)
end
