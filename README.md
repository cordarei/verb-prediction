# Verb Prediction Experiment

This is code for an experiment meant to demonstrate:

1. clustering verbs using topic models for the purpose of learning scripts or frames
2. evaluating the learned topic models by perplexity counting only verbs

It was supposed to show that adding argument and coreference
information increases the ability of a model to predict the verbs in a
test correctly, and that perplexity is a useful measure of this
ability. Unfortunately it wasn't successful. The model with verb
arguments only beat the baseline sometimes, and the coreference model
completely failed. This is largely because the models are very
unsophisticated, but I had predicted that even an unsophisticated
model should be able to beat the baseline verb-only LDA model.

Since I've lost interest in continuing this line of research, I am
making the code available as-is, on the off-chance that someone may
find it useful.

## preprocess/

This directory contains a program for preprocessing the Annotated
English Gigaword corpus, written in Scala. It uses the Java API
for reading in documents from the corpus, and processes the
documents using POS tags and dependencies to extract verbs and
create data for training the topic models.

## src/

This directory contains the Julia code for training and evaluating
four LDA-based topic models. `lda.jl` and `verbonly.jl` implement LDA
using Gibbs sampling; `verbargs.jl` and `verbchain.jl` implement
variants of the LDA model including verb arguments and verb-argument
coreference relations, respectively.

## tools/

This directory contains some scripts written in Julia for setting up
experiments and formatting results in a table. These are good examples
of how useful Julia is for system scripting; it's much nicer than
bash.


## Permissions

All files written by Joseph Irwin.

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)

To the extent possible under law, the author(s) have dedicated all copyright
and related and neighboring rights to this software to the public domain
worldwide. This software is distributed without any warranty.

You should have received a copy of the CC0 Public Domain Dedication along with
this software. If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.
