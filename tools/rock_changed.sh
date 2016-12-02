#!/usr/bin/env bash

# Runs elvis on modified files (both staged and unstaged)
# TODO: could run on all files modified in a branch

s=$(git status | grep "new file:\|modified:" | sed -e 's/ //g' | cut -d ':' -f 2 \
| grep "erl$"| sort | uniq)

for f in $s; do
    echo "Elvis is rocking $s..."
    elvis rock $f
done
