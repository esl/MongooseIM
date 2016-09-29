#!/usr/bin/env bash

# Runs elvis on modified files (both staged and unstaged)
# you need a patched version of elvis, the one that accepts filenames
# on commandline (https://github.com/bartekgorny/elvis/tree/cmdline, not yet in master)

s=$(git status | grep "new file:\|modified:" | grep "erl$")
s=$(echo $s | sed -e 's/ //g' | cut -d ':' -f 2 | sort | uniq)

for f in $s; do
    echo "Elvis is rocking $s..."
    elvis rock $f
done
