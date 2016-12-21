#!/usr/bin/env bash

# Runs elvis on modified files (both staged and unstaged)
# or on all files modified within a branch

if [ "$1" ]; then
    # branch is given, assemble a list of changed files
    s=$(git log master..$1 --format=format: --name-only | grep "erl$" | sort | uniq)
else
    # no branch - see what has changed
    s=$(git status | grep "new file:\|modified:" | sed -e 's/ //g' | cut -d ':' -f 2 \
        | grep "erl$"| sort | uniq)
fi

for f in $s; do
    echo
    echo "* * * * * Elvis is rocking $f..."
    echo
    elvis rock $f
done
