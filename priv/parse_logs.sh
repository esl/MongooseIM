#!/usr/bin/env bash

## Args = [FileName, Username, Domain | FileList]

echo "$@" >$1

file=$1
user=$2
domain=$3

shift 3

pattern="${user}@${domain}"

filelist=()

for f in "$@"; do
filelist+=("${f}"*)
done

grep -iF "$pattern" "${filelist[@]}" > "$file" || true
