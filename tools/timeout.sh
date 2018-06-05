#!/usr/bin/env bash

if hash timeout 2>/dev/null; then
    timeout "$@"
elif hash gtimeout 2>/dev/null; then
    gtimeout "$@"
else
    perl -e 'alarm shift; exec @ARGV' "$@";
fi
