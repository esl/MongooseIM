#!/usr/bin/env bash
#
# Runs a command, forwards output into a file.
# Usage example:
# ./tools/silent_exec.sh "/tmp/log_file.log" ps -aux
#
# Env variables:
# - VERBOSE=0 (default) - hide command output, forward output into file
# - VERBOSE=1 - forward command output to console and into file
TOOLS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

OUT_FILE="$1"

OUT_DIRNAME=$(dirname "$OUT_FILE")
OUT_BASENAME=$(basename "$OUT_FILE")
OUT_ABSNAME=$("$TOOLS_DIR"/abs_dirpath.sh "$OUT_DIRNAME")
shift

# example: echo "long string" | truncate_string 80
function truncate_string
{
    awk -v len="$1" '{ if (length($0) > len) print substr($0, 1, len-3) "..."; else print; }'
}


echo "RUN: $@" | truncate_string 120
echo "LOG: $OUT_ABSNAME/$OUT_BASENAME"
echo ""

if [ "$VERBOSE" = "1" ]; then
    $@ 2>&1 | tee "$OUT_FILE" || exit 1
else
    $@ > "$OUT_FILE" 2>&1 || (cat "$OUT_FILE"; exit 1)
fi
