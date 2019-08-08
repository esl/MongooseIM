#!/usr/bin/env bash
#
# Runs a command, forwards output into a file.
# Usage example:
# ./tools/silent_exec.sh "/tmp/log_file.log" ps -aux
#
# Env variables:
# - VERBOSE=0 (default) - hide command output, forward output into file
# - VERBOSE=1 - forward command output to console and into file
set -e

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
    # Returns the same error as $@
    echo "" > "$OUT_FILE"
    tail -f "$OUT_FILE" &
    tail_pid=$!
    exit_code=0
    $@ 2>&1 > "$OUT_FILE" || exit_code=$?
    kill $tail_pid
    exit $exit_code
else
    $@ > "$OUT_FILE" 2>&1 || (cat "$OUT_FILE"; exit 1)
fi
