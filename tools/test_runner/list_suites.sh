#!/usr/bin/env bash
# Prints a list of SUITES one per line, withouts _SUITE prefix.
#
# Arguments:
# --prefix            -- add "small:" and "big:" prefixes to the output
# --skip-big-tests    -- do not return big suites
# --skip-small-tests  -- do not return small suites

TOOLS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

SMALL=true
BIG=true
SMALL_PREFIX=""
BIG_PREFIX=""

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    --skip-big-tests)
    BIG=false
    shift
    ;;
    --skip-small-tests)
    SMALL=false
    shift
    ;;
    --prefix)
    SMALL_PREFIX="small:"
    BIG_PREFIX="big:"
    shift
    ;;
    *)
        echo "Unknown argument $key"
        exit 1
esac
done

BIG_FILES=()
SMALL_FILES=()

if [ "$BIG" = "true" ]; then
    # Concat arrays
    BIG_FILES="$TOOLS_DIR"/../big_tests/tests/*_SUITE.erl
fi

if [ "$SMALL" = "true" ]; then
    # Concat arrays
    SMALL_FILES="$TOOLS_DIR"/../test/*_SUITE.erl
fi

for file in $SMALL_FILES
do
    echo "$SMALL_PREFIX"$(basename "$file" _SUITE.erl)
done

for file in $BIG_FILES
do
    echo "$BIG_PREFIX"$(basename "$file" _SUITE.erl)
done
