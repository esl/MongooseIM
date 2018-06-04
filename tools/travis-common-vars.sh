#!/bin/bash

TOOLS=`dirname $0`

if [ `uname` = "Darwin" ]; then
    BASE=$(cd "$TOOLS/.."; pwd -P)
    SED=gsed
else
    BASE=`readlink -f ${TOOLS}/..`
    SED=sed
fi

TLS_DIST=${TLS_DIST:-no}

DEFAULT_DEV_NODES="mim1 mim2 mim3 fed1 reg1"
DEV_NODES="${DEV_NODES:-$DEFAULT_DEV_NODES}"

# Create a bash array DEFAULT_DEV_NODES with node names
IFS=' ' read -r -a DEV_NODES_ARRAY <<< "$DEV_NODES"

# Linux volumes are faster than layer fs.
# Mac volumes are actually slower than layer fs.
case "$(uname -s)" in
    Darwin*)    DEFAULT_DATA_ON_VOLUME=false;;
    *)          DEFAULT_DATA_ON_VOLUME=true
esac
DATA_ON_VOLUME=${DATA_ON_VOLUME:-$DEFAULT_DATA_ON_VOLUME}

# Returns its arguments if data on volume is enabled
function data_on_volume
{
    if [ "$DATA_ON_VOLUME" = 'true' ]; then
        echo "$@"
    fi
}

# Example: mktempdir "SUFFIX_OR_PREFIX"
function mktempdir
{
    case "$(uname -s)" in
        Darwin*)    mktemp -d -t "$1" ;; # On mac -t is prefix
        Linux*)     mktemp -d --suffix="$1" ;; # gnu mktemp
        *)          mktemp -d
    esac
}
