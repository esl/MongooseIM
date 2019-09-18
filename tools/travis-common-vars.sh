#!/bin/bash

TOOLS=`dirname $0`

if [ `uname` = "Darwin" ]; then
    BASE=$(cd "$TOOLS/.."; pwd -P)
    # Don't forget to install gsed command using "brew install gnu-sed"
    SED=gsed
else
    BASE=`readlink -f ${TOOLS}/..`
    SED=sed
fi
# Make it full
TOOLS="$BASE/tools"

TLS_DIST=${TLS_DIST:-false}
START_NODES=${START_NODES:-true}

DEFAULT_DEV_NODES="mim1 mim2 mim3 fed1 reg1"
DEV_NODES="${DEV_NODES-$DEFAULT_DEV_NODES}"

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

# Example: mktempdir "PREFIX"
#
# MAC OS X and docker specific:
#   Docker for Mac limits where mounts can be.
#   Mounts can be in /tmp, /Users, /Volumes but not in /var/folders/cd/
#   Default behaviour of mktemp on Mac is to create a directory like
#   /var/folders/cd/qgvc26bj6hg1kgr41q96zydh0000gp/T/tmp.Sa9w8Xp3
function mktempdir
{
    mktemp -d "/tmp/$1.XXXXXXXXX"
}

function mount_ro_volume
{
    echo "-v $1:$2:ro"
}

function db_name
{
    echo mongooseim-$1
}
