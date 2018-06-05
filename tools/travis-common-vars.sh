#!/bin/bash

TOOLS=`dirname $0`

if [ `uname` = "Darwin" ]; then
    BASE=$(cd "$TOOLS/.."; pwd -P)
    # Don't forget to install in with "brew install gnu-sed"
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
# DEFAULT_REMOTE_RO_VOLUMES is for docker-machine.
case "$(uname -s)" in
    Darwin*)    DEFAULT_DATA_ON_VOLUME=false DEFAULT_REMOTE_RO_VOLUMES=false;;
    *)          DEFAULT_DATA_ON_VOLUME=true DEFAULT_REMOTE_RO_VOLUMES=false
esac
DATA_ON_VOLUME=${DATA_ON_VOLUME:-$DEFAULT_DATA_ON_VOLUME}
REMOTE_RO_VOLUMES=${REMOTE_RO_VOLUMES:-$DEFAULT_REMOTE_RO_VOLUMES}

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

function init_docker
{
    if [ "$REMOTE_RO_VOLUMES" = 'true' ]; then
        export REMOTE_ROOT_DIR="$(mktempdir remote_mongooseim)"
        docker rm -f mongooseim-uploader || echo "Skip mongooseim-uploader removal"
        echo "Start remote uploader helper"
        docker run --name mongooseim-uploader -v "$REMOTE_ROOT_DIR":/data -it busybox true
    fi
}

echoerr() { echo "$@" 1>&2; }

function mount_ro_volume
{
    if [ "$REMOTE_RO_VOLUMES" = 'true' ]; then
        echoerr "Uploading $1 to $REMOTE_ROOT_DIR"
        docker cp "$1" mongooseim-uploader:/data/
        echo "-v $REMOTE_ROOT_DIR/$(basename $1):$2:ro"
    else
        echo "-v $1:$2:ro"
    fi
}
