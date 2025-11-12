#!/bin/bash

TOOLS=`dirname $0`

# make sure base32 is installed
if [ `uname` = "Darwin" ] && ! command -v base32 >/dev/null 2>&1; then
    echo "Please install base32:"
    echo "  brew install coreutils"
    exit 1
fi

if echo | base32 -w0 > /dev/null 2>&1; then
      # GNU coreutils base32, '-w' supported
      ENCODER="base32 -w0"
    else
      # Openssl base32, no wrapping by default
      ENCODER="base32"
fi

function cat32 {
    cat "$1" | $ENCODER
}

DOCKER=${DOCKER:-docker}

DOCKER_HEALTH=Health
if [[ "$DOCKER" == *"podman"* ]]; then
    # Overrides for podman
    DOCKER_HEALTH=Healthcheck
fi

if [ `uname` = "Darwin" ]; then
    BASE=$(cd "$TOOLS/.."; pwd -P)
    # make sure gsed is installed
    if ! command -v gsed >/dev/null 2>&1; then
        echo "Please install gsed:"
        echo "  brew install gnu-sed"
        exit 1
    fi
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

function db_name
{
    echo mongooseim-$1
}

function entrypoint
{
    INJECT_FILES=$(cat32 tools/inject-files.sh)
    echo 'eval ${INSTALL_DEPS_CMD:-echo} && echo '${INJECT_FILES}' | eval ${BASE32DEC:-base32 --decode} | bash'
}

PYTHON2_BASE32_DEC="python2 -c 'import base64; import sys; sys.stdout.write(base64.b32decode(sys.stdin.readline().strip()))'"
PYTHON3_BASE32_DEC="python3 -c 'import base64; import sys; sys.stdout.buffer.write(base64.b32decode(sys.stdin.readline().strip()))'"
