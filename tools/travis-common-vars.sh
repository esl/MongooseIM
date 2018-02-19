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
