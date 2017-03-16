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
