#!/bin/bash

TOOLS=`dirname $0`

if [ `uname` = "Darwin" ]; then
    BASE=$(cd "$TOOLS/.."; pwd -P)
else
    BASE=`readlink -f ${TOOLS}/..`
fi

EJD1=${BASE}/dev/mongooseim_node1
EJD2=${BASE}/dev/mongooseim_node2
EJD1CTL=${EJD1}/bin/mongooseim
EJD2CTL=${EJD2}/bin/mongooseim
