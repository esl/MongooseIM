#!/bin/bash

TOOLS=`dirname $0`

source tools/travis-common-vars.sh

CQLDIR=${BASE}/apps/ejabberd/priv
cqlsh localhost 9160 -f ${CQLDIR}/cassandra.cql
