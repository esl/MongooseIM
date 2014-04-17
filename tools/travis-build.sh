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
SUMMARIES_DIRS=${BASE}'/test/ejabberd_tests/ct_report/ct_run*'


TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)

${TOOLS}/set-odbc-password vars ${TRAVIS_DB_PASSWORD}

make devclean devrel
echo -n "starting MongooseIM node 1: "
${EJD1CTL} start && echo ok || echo failed
echo -n "starting MongooseIM node 2: "
${EJD2CTL} start && echo ok || echo failed
sleep 1
echo -n "pinging MongooseIM node 1: "
${EJD1CTL} ping
echo -n "pinging MongooseIM node 2: "
${EJD2CTL} ping

make test_deps
${TOOLS}/set-odbc-password test ${TRAVIS_DB_PASSWORD}

make test_config TESTSPEC=default.spec CONFIG=$TEST_CONFIG

RAN_TESTS=`cat /tmp/ct_count`

echo -n "stopping MongooseIM node 1: "
${EJD1CTL} stop
echo -n "stopping MongooseIM node 2: "
${EJD2CTL} stop

if [ `uname` = "Darwin" ]; then
    SUMMARIES_DIR=`ls -dt ${SUMMARIES_DIRS} | head -n ${RAN_TESTS}`
else
    SUMMARIES_DIR=`eval ls -d ${SUMMARIES_DIRS} --sort time | head -n ${RAN_TESTS}`
fi

${TOOLS}/summarise-ct-results ${SUMMARIES_DIR}
CT_STATUS=$?

echo
echo "All tests done."
if [ ${CT_STATUS} == 0 ]
then
    echo "Build succeeded"
else
    echo "Build failed"
fi

exit ${CT_STATUS}