#!/bin/bash

source tools/travis-common-vars.sh

echo ${BASE}

SUMMARIES_DIRS=${BASE}'/test/ejabberd_tests/ct_report/ct_run*'

echo "############################"
echo "Running embeded common tests"
echo "############################"

make ct
EMBEDED_CT_STATUS=$?

echo "############################"
echo "Running ejabberd_tests"
echo "############################"

echo -n "starting MongooseIM node 1: "
${EJD1CTL} start && echo ok || echo failed
echo -n "starting MongooseIM node 2: "
${EJD2CTL} start && echo ok || echo failed
sleep 1
echo -n "pinging MongooseIM node 1: "
${EJD1CTL} ping
echo -n "pinging MongooseIM node 2: "
${EJD2CTL} ping

make test_preset TESTSPEC=default.spec PRESET=$TEST_CONFIG

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
TEST_STATUSES="${EMBEDED_CT_STATUS}${CT_STATUS}"

if [ ${TEST_STATUSES} = "00" ]
then
    RESULT=0
    echo "Build succeeded"
else
    RESULT=1
    echo "Build failed - ${TEST_STATUSES}"
fi

exit ${RESULT}
