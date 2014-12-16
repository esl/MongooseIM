#!/bin/bash

source tools/travis-common-vars.sh

echo ${BASE}

fold_start() {
    echo -en "travis_fold:start:$1\\r"
    echo "$2"
}

fold_end() {
    echo -en "travis_fold:end:$1\\r"
}

SUMMARIES_DIRS=${BASE}'/test/ejabberd_tests/ct_report/ct_run*'

fold_start "embeded" "Running embeded common tests"
echo "############################"
echo "Running embeded common tests"
echo "############################"

make ct
EMBEDED_CT_STATUS=$?
fold_end "embeded"

echo "############################"
echo "Running ejabberd_tests"
echo "############################"

# Print logs to stdout
mkdir -p ${BASE}/dev/mongooseim_node1/log
mkdir -p ${BASE}/dev/mongooseim_node2/log

touch ${BASE}/dev/mongooseim_node1/log/ejabberd.log
touch ${BASE}/dev/mongooseim_node2/log/ejabberd.log
touch /tmp/ct_travis_hook.log

tail -f ${BASE}/dev/mongooseim_node1/log/ejabberd.log &
tail -f ${BASE}/dev/mongooseim_node2/log/ejabberd.log &
tail -f /tmp/ct_travis_hook.log &

# Actual run
fold_start "start_nodes" "Starting MongooseIM nodes"
echo -n "starting MongooseIM node 1: "
${EJD1CTL} start && echo ok || echo failed
echo -n "starting MongooseIM node 2: "
${EJD2CTL} start && echo ok || echo failed
sleep 1
echo -n "pinging MongooseIM node 1: "
${EJD1CTL} ping
echo -n "pinging MongooseIM node 2: "
${EJD2CTL} ping
fold_end "start_nodes"

make cover_test_preset TESTSPEC=default.spec PRESET=$TEST_CONFIG

RAN_TESTS=`cat /tmp/ct_count`

fold_start "stop_nodes" "Stopping MongooseIM nodes"
echo -n "stopping MongooseIM node 1: "
${EJD1CTL} stop
echo -n "stopping MongooseIM node 2: "
${EJD2CTL} stop
fold_end "stop_nodes"

fold_start "sum_ct" "Summarise Common Tests results"
if [ `uname` = "Darwin" ]; then
    SUMMARIES_DIR=`ls -dt ${SUMMARIES_DIRS} | head -n ${RAN_TESTS}`
else
    SUMMARIES_DIR=`eval ls -d ${SUMMARIES_DIRS} --sort time | head -n ${RAN_TESTS}`
fi

${TOOLS}/summarise-ct-results ${SUMMARIES_DIR}
CT_STATUS=$?
fold_end "sum_ct"

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
