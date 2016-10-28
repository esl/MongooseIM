#!/bin/bash

PRESET="internal_mnesia"
SMALL_TESTS="true"
COVER_ENABLED="true"

while getopts ":p::s::c:" opt; do
  case $opt in
    p)
      PRESET=$OPTARG
      ;;
    s)
      SMALL_TESTS=$OPTARG
      ;;
    c)
      COVER_ENABLED=$OPTARG
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      exit 1
      ;;
  esac
done

source tools/travis-common-vars.sh
source tools/travis-helpers.sh

if [ "$TRAVIS_SECURE_ENV_VARS" == 'true' ]; then
  CT_REPORTS=$(ct_reports_dir)

  echo "Test results will be uploaded to:"
  echo $(s3_url ${CT_REPORTS})
fi
# Print ct_progress_hook output
echo "" > /tmp/progress
tail -f /tmp/progress &

# Kill children on exit, but do not kill self on normal exit
trap "trap - SIGTERM && kill -- -$$ 2> /dev/null" SIGINT SIGTERM
trap "trap '' SIGTERM && kill -- -$$ 2> /dev/null" EXIT

echo ${BASE}

MIM1=${BASE}/_build/mim1/rel/mongooseim
MIM2=${BASE}/_build/mim2/rel/mongooseim
MIM3=${BASE}/_build/mim3/rel/mongooseim
FED1=${BASE}/_build/fed1/rel/mongooseim
MIM1CTL=${MIM1}/bin/mongooseimctl
MIM2CTL=${MIM2}/bin/mongooseimctl
MIM3CTL=${MIM3}/bin/mongooseimctl
FED1CTL=${FED1}/bin/mongooseimctl

NODES=(${MIM1CTL} ${MIM2CTL} ${MIM3CTL} ${FED1CTL})

start_node() {
	echo -n "${1} start: "
	${1} start && echo ok || echo failed
	${1} started
	${1} status
	echo
}

stop_node() {
	echo -n "${1} stop: "
	${1} stop
	${1} stopped
	echo
}

summaries_dir() {
	if [ `uname` = "Darwin" ]; then
		echo `ls -dt ${1} | head -n ${2}`
	else
		echo `eval ls -d ${1} --sort time | head -n ${2}`
	fi
}

run_small_tests() {
	echo "############################"
	echo "Running small tests (apps/ejabberd/tests)"
	echo "############################"
	echo "Add option -s false to skip embeded common tests"
	make ct
	SMALL_SUMMARIES_DIRS=${BASE}/_build/test/logs/ct_run*
	SMALL_SUMMARIES_DIR=$(summaries_dir ${SMALL_SUMMARIES_DIRS} 1)
	${TOOLS}/summarise-ct-results ${SMALL_SUMMARIES_DIR}
}

maybe_run_small_tests() {
	if [ "$SMALL_TESTS" = "true" ]; then
		run_small_tests
	else
		echo "############################"
		echo "Small tests skipped"
		echo "############################"
	fi
}

run_test_preset() {
	tools/print-dots.sh start
    cd ${BASE}/test.disabled/ejabberd_tests
	if [ "$COVER_ENABLED" = "true" ]; then
		make cover_test_preset TESTSPEC=default.spec PRESET=$PRESET
	else
		make test_preset TESTSPEC=default.spec PRESET=$PRESET
    fi
    cd -
	tools/print-dots.sh stop
}

run_tests() {
	maybe_run_small_tests
	SMALL_STATUS=$?
	echo "SMALL_STATUS=$SMALL_STATUS"
	echo ""
	echo "############################"
	echo "Running big tests (tests/ejabberd_tests)"
	echo "############################"

	for node in ${NODES[@]}; do
		start_node $node;
	done

	run_test_preset

	RAN_TESTS=`cat /tmp/ct_count`

	for node in ${NODES[@]}; do
		stop_node $node;
	done

	SUMMARIES_DIRS=${BASE}'/test.disabled/ejabberd_tests/ct_report/ct_run*'
	SUMMARIES_DIR=$(summaries_dir ${SUMMARIES_DIRS} ${RAN_TESTS})
	${TOOLS}/summarise-ct-results ${SUMMARIES_DIR}
	BIG_STATUS=$?

	echo
	echo "All tests done."

	if [ $SMALL_STATUS -eq 0 -a $BIG_STATUS -eq 0 ]
	then
		RESULT=0
		echo "Build succeeded"
	else
		RESULT=1
		echo "Build failed:"
		[ $SMALL_STATUS -ne 0 ] && echo "    small tests failed"
		[ $BIG_STATUS -ne 0 ]   && echo "    big tests failed"
	fi

	exit ${RESULT}
}

if [ $PRESET == "dialyzer_only" ]; then
	make dialyzer

else
	run_tests
fi

