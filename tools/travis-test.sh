#!/usr/bin/env bash
set -uo pipefail
IFS=$'\n\t'

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

CAN_PRINT_S3_URL=${TRAVIS_SECURE_ENV_VARS:-false}

if [ "$CAN_PRINT_S3_URL" == 'true' ]; then
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

summaries_dir() {
  if [ `uname` = "Darwin" ]; then
    echo `ls -dt ${1} | head -n 1`
  else
    echo `eval ls -d ${1} --sort time | head -n 1`
  fi
}

run_small_tests() {
  echo "############################"
  echo "Running small tests (test/)"
  echo "############################"
  echo "Advice: "
  echo "    Add option \"-s false\" to skip embeded common tests"
  echo "Example: "
  echo "    ./tools/travis-test.sh -s false"
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

start_services() {
    for env in ${BASE}/test.disabled/ejabberd_tests/services/*-compose.yml; do
        echo "Stating service" $(basename "${env}") "..."
        time ${BASE}/tools/docker-compose.sh -f "${env}" up -d
        echo "docker-compose execution time reported above"
        echo ""
    done
}

run_test_preset() {
  tools/print-dots.sh start
  cd ${BASE}/test.disabled/ejabberd_tests
  local MAKE_RESULT=0
  if [ "$COVER_ENABLED" = "true" ]; then
    make cover_test_preset TESTSPEC=default.spec PRESET=$PRESET
    MAKE_RESULT=$?
  else
    make test_preset TESTSPEC=default.spec PRESET=$PRESET
    MAKE_RESULT=$?
  fi
  cd -
  tools/print-dots.sh stop
  return ${MAKE_RESULT}
}

print_running_nodes() {
    echo "Running nodes:"
    # Expand wildcard into a bash array
    EPMDS=( "${BASE}"/_build/mim1/rel/mongooseim/erts-*/bin/epmd )
    # Missing index expands into ${EPMDS[0]}
    "$EPMDS" -names
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

  # Start all additional services
  start_services

  run_test_preset
  BIG_STATUS=$?

  SUMMARIES_DIRS=${BASE}'/test.disabled/ejabberd_tests/ct_report/ct_run*'
  SUMMARIES_DIR=$(summaries_dir ${SUMMARIES_DIRS})
  ${TOOLS}/summarise-ct-results ${SUMMARIES_DIR}
  BIG_STATUS_BY_SUMMARY=$?

  echo
  echo "All tests done."

  grep "fail_ci_build=true" ${BASE}/_build/mim*/rel/mongooseim/log/ejabberd.log
  # If phrase found than exit with code 1
  test $? -eq 1
  LOG_STATUS=$?

  if [ $SMALL_STATUS -eq 0 -a $BIG_STATUS -eq 0 -a $BIG_STATUS_BY_SUMMARY -eq 0 -a $LOG_STATUS -eq 0 ]
  then
    RESULT=0
    echo "Build succeeded"
  else
    RESULT=1
    echo "Build failed:"
    [ $SMALL_STATUS -ne 0 ] && echo "    small tests failed"
    [ $BIG_STATUS_BY_SUMMARY -ne 0 ]   && echo "    big tests failed"
    [ $BIG_STATUS -ne 0 ]   && echo "    big tests failed - missing suites"
    [ $LOG_STATUS -ne 0 ]   && echo "    log contains errors"
    print_running_nodes
  fi

  exit ${RESULT}
}

enable_tls_dist () {
  for node in "$MIM1" "$MIM2" "$MIM3" "$FED1"; do
    # Reenable commented out TLS dist options,
    # i.e. remove the single leading comment character on lines
    # commented out with just a single comment character.
    $SED -i -e 's/^#\([^#]\)/\1/' "$node"/etc/vm.dist.args
  done
}

if [ $PRESET == "dialyzer_only" ]; then
  tools/print-dots.sh start
  ./rebar3 dialyzer
  RESULT=$?
  tools/print-dots.sh stop
  exit ${RESULT}
else
  [ x"$TLS_DIST" == xyes ] && enable_tls_dist
  run_tests
fi

