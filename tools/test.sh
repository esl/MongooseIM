#!/usr/bin/env bash
#
# Env variables:
# - SMALL_TESTS
# - EUNIT_TESTS
# - COVER_ENABLED
# - STOP_NODES (default false)
# - CT_HOOKS
set -o pipefail
shopt -s nullglob
IFS=$'\n\t'

DEFAULT_PRESET=internal_mnesia
PRESET="${PRESET-$DEFAULT_PRESET}"
SMALL_TESTS="${SMALL_TESTS:-true}"
EUNIT_TESTS="${EUNIT_TESTS:-false}"
COVER_ENABLED="${COVER_ENABLED:-true}"
REBAR_CT_EXTRA_ARGS="${REBAR_CT_EXTRA_ARGS:-}"

while getopts ":p:s:e:c:h:" opt; do
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
    e)
      EUNIT_TESTS=$OPTARG
      ;;
    h)
      CT_HOOKS=$OPTARG
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

source tools/common-vars.sh
source tools/helpers.sh

if [ "${AWS_SECRET_ACCESS_KEY}" ]; then
  CT_REPORTS=$(ct_reports_dir)

  echo "Test results will be uploaded to:"
  echo $(s3_url ${CT_REPORTS})
fi
# Print ct_progress_hook output
echo "" > /tmp/progress
tail -f /tmp/progress &
PRINT_PROGRESS_PID=$!
CURRENT_SCRIPT_PID=$$
./tools/kill_processes_on_exit.sh $CURRENT_SCRIPT_PID $PRINT_PROGRESS_PID &

echo ${BASE}

# Example: choose_newest_directory dir1 dir2 dir3
# Returns: a directory, that was modified last
choose_newest_directory() {
  if [ "$#" -eq 0 ]; then
      echo "No arguments passed"
      exit 1
  fi

  if [ `uname` = "Darwin" ]; then
    ls -dt "$@" | head -n 1
  else
    ls -d "$@" --sort time | head -n 1
  fi
}

circleci_tests_available() {
  command -v circleci >/dev/null 2>&1 && [[ "$CIRCLECI" == "true" ]]
}

select_circleci_suites() {
  local glob_pattern="$1"
  local output_file="$2"

  rm -f "$output_file"
  # Use xargs to normalize input to newlines, then sed to process
  # This follows CircleCI docs recommendation to use xargs for input handling
  if ! circleci tests glob "$glob_pattern" | \
      circleci tests run --command="xargs -n1 echo | sed 's|.*/||; s|\.erl$||' | sed '/^$/d' >>$output_file" --verbose
  then
    echo "circleci tests run failed for pattern $glob_pattern"
    return 1
  fi

  if [ ! -s "$output_file" ]; then
    echo "circleci tests run returned no suites for pattern $glob_pattern"
    return 1
  fi
}

maybe_select_small_test_suites() {
  if circleci_tests_available && select_circleci_suites "test/**/*_SUITE.erl" selected_small_suites; then
    if [ -s selected_small_suites ]; then
      # Convert newline-separated suites to comma-separated list for rebar3
      SELECTED_SUITES=$(cat selected_small_suites | tr '\n' ',' | sed 's/,$//')
      echo "Selected small test suites: $SELECTED_SUITES"
      export SUITE="$SELECTED_SUITES"
    fi
  else
    echo "CircleCI did not return specific small-test suites to rerun; executing default list"
  fi
}

run_small_tests() {
  tools/print-dots.sh start
  tools/print-dots.sh monitor $$
  if [ "$COVER_ENABLED" = true ]; then
    REBAR_CT_EXTRA_ARGS=" -c $REBAR_CT_EXTRA_ARGS "
  fi
  export REBAR_CT_EXTRA_ARGS="$REBAR_CT_EXTRA_ARGS"
  maybe_select_small_test_suites
  make ct
  RESULT="$?"
  tools/print-dots.sh stop
  return "$RESULT"
}

run_eunit_tests() {
  tools/print-dots.sh start
  tools/print-dots.sh monitor $$
  if [ "$COVER_ENABLED" = true ]; then
    export REBAR_EUNIT_EXTRA_ARGS="-c"
  fi
  make eunit
  RESULT="$?"
  tools/print-dots.sh stop
  ## print execution results w/o compilation log
  sed -n '/===> Performing EUnit tests.../,$p' "${BASE}/eunit.log"
  return "$RESULT"
}

maybe_run_small_tests() {
  if [ "$SMALL_TESTS" = "true" ]; then
    echo "############################"
    echo "Running small tests (test/)"
    echo "############################"
    run_small_tests
  else
    echo "############################"
    echo "Small tests skipped"
    echo "############################"
  fi
}

maybe_run_eunit_tests() {
  if [ "$EUNIT_TESTS" = "true" ]; then
    echo "############################"
    echo "Running eunit tests"
    echo "############################"
    run_eunit_tests
  else
    echo "############################"
    echo "Eunit tests skipped"
    echo "############################"
  fi
}

function is_member
{
  local e match="$1"
  shift
  for e; do [[ "$e" == "$match" ]] && return 0; done
  return 1
}

run_test_preset() {
  tools/print-dots.sh start
  tools/print-dots.sh monitor $$
  cd ${BASE}/big_tests
  local MAKE_RESULT=0
  TESTSPEC=${TESTSPEC:-default.spec}
  maybe_select_suites
  if [ "$COVER_ENABLED" = "true" ]; then
    make cover_test_preset TESTSPEC=$TESTSPEC PRESET=$PRESET CT_HOOKS=$CT_HOOKS
    MAKE_RESULT=$?
  else
    make test_preset TESTSPEC=$TESTSPEC PRESET=$PRESET CT_HOOKS=$CT_HOOKS
    MAKE_RESULT=$?
  fi
  cd -
  tools/print-dots.sh stop
  return ${MAKE_RESULT}
}

maybe_select_suites() {
  if circleci_tests_available && select_circleci_suites "tests/*_SUITE.erl" selected_suites; then
    if [ -s selected_suites ]; then
      escript ../tools/select_suites_to_run.erl $TESTSPEC $(<selected_suites)
    else
      echo "CircleCI returned an empty suite list for big tests; running defaults"
    fi
  else
    echo "CircleCI did not provide failed big-test suites; running defaults"
  fi
}

print_running_nodes() {
    echo "Running nodes:"
    # Expand wildcard into a bash array
    # if there's no mim1 build, fallback to epmd
    EPMDS=( "${BASE}"/_build/mim1/rel/mongooseim/erts-*/bin/epmd epmd )
    # Missing index expands into ${EPMDS[0]}
    "$EPMDS" -names
}

maybe_pause_before_test() {
  if [ "$PAUSE_BEFORE_BIG_TESTS" -gt 0 ] 2>/dev/null; then
    local read_ret_val
    tools/print-dots.sh start_countdown "$PAUSE_BEFORE_BIG_TESTS" "continue in" $$
    read -es -p $'press enter to pause before the big_tests\n' -t "$PAUSE_BEFORE_BIG_TESTS"
    read_ret_val="$?"
    tools/print-dots.sh stop
    [ "$read_ret_val" -ne 0 ] && { echo; return; }
    echo "[PAUSED]"
    read -es -p $'press enter to continue\n'
  fi
}

run_tests() {
  maybe_run_small_tests
  SMALL_STATUS=$?
  echo "SMALL_STATUS=$SMALL_STATUS"
  echo ""
  maybe_run_eunit_tests
  EUNIT_STATUS=$?
  echo "EUNIT_STATUS=$EUNIT_STATUS"
  echo ""
  echo "############################"
  echo "Running big tests (big_tests)"
  echo "############################"

  rm -f /tmp/ct_summary

  time ${TOOLS}/start-nodes.sh || { echo "Failed to start MongooseIM nodes"; return 1; }

  maybe_pause_before_test

  # Requires shopt -s nullglob
  SUMMARIES_DIRS_BEFORE=()
  for DIR in ${BASE}/big_tests/ct_report/ct_run*; do
    SUMMARIES_DIRS_BEFORE+=( "$DIR" )
  done

  run_test_preset
  BIG_STATUS=$?

  SUMMARIES_DIRS_AFTER=()
  for DIR in ${BASE}/big_tests/ct_report/ct_run*; do
    SUMMARIES_DIRS_AFTER+=( "$DIR" )
  done

  for DIR in ${SUMMARIES_DIRS_AFTER[@]}; do
    if ! is_member "$DIR" ${SUMMARIES_DIRS_BEFORE[@]}; then
        # when SUMMARIES_DIRS_BEFORE doesn't contain value DIR
        SUMMARIES_DIRS+=( "$DIR" )
    fi
  done

  echo "SUMMARIES_DIRS=$SUMMARIES_DIRS"
  big_tests/_build/default/lib/ct_groups_summary_hook/priv/summarise-ct-results ${SUMMARIES_DIRS}
  BIG_STATUS_BY_SUMMARY=$?

  echo
  echo "All tests done."

  local log_files=( $( compgen -G "${BASE}/_build/mim"*"/rel/mongooseim/log/mongooseim.log.1" ) )
  [ 0 -ne "${#log_files[@]}" ] && grep -F "fail_ci_build=true" "${log_files[@]}"
  # If phrase found than exit with code 1
  test $? -eq 1
  LOG_STATUS=$?

  if [ $SMALL_STATUS -eq 0 ] && [ $EUNIT_STATUS -eq 0 ] && [ $BIG_STATUS -eq 0 ] &&
     [ $BIG_STATUS_BY_SUMMARY -eq 0 ] && [ $LOG_STATUS -eq 0 ]; then
    RESULT=0
    echo "Build succeeded"
  else
    RESULT=1
    echo "Build failed:"
    [ $SMALL_STATUS -ne 0 ] && echo "    small tests failed"
    [ $EUNIT_STATUS -ne 0 ] && echo "    eunit tests failed"
    [ $BIG_STATUS_BY_SUMMARY -ne 0 ]   && echo "    big tests failed"
    [ $BIG_STATUS -ne 0 ]   && echo "    big tests failed - missing suites (error code: $BIG_STATUS)"
    [ $LOG_STATUS -ne 0 ]   && echo "    log contains errors"
    print_running_nodes
  fi

  if [ -f /tmp/ct_summary ]; then
      echo "Failed big cases:"
      cat /tmp/ct_summary
      echo ""
  fi

  # Do not stop nodes if big tests failed
  if [ "$STOP_NODES" = true ] && [ $BIG_STATUS -eq 0 ] && [ $BIG_STATUS_BY_SUMMARY -eq 0 ]; then
      echo "Stopping MongooseIM nodes"
      ./tools/stop-nodes.sh
  else
      echo "Keep MongooseIM nodes running"
  fi

  exit ${RESULT}
}

enable_tls_dist () {
  for node in ${DEV_NODES_ARRAY[@]}; do
    # Reenable commented out TLS dist options,
    # i.e. remove the single leading comment character on lines
    # commented out with just a single comment character.
    $SED -i -e 's/^#\([^#]\)/\1/' ${BASE}/_build/"$node"/rel/mongooseim/etc/vm.dist.args
  done
}

build_pkg () {
  set -e
  cd tools/pkg

  local platform=$1
  local erlang_version=$2
  local project_root=$(git rev-parse --show-toplevel)

  if [[ $platform == rockylinux* ]] || [[ $platform == almalinux* ]]; then
      local dockerfile_name="Dockerfile_rpm"
  elif [[ $platform == debian* ]] || [[ $platform == ubuntu* ]]; then
      local dockerfile_name="Dockerfile_deb"
  else
      echo "No dockerfile for given platform" && exit 1
  fi

  version=$(cat "${project_root}/VERSION")
  commit_sha=$(git rev-parse --short HEAD)
  # Do not add commit hash to package revision if package is built for tag
  if [[ "$(git describe --exact-match --tags HEAD 2>/dev/null)" == "$version" ]]; then
      revision="1"
  else
      revision="1.${commit_sha}"
  fi

  ./build.sh \
    --platform $platform \
    --version $version \
    --revision $revision \
    --erlang_version $erlang_version \
    --dockerfile_path "$project_root/tools/pkg/$dockerfile_name" \
    --context_path $project_root \
    --built_packages_directory "$project_root/tools/pkg/packages"
  set +e
}

if [ "$PRESET" == "dialyzer_only" ]; then
  tools/print-dots.sh start
  tools/print-dots.sh monitor $$
  ./rebar3 dialyzer
  RESULT=$?
  tools/print-dots.sh stop
  exit ${RESULT}
elif [ "$PRESET" == "xref_only" ]; then
  tools/print-dots.sh start
  tools/print-dots.sh monitor $$
  ./rebar3 xref
  RESULT=$?
  tools/print-dots.sh stop
  exit ${RESULT}
elif [ "$PRESET" == "edoc_only" ]; then
  tools/print-dots.sh start
  tools/print-dots.sh monitor $$
  ./rebar3 edoc
  RESULT=$?
  tools/print-dots.sh stop
  exit ${RESULT}
elif [ "$PRESET" == "pkg" ]; then
  build_pkg $pkg_PLATFORM $pkg_OTP_VERSION
elif [ "$PRESET" == "small_tests" ]; then
  time maybe_run_small_tests
  SMALL_RESULT=$?
  time maybe_run_eunit_tests
  EUNIT_RESULT=$?
  if [ "$SMALL_RESULT" -ne 0 ] || [ "$EUNIT_RESULT" -ne 0 ]; then
    exit 1
  fi
else
  [ x"$TLS_DIST" == xtrue ] && enable_tls_dist
  run_tests
fi
