#!/usr/bin/env bash
# Allowed env variables:
# - SMALL_TESTS
# - COVER_ENABLED
# - START_SERVICES
# - PRESET
# - DB
# - DEV_NODES
# - TEST_HOSTS
# - VERBOSE

# Just compile big tests example (not really):
# /tools/run-all-tests.sh --no-small-tests --db --preset --dev-nodes --test-hosts --no-cover

PRESETS_ARRAY=(
    internal_mnesia
    internal_redis
    pgsql_mnesia
    odbc_mssql_mnesia
    mysql_mnesia
    ldap_mnesia
    riak_mnesia
    cassandra_mnesia
    elasticsearch_mnesia
)

DBS_ARRAY=(
    mysql
    pgsql
    riak
    cassandra
    elasticsearch
    mssql
    redis
    ldap
)

DEV_NODES_ARRAY=(
    mim1
    mim2
    mim3
    fed1
    reg1
)

TEST_HOSTS_ARRAY=(
    mim
    mim2
    mim3
    fed
    reg
)

SMALL_TESTS_DEFAULT=true
START_SERVICES_DEFAULT=false # we don't use them anyway
COVER_ENABLED_DEFAULT=true

BIG_TESTS=true
BUILD_TESTS=true

SELECTED_TESTS=()

# Parse command line arguments
# Prefer arguments to env variables
while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    --db)
        shift # past argument
        unset DB # We ignore env variable value
        DBS_ARRAY=() # No dbs by default, if --db option present
        # Read a list of databases between --db and next option
        # (or the end of options)
        while [[ $# -gt 0 ]]
        do
            key="$1"
            if [[ "$key" != --* ]]; then
                echo "Database argument parsed $key"
                DBS_ARRAY+=("$key")
                shift # past value
            else
                echo "No more databases"
                break
            fi
        done
    ;;

    # Similar how we parse --db option
    --preset)
        shift # past argument
        unset PRESET
        PRESETS_ARRAY=()
        while [[ $# -gt 0 ]]
        do
            key="$1"
            if [[ "$key" != --* ]]; then
                echo "Preset argument parsed $key"
                PRESETS_ARRAY+=("$key")
                shift # past value
            else
                echo "No more presets"
                break
            fi
        done
    ;;

    --dev-nodes)
        shift # past argument
        unset DEV_NODES
        DEV_NODES_ARRAY=()
        while [[ $# -gt 0 ]]
        do
            key="$1"
            if [[ "$key" != --* ]]; then
                echo "Dev-name argument parsed $key"
                DEV_NODES_ARRAY+=("$key")
                shift # past value
            else
                echo "No more dev names"
                break
            fi
        done
    ;;

    --test-hosts)
        shift # past argument
        unset TEST_HOSTS
        TEST_HOSTS_ARRAY=()
        while [[ $# -gt 0 ]]
        do
            key="$1"
            if [[ "$key" != --* ]]; then
                echo "Test-host argument parsed $key"
                TEST_HOSTS_ARRAY+=("$key")
                shift # past value
            else
                echo "No more test hosts"
                break
            fi
        done
    ;;

    --no-cover)
        shift # past argument
        COVER_ENABLED=false
    ;;

    --no-services)
        shift # past argument
        START_SERVICES=false
    ;;

    --no-small-tests)
        shift # past argument
        SMALL_TESTS=false
    ;;

    --no-big-tests)
        shift # past argument
        BIG_TESTS=false
    ;;

    --no-build-tests)
        shift # past argument
        BUILD_TESTS=false
    ;;
    --verbose)
        export VERBOSE=1
        shift # skip placeholder
    ;;

    --list-dbs)
        ( IFS=$'\n'; echo "${DBS_ARRAY[*]}" )
        exit 0
    ;;
    --list-presets)
        ( IFS=$'\n'; echo "${PRESETS_ARRAY[*]}" )
        exit 0
    ;;
    --list-dev-nodes)
        ( IFS=$'\n'; echo "${DEV_NODES_ARRAY[*]}" )
        exit 0
    ;;
    --list-test-hosts)
        ( IFS=$'\n'; echo "${TEST_HOSTS_ARRAY[*]}" )
        exit 0
    ;;
    --)
        shift # skip placeholder
    ;;
    --*)
        echo "Unknown argument $key"
        exit 1
    ;;
    *)
        SELECTED_TESTS+=( "$key" )
        shift
esac
done
echo "No more arguments"

if [ -z "$RUN_ALL_TESTS_COMPLETE" ]; then
    echo "-----------------------------------"
    echo "Bash tab completion is disabled!"
    echo "Run the command to enable completion in Bash and zsh: "
    echo "    source tools/run-all-tests-complete.sh"
    echo "-----------------------------------"
    echo ""
fi

if [ "$BIG_TESTS" = false ]; then
    echo "Unset PRESET, DB, DEV_NODES because --no-big-tests option is passed"
    PRESET="small_tests"
    DB=""
    DEV_NODES=""
    BUILD_TESTS=false
fi

./tools/test_runner/selected-tests-to-test-spec.sh "${SELECTED_TESTS[@]}"

# Use env variable or default
export SMALL_TESTS="${SMALL_TESTS:-$SMALL_TESTS_DEFAULT}"
export START_SERVICES="${START_SERVICES:-$START_SERVICES_DEFAULT}"
export COVER_ENABLED="${COVER_ENABLED:-$COVER_ENABLED_DEFAULT}"

# Join array to string
PRESETS_DEFAULT="${PRESETS_ARRAY[@]}"
DBS_DEFAULT="${DBS_ARRAY[@]}"
DEV_NODES_DEFAULT="${DEV_NODES_ARRAY[@]}"
TEST_HOSTS_DEFAULT="${TEST_HOSTS_ARRAY[@]}"

./tools/configure with-all

# Allow user to pass PRESET and DB as an env variable
# (or use default value)
# "${parameter-word}" means:
# - if parameter set and not null - use parameter
# - if parameter set and null - use parameter (null)
# - if parameter is unset - use word
#
# Use empty DB (DB="") to skip database setup
# Use empty DEV_NODES (DEV_NODES="") to skip node compilation and restart
export PRESET="${PRESET-$PRESETS_DEFAULT}"
export DB="${DB-$DBS_DEFAULT}"
export DEV_NODES="${DEV_NODES-$DEV_NODES_DEFAULT}"
export TEST_HOSTS="${TEST_HOSTS-$TEST_HOSTS_DEFAULT}"
export BUILD_TESTS="$BUILD_TESTS"
# Pass extra arguments from tools/test_runner/selected-tests-to-test-spec.sh
# to rebar3 in Makefile
export REBAR_CT_EXTRA_ARGS=" --spec auto_small_tests.spec "
export TESTSPEC="auto_big_tests.spec"

# Debug printing
echo "Variables:"
echo "    PRESET=\"$PRESET\""
echo "    DB=\"$DB\""
echo "    DEV_NODES=\"$DEV_NODES\""
echo "    TEST_HOSTS=\"$TEST_HOSTS\""
echo "    SMALL_TESTS=$SMALL_TESTS"
echo "    START_SERVICES=$START_SERVICES"
echo "    COVER_ENABLED=$COVER_ENABLED"
echo "    BUILD_TESTS=$BUILD_TESTS"
echo "    REBAR_CT_EXTRA_ARGS=$REBAR_CT_EXTRA_ARGS"
echo "    TESTSPEC=$TESTSPEC"
echo ""

./tools/build-releases.sh

./tools/travis-build-tests.sh

./tools/travis-setup-db.sh

./tools/travis-test.sh
