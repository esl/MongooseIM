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
# - STOP_NODES

# Just compile big tests example (not really):
# /tools/test-runner.sh --no-small-tests --db --preset --dev-nodes --test-hosts --no-cover


USAGE=$(cat <<-END
This script runs small and big tests for MongooseIM

Options:
--db [DB]             -- a list of databases to setup for big tests
--preset [PRESET]     -- a list of presets to run during big tests
--dev-nodes [NODE]    -- a list of release nodes to build and start
--test-hosts [HOST]   -- a list of test hosts to apply preset to
--no-cover            -- disable coverage reports
--no-services         -- DELETE ME
--no-small-tests      -- disable small tests
--no-big-tests        -- disable big tests
--no-build-tests      -- disable big test compilation
--no-stop-nodes       -- do not stop nodes after big tests
--tls-dist            -- enable encryption between nodes in big tests
--verbose             -- print script output

Test specifications:
-- [SUITE]
-- [SUITE:GROUP]
-- [SUITE:GROUP1:...:GROUPN]
-- [SUITE:GROUP:TESTCASE]
-- [SUITE:TESTCASE]

Commands:
--show-small-reports  -- show small test reports
--show-big-reports    -- show big test reports
--list-dbs            -- get a list of supported databases
--list-presets        -- get a list of supported presets
--list-dev-nodes      -- get a list of dev names
--list-test-hosts     -- get a list of test hosts
--help                -- show this information
--examples            -- print the command examples
--examples-complete   -- print the command autocompletion examples
END
)


EXAMPLES=$(cat <<-END
Script examples:

./tools/test-runner.sh --db redis mysql -- rdbms mam
    Setups Redis and MySQL databases
    Runs mam_SUITE and rdbms_SUITE
    -- is used to separate test suites from databases

./tools/test-runner.sh muc:register
    Runs register group in muc_SUITE

./tools/test-runner.sh --db -- muc
    Skips database setup step and runs muc_SUITE
    Running databases would still be running

./tools/test-runner.sh --no-big-tests --no-cover ejabberd_config:smoke
    Runs smoke testcase in ejabberd_config_SUITE
    Disables big tests and cover

./tools/test-runner.sh --no-big-tests
    Travis build job 1

./tools/test-runner.sh --no-small-tests --db redis --tls-dist --preset internal_mnesia
    Travis build job 2

./tools/test-runner.sh --no-small-tests --db redis mysql --preset mysql_redis
    Travis build job 3

./tools/test-runner.sh --no-small-tests --db redis mssql --preset odbc_mssql_mnesia
    Travis build job 4

./tools/test-runner.sh --no-small-tests --db redis ldap --preset ldap_mnesia
    Travis build job 5

./tools/test-runner.sh --no-small-tests --db redis elasticsearch cassandra --preset elasticsearch_and_cassandra_mnesia -- mam mongoose_cassandra mongoose_elasticsearch
    Travis build job 6
    Separator -- between presets and suites

./tools/test-runner.sh --db redis pgsql --preset pgsql_mnesia
    Travis build job 8

./tools/test-runner.sh --db redis riak --preset riak_mnesia
    Travis build job 9
END
)

COMPLETE_EXAMPLES=$(cat <<-END
Script completion examples:

./tools/test-runner.sh TAB
    Show both small and big suites

./tools/test-runner.sh --no-big-tests TAB
    Show only small suites

./tools/test-runner.sh --no-small-tests TAB
    Show only big suites

./tools/test-runner.sh muc:TAB
    Complete argument with groups and test cases from muc_SUITE
    There should not be a whitespace after ":"

./tools/test-runner.sh --db TAB
    Complete with database names

./tools/test-runner.sh --db mysql TAB
    Complete with database names

./tools/test-runner.sh --db mysql -- TAB
    Separate db from arguments
    Complete with arguments
END
)

#

COMPLETE_ADVICE=$(cat <<-END
-----------------------------------
Bash tab completion is disabled!
Run the command to enable completion in Bash and zsh:
    source tools/test-runner-complete.sh
-----------------------------------

END
)

HELP_ADVICE=$(cat <<-END
-----------------------------------
Run with --help argument to show the script docs:
    ./tools/test-runner.sh --help
    ./tools/test-runner.sh --help --examples --examples-complete
-----------------------------------
END
)


function print_help
{
    local bold=$(tput bold)
    local normal=$(tput sgr0)
    # Print options using bold font
    echo "$USAGE" \
        | sed -e "s/^--/$bold--/g" | sed "s/ --/$normal--/g"
}

function print_examples
{
    local bold=$(tput bold)
    local normal=$(tput sgr0)
    # Make each line, that has "test-runner", bold
    # & is the whole match
    echo "$EXAMPLES" \
        | sed -e 's/.*test-runner.*/'"$bold&$normal"'/g'
}

function print_complete_examples
{
    local bold=$(tput bold)
    local normal=$(tput sgr0)
    # Make each line, that has "test-runner", bold
    # & is the whole match
    echo "$COMPLETE_EXAMPLES" \
        | sed -e 's/.*test-runner.*/'"$bold&$normal"'/g'
}

# Usage: print_help "advice text"
function print_advice
{
    local gray=$(tput setaf 7) # dim white text
    local normal=$(tput sgr0)
    echo "$gray$1$normal"
}

# If we execute the script with several commands, we want to separate the command
# output
# I.e. in case of:
# ./tools/test-runner.sh --help --examples --examples-complete
# We want two separators. One between help and examples.
# And another between examples and completion examples.
# i.e.:
#
# HELP
# -----
# EXAMPLES
# -----
# COMPLETE EXAMPLES
DELIMETER_ADDED=false
function print_command_delimeter
{
    if [ "$DELIMETER_ADDED" = true ]; then
        echo -e "\n--------------------------------------------\n"
    fi
    DELIMETER_ADDED=true
}

# Preset names are internal_mnesia, internal_redis ...
# Run the command to see the list:
# ./tools/test-runner.sh --list-presets
PRESETS_ARRAY=(
    $( ./tools/test_runner/list_presets.sh )
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
STOP_NODES=true
TLS_DIST=no

SELECTED_TESTS=()
STOP_SCRIPT=false

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

    --no-stop-nodes)
        shift # past argument
        STOP_NODES=false
    ;;

    --tls-dist)
        shift # past argument
        TLS_DIST=yes
    ;;

    --verbose)
        export VERBOSE=1
        shift # past argument
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
    --help)
        print_command_delimeter
        print_help
        STOP_SCRIPT=true
        shift # consume argument, continue execution
    ;;
    --examples)
        print_command_delimeter
        print_examples
        STOP_SCRIPT=true
        shift # consume argument, continue execution
    ;;
    --examples-complete)
        print_command_delimeter
        print_complete_examples
        STOP_SCRIPT=true
        shift # consume argument, continue execution
    ;;
    --show-small-reports)
        ./tools/test_runner/show_reports.sh big
        STOP_SCRIPT=true
        shift # consume argument, continue execution
    ;;
    --show-big-reports)
        ./tools/test_runner/show_reports.sh big
        STOP_SCRIPT=true
        shift # consume argument, continue execution
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


if [ "$STOP_SCRIPT" = true ]; then
    # Skipping test execution
    exit 0
fi

print_advice "$HELP_ADVICE"

if [ -z "$RUN_ALL_TESTS_COMPLETE" ]; then
    print_advice "$COMPLETE_ADVICE"
fi

if [ "$BIG_TESTS" = false ]; then
    echo "Unset PRESET, DB, DEV_NODES because --no-big-tests option has been passed"
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
export TLS_DIST="$TLS_DIST"
# Pass extra arguments from tools/test_runner/selected-tests-to-test-spec.sh
# to rebar3 in Makefile
export REBAR_CT_EXTRA_ARGS=" --spec auto_small_tests.spec "
export TESTSPEC="auto_big_tests.spec"
export STOP_NODES="$STOP_NODES"

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
echo "    TLS_DIST=$TLS_DIST"
echo "    STOP_NODES=$STOP_NODES"
echo ""

./tools/build-releases.sh

./tools/travis-build-tests.sh

./tools/travis-setup-db.sh

./tools/travis-test.sh
