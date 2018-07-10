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
--tls-dist            -- enable encryption between nodes in big tests
--verbose             -- print script output

Test specifications:
-- [SUITE]
-- [SUITE:GROUP]
-- [SUITE:GROUP1:...:GROUPN]
-- [SUITE:GROUP:TESTCASE]
-- [SUITE:TESTCASE]

Commands:
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
./tools/run-all-tests.sh --db redis mysql -- rdbms mam
    Setups Redis and MySQL databases
    Runs mam_SUITE and rdbms_SUITE
    -- is used to separate test suites from databases

./tools/run-all-tests.sh muc:register
    Runs register group in muc_SUITE

./tools/run-all-tests.sh --db -- muc
    Skips database setup step and runs muc_SUITE
    Running databases would still be running

./tools/run-all-tests.sh --no-big-tests --no-cover ejabberd_config:smoke
    Runs smoke testcase in ejabberd_config_SUITE
    Disables big tests and cover

./tools/run-all-tests.sh --no-big-tests
    Travis build job 1

./tools/run-all-tests.sh --no-small-tests --db redis --tls-dist --preset internal_mnesia
    Travis build job 2

./tools/run-all-tests.sh --no-small-tests --db redis mysql --preset mysql_redis
    Travis build job 3

./tools/run-all-tests.sh --no-small-tests --db redis mssql --preset odbc_mssql_mnesia
    Travis build job 4

./tools/run-all-tests.sh --no-small-tests --db redis ldap --preset ldap_mnesia
    Travis build job 5

./tools/run-all-tests.sh --no-small-tests --db redis elasticsearch cassandra --preset elasticsearch_and_cassandra_mnesia -- mam mongoose_cassandra mongoose_elasticsearch
    Travis build job 6
    Separator -- between presets and suites

./tools/run-all-tests.sh --db redis pgsql --preset pgsql_mnesia
    Travis build job 8

./tools/run-all-tests.sh --db redis riak --preset riak_mnesia
    Travis build job 9

END
)

COMPLETE_EXAMPLES=$(cat <<-END
./tools/run-all-tests.sh TAB
    Show both small and big suites

./tools/run-all-tests.sh --no-big-tests TAB
    Show only small suites

./tools/run-all-tests.sh --no-small-tests TAB
    Show only big suites

./tools/run-all-tests.sh muc:TAB
    Complete argument with groups and test cases from muc_SUITE
    There should not be a whitespace after ":"

./tools/run-all-tests.sh --db TAB
    Complete with database names

./tools/run-all-tests.sh --db mysql TAB
    Complete with database names

./tools/run-all-tests.sh --db mysql -- TAB
    Separate db from arguments
    Complete with arguments

END
)

#

COMPLETE_ADVICE=$(cat <<-END
-----------------------------------
Bash tab completion is disabled!
Run the command to enable completion in Bash and zsh:
    source tools/run-all-tests-complete.sh
-----------------------------------

END
)

HELP_ADVICE=$(cat <<-END
-----------------------------------
Run with --help argument to show the script docs:
    ./tools/run-all-tests.sh --help
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
    # Make each line, that has "run-all-tests", bold
    # & is the whole match
    echo "$EXAMPLES" \
        | sed -e 's/.*run-all-tests.*/'"$bold&$normal"'/g'
}

function print_complete_examples
{
    local bold=$(tput bold)
    local normal=$(tput sgr0)
    # Make each line, that has "run-all-tests", bold
    # & is the whole match
    echo "$COMPLETE_EXAMPLES" \
        | sed -e 's/.*run-all-tests.*/'"$bold&$normal"'/g'
}

# Usage: print_help "advice text"
function print_advice
{
    local gray=$(tput setaf 7) # dim white text
    local normal=$(tput sgr0)
    echo "$gray$1$normal"
}

# Preset names are internal_mnesia, internal_redis ...
# Run the command to see the list:
# ./tools/run-all-tests.sh --list-presets
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
TLS_DIST=no

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
        print_help
        exit 0
    ;;
    --examples)
        print_examples
        exit 0
    ;;
    --examples-complete)
        print_complete_examples
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
echo ""

./tools/build-releases.sh

./tools/travis-build-tests.sh

./tools/travis-setup-db.sh

./tools/travis-test.sh
