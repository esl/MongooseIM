#!/usr/bin/env bash
# Allowed env variables:
# - SMALL_TESTS
# - COVER_ENABLED
# - PRESET
# - DB
# - DEV_NODES
# - TEST_HOSTS
# - VERBOSE
# - STOP_NODES

# Fail on errors
set -e

USAGE=$(cat <<-END
This script runs small and big tests for MongooseIM

Options:
--db [DB]             -- a list of databases to setup for big tests
--preset [PRESET]     -- a list of presets to run during big tests
--dev-nodes [NODE]    -- a list of release nodes to build and start
--test-hosts [HOST]   -- a list of test hosts to apply preset to and collect cover info from
--one-node            -- the same as "--dev-nodes mim1 --test-hosts mim --"
--skip-preset         -- skip preset application, ignores --preset. Also known as quick mode
--skip-cover          -- disable coverage reports
--skip-small-tests    -- disable small tests
--skip-big-tests      -- disable big tests
--skip-build-tests    -- disable big test compilation
--skip-build-mim      -- disable MIM nodes compilation
--skip-start-nodes    -- do not start nodes before big tests
--skip-stop-nodes     -- do not stop nodes after big tests
--skip-setup-db       -- do not start any databases, the same as "--db --" option
--tls-dist            -- enable encryption between nodes in big tests
--verbose             -- print script output
--colors              -- force colors in help and examples commands
--pause [SECONDS]     -- pause before big_tests execution

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

./tools/test-runner.sh --db redis mysql --preset mysql_redis -- rdbms mam
    Setups Redis and MySQL databases
    Runs mam_SUITE and rdbms_SUITE
    -- is used to separate test suites from databases

./tools/test-runner.sh --preset internal_mnesia -- muc:register
    Runs register group in muc_SUITE

./tools/test-runner.sh --db --preset internal_mnesia -- muc
    Skips database setup step and runs muc_SUITE
    Running databases would still be running

./tools/test-runner.sh --skip-big-tests --skip-cover ejabberd_config:smoke
    Runs smoke testcase in ejabberd_config_SUITE
    Disables big tests and cover

./tools/test-runner.sh --skip-big-tests
    Travis build job with small tests

./tools/test-runner.sh --skip-small-tests --db redis --tls-dist --preset internal_mnesia
    Travis build job with internal_mnesia

./tools/test-runner.sh --skip-small-tests --db redis mysql --preset mysql_redis
    Travis build job with mysql_redis

./tools/test-runner.sh --skip-small-tests --db redis mssql --preset odbc_mssql_mnesia
    Travis build job with odbc_mssql_mnesia

./tools/test-runner.sh --skip-small-tests --db redis ldap --preset ldap_mnesia
    Travis build job with ldap_mnesia

./tools/test-runner.sh --skip-small-tests --db redis elasticsearch cassandra --preset elasticsearch_and_cassandra_mnesia -- mam mongoose_cassandra mongoose_elasticsearch
    Travis MAM-only build job with elasticsearch_and_cassandra_mnesia
    Separator -- between presets and suites

./tools/test-runner.sh --db redis pgsql --preset pgsql_mnesia
    Travis build job with pgsql_mnesia

./tools/test-runner.sh --db redis riak --preset riak_mnesia
    Travis build job with riak_mnesia

./tools/test-runner.sh --skip-small-tests --db mysql --preset mysql_mnesia --skip-stop-nodes -- mam
    Runs mam_SUITE with MySQL

./tools/test-runner.sh --skip-small-tests --skip-setup-db --dev-nodes --test-hosts --skip-cover --skip-preset -- mam
    Sets dev-nodes and test-hosts to empty lists
    Reruns mam_SUITE

./tools/test-runner.sh --rerun-big-tests -- mam
    The same command as above

./tools/test-runner.sh --help --examples --colors | more
    Display help using "more" command
END
)

COMPLETE_EXAMPLES=$(cat <<-END
Script completion examples:

./tools/test-runner.sh TAB
    Show both small and big suites

./tools/test-runner.sh --skip-big-tests TAB
    Show only small suites

./tools/test-runner.sh --skip-small-tests TAB
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

# We only use colors if they are supported by the terminal application
bold=""
normal=""
white=""

# if stdout is terminal
function init_colors
{
    # see if it supports colors...
    ncolors=$(tput colors)
    if test -n "$ncolors" && test $ncolors -ge 8; then
        bold="$(tput bold)"
        normal="$(tput sgr0)"
        white="$(tput setaf 7)" # dim white
    fi
}

if test -t 1; then
    init_colors
fi

function print_help
{
    # Print options using bold font
    echo "$USAGE" \
        | sed -e "s/^--/$bold--/g" | sed "s/ --/$normal--/g"
}

function print_examples
{
    # Make each line, that has "test-runner", bold
    # & is the whole match
    echo "$EXAMPLES" \
        | sed -e 's/.*test-runner.*/'"$bold&$normal"'/g'
}

function print_complete_examples
{
    # Make each line, that has "test-runner", bold
    # & is the whole match
    echo "$COMPLETE_EXAMPLES" \
        | sed -e 's/.*test-runner.*/'"$bold&$normal"'/g'
}

# Usage: print_help "advice text"
function print_advice
{
    echo "$white$1$normal"
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

DELAYED_ARRAY=()
function delay_exec
{
    local flat_args="$@"
    DELAYED_ARRAY+=("$flat_args")
}

function run_delayed
{
    for i in "${!DELAYED_ARRAY[@]}"; do ${DELAYED_ARRAY[$i]}; done
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
COVER_ENABLED_DEFAULT=true
PRESET_ENABLED_DEFAULT=true

PAUSE_BEFORE_BIG_TESTS=0
BIG_TESTS=true
BUILD_TESTS=true
BUILD_MIM=true
START_NODES=true
STOP_NODES=true
TLS_DIST=false

SELECTED_TESTS=()
STOP_SCRIPT=false
SKIP_DB_SETUP=false
DB_FROM_PRESETS=true

# Parse command line arguments
# Prefer arguments to env variables
while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    --pause)
        shift # past argument
        PAUSE_BEFORE_BIG_TESTS=15
        if [ "$1" -gt 0 ] 2>/dev/null; then
            PAUSE_BEFORE_BIG_TESTS="$1"
            shift
        fi
    ;;

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
        DB_FROM_PRESETS=false
    ;;

    --skip-setup-db)
        shift # past argument
        SKIP_DB_SETUP=true
        DB_FROM_PRESETS=false
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

    --one-node)
        shift # past argument
        unset TEST_HOSTS
        unset DEV_NODES
        TEST_HOSTS_ARRAY=( mim )
        DEV_NODES_ARRAY=( mim1 )
        ;;

    --skip-cover)
        shift # past argument
        COVER_ENABLED=false
    ;;

    --skip-preset)
        shift # past argument
        # Disable preset application
        PRESET_ENABLED=false
    ;;

    --skip-small-tests)
        shift # past argument
        SMALL_TESTS=false
    ;;

    --skip-big-tests)
        shift # past argument
        BIG_TESTS=false
    ;;

    --skip-build-tests)
        shift # past argument
        BUILD_TESTS=false
    ;;

    --skip-build-mim)
        shift # past argument
        BUILD_MIM=false
    ;;

    --skip-start-nodes)
        shift # past argument
        START_NODES=false
        BUILD_MIM=false
    ;;

    --skip-stop-nodes)
        shift # past argument
        STOP_NODES=false
    ;;

    --tls-dist)
        shift # past argument
        TLS_DIST=true
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
    --colors)
        init_colors
        shift # consume argument
    ;;
    --help)
        delay_exec print_command_delimeter
        delay_exec print_help
        STOP_SCRIPT=true
        shift # consume argument, continue execution
    ;;
    --examples)
        delay_exec print_command_delimeter
        delay_exec print_examples
        STOP_SCRIPT=true
        shift # consume argument, continue execution
    ;;
    --examples-complete)
        delay_exec print_command_delimeter
        delay_exec print_complete_examples
        STOP_SCRIPT=true
        shift # consume argument, continue execution
    ;;
    --show-small-reports)
        delay_exec ./tools/test_runner/show_reports.sh small
        STOP_SCRIPT=true
        shift # consume argument, continue execution
    ;;
    --show-big-reports)
        delay_exec ./tools/test_runner/show_reports.sh big
        STOP_SCRIPT=true
        shift # consume argument, continue execution
    ;;

    --rerun-big-tests)
        shift # consume argument, continue execution
        # Appends "--skip-small-tests ... --skip-preset" arguments to the current positional parameters
        set -- --skip-small-tests --skip-setup-db --dev-nodes --test-hosts --skip-cover --skip-preset "$@"
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

run_delayed


if [ "$STOP_SCRIPT" = true ]; then
    # Skipping test execution
    exit 0
fi


if [ "$SKIP_DB_SETUP" = true ]; then
    echo "--skip-setup-db always overrides --db argument"
    unset DB # We ignore env variable value
    DBS_ARRAY=() # No dbs
fi

print_advice "$HELP_ADVICE"

if [ -z "$RUN_ALL_TESTS_COMPLETE" ]; then
    print_advice "$COMPLETE_ADVICE"
fi

if [ "$BIG_TESTS" = false ]; then
    echo "Unset PRESET, DB, DEV_NODES because --skip-big-tests option has been passed"
    PRESET="small_tests"
    DB=""
    DEV_NODES=""
    BUILD_TESTS=false
    BUILD_MIM=false
fi

if [ "$DB_FROM_PRESETS" = true ]; then
    # Get a final preset list
    # Be aware, that actual PRESET var would be set below
    # We just need to know it NOW to call presets_to_dbs
    FINAL_PRESETS_DEFAULT="${PRESETS_ARRAY[@]}"
    FINAL_PRESET="${PRESET-$FINAL_PRESETS_DEFAULT}"

    echo "Choose databases based on presets $FINAL_PRESET"
    # We DON'T ignore DB variable, it's still a priority
    DBS_ARRAY=( $(./tools/test_runner/presets_to_dbs.sh $FINAL_PRESET ))
fi

# Use env variable or default
export SMALL_TESTS="${SMALL_TESTS:-$SMALL_TESTS_DEFAULT}"
export COVER_ENABLED="${COVER_ENABLED:-$COVER_ENABLED_DEFAULT}"
export PRESET_ENABLED="${PRESET_ENABLED:-$PRESET_ENABLED_DEFAULT}"

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
export BUILD_MIM="$BUILD_MIM"
export TLS_DIST="$TLS_DIST"
# Pass extra arguments from tools/test_runner/selected-tests-to-test-spec.sh
# to rebar3 in Makefile
if [[ -f "auto_small_tests.spec" ]]; then
    export REBAR_CT_EXTRA_ARGS=" --spec \"$(pwd)/auto_small_tests.spec\" "
else
    export REBAR_CT_EXTRA_ARGS=""
fi
export TESTSPEC="auto_big_tests.spec"
export START_NODES="$START_NODES"
export STOP_NODES="$STOP_NODES"
export PAUSE_BEFORE_BIG_TESTS="$PAUSE_BEFORE_BIG_TESTS"

# Debug printing
echo "Variables:"
echo "    PRESET=\"$PRESET\""
echo "    DB=\"$DB\""
echo "    DEV_NODES=\"$DEV_NODES\""
echo "    TEST_HOSTS=\"$TEST_HOSTS\""
echo "    SMALL_TESTS=$SMALL_TESTS"
echo "    COVER_ENABLED=$COVER_ENABLED"
echo "    PRESET_ENABLED=$PRESET_ENABLED"
echo "    BUILD_TESTS=$BUILD_TESTS"
echo "    BUILD_MIM=$BUILD_MIM"
echo "    REBAR_CT_EXTRA_ARGS=$REBAR_CT_EXTRA_ARGS"
echo "    TESTSPEC=$TESTSPEC"
echo "    TLS_DIST=$TLS_DIST"
echo "    START_NODES=$START_NODES"
echo "    STOP_NODES=$STOP_NODES"
echo ""

./tools/build-releases.sh

./tools/travis-build-tests.sh

./tools/test_runner/selected-tests-to-test-spec.sh "${SELECTED_TESTS[@]}"

./tools/travis-setup-db.sh

./tools/travis-test.sh
