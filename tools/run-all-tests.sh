#!/usr/bin/env bash
# Allowed env variables:
# - SMALL_TESTS
# - COVER_ENABLED
# - START_SERVICES
# - PRESET
# - DB
# - DEV_NAMES
# - TEST_HOSTS
# - VERBOSE

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

DEV_NAMES_ARRAY=(
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

    --dev-names)
        shift # past argument
        unset DEV_NAMES
        DEV_NAMES_ARRAY=()
        while [[ $# -gt 0 ]]
        do
            key="$1"
            if [[ "$key" != --* ]]; then
                echo "Dev-name argument parsed $key"
                DEV_NAMES_ARRAY+=("$key")
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

    --list-dbs)
        ( IFS=$'\n'; echo "${DBS_ARRAY[*]}" )
        exit 0
    ;;
    --list-presets)
        ( IFS=$'\n'; echo "${PRESETS_ARRAY[*]}" )
        exit 0
    ;;
    --list-dev-names)
        ( IFS=$'\n'; echo "${DEV_NAMES_ARRAY[*]}" )
        exit 0
    ;;
    --list-test-hosts)
        ( IFS=$'\n'; echo "${TEST_HOSTS_ARRAY[*]}" )
        exit 0
    ;;
    *)
        echo "Unknown argument $key"
        exit 1
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


# Use env variable or default
export SMALL_TESTS="${SMALL_TESTS:-$SMALL_TESTS_DEFAULT}"
export START_SERVICES="${START_SERVICES:-$START_SERVICES_DEFAULT}"
export COVER_ENABLED="${COVER_ENABLED:-$COVER_ENABLED_DEFAULT}"

# Join array to string
PRESETS_DEFAULT="${PRESETS_ARRAY[@]}"
DBS_DEFAULT="${DBS_ARRAY[@]}"
DEV_NAMES_DEFAULT="${DEV_NAMES_ARRAY[@]}"
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
# Use empty DEV_NAMES (DEV_NAMES="") to skip node compilation and restart
export PRESET="${PRESET-$PRESETS_DEFAULT}"
export DB="${DB-$DBS_DEFAULT}"
export DEV_NAMES="${DEV_NAMES-$DEV_NAMES_DEFAULT}"
export TEST_HOSTS="${TEST_HOSTS-$TEST_HOSTS_DEFAULT}"

# Debug printing
echo "Variables:"
echo "    PRESET=\"$PRESET\""
echo "    DB=\"$DB\""
echo "    DEV_NAMES=\"$DEV_NAMES\""
echo "    TEST_HOSTS=\"$TEST_HOSTS\""
echo "    SMALL_TESTS=$SMALL_TESTS"
echo "    START_SERVICES=$START_SERVICES"
echo "    COVER_ENABLED=$COVER_ENABLED"
echo ""

# "make devrel", but for a list of dev nodes
if [ -z "$DEV_NAMES" ]; then
    echo "Skip make devrel"
else
    make $DEV_NAMES
fi

./tools/travis-build-tests.sh

./tools/travis-setup-db.sh

./tools/travis-test.sh
