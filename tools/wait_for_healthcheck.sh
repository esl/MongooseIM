#!/usr/bin/env bash
# Waiting for a running container to become healthy.
# Waits if no container was found with reason "no such object".
# Waits if the container does not healthcheck configured.
# Fails if this script runs for too long with the reason "Killed by timeout".
#
# Usage example:
# ./tools/wait_for_healthcheck.sh mongooseim-mysql
# or
# ./tools/wait_for_healthcheck.sh "$CONTAINER"
#
# TIMEOUT argument documentation
# ------------------------------
#
# TIMEOUT is number of times we try to get healthcheck status.
# Because we wait one second between tries, it can be interpreted as
# number of seconds we wait.
#
# with 5 seconds timeout:
# TIMEOUT=5 ./tools/wait_for_healthcheck.sh "$CONTAINER"
#
# If call to docker daemon gets blocked for a long time
# (for example, if docker daemon is down),
# that TIMEOUT would not work as expected.
#
# If "docker inspect" takes some time to execute, than TIMEOUT does not work
# as expected.
#
# This command always fails:
# TIMEOUT=0 ./tools/wait_for_healthcheck.sh "$CONTAINER"
#
# This command would try to get healthcheck status once:
# TIMEOUT=1 ./tools/wait_for_healthcheck.sh "$CONTAINER"

set -e
source tools/common-vars.sh

if [ "$#" -ne 1 ]; then
    exit "Illegal number of parameters"
fi
CONTAINER="$1"

CMD=$($DOCKER inspect "$CONTAINER" --format '{{println (index .Config.Healthcheck.Test 1) }}')

# Default timeout is 1 minute
TIMEOUT="${TIMEOUT:-60}"

# Directly run the command
# We could run "docker inspect" to get the healthcheck status from Docker,
# but it often takes health-interval seconds to return Healthy (i.e. 30 seconds by default)
function run_health_check_command {
    $DOCKER exec "$CONTAINER" sh -c "$CMD"
}

for i in $(seq 0 ${TIMEOUT}); do
    if run_health_check_command ; then
        echo -e "\nWaiting is done after $i seconds"
        exit 0
    fi
    echo -n "."
    sleep 1
done
echo -e "\nKilled by timeout"
exit 1
