#!/usr/bin/env bash
# Waiting for a running container to become healthy.
# Waits if no container was found with reason "no such object".
# Waits if the container does not healthcheck configured.
# Fails if this script runs for too long with the reason "Killed by timeout".
#
# Usage example:
# ./tools/wait_for_healthcheck.sh mongooseim-riak
# or
# ./tools/wait_for_healthcheck.sh "$CONTAINER"
#
# with 5 seconds timeout:
# TIMEOUT=5 ./tools/wait_for_healthcheck.sh "$CONTAINER"

set -e

if [ "$#" -ne 1 ]; then
    exit "Illegal number of parameters"
fi
CONTAINER="$1"

# Default timeout is 1 minute
TIMEOUT="${TIMEOUT:-60}"

# Stop waiting after timeout using a background task
MAIN_PID=$BASHPID
(sleep "$TIMEOUT"; echo ""; echo "Killed by timeout"; kill $MAIN_PID) &
# Get pid of a background task
KILLER_PID=$!
# Kill the process on exit
trap "kill $KILLER_PID" EXIT

# Gets a health check of a container
# Usage example:
# health_status "$CONTAINER"
function health_status
{
    docker inspect --format '{{json .State.Health.Status }}' "$1"
}

while [ $(health_status "$CONTAINER")"" != "\"healthy\"" ]
do
    printf "."
    sleep 1
done
echo ""
echo "Waiting is done"
