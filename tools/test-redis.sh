#!/usr/bin/env bash

set -e

## redi.sh doesn't work on MacOS.
## use docker to run redis test:
##      docker run --rm -it -v ${PWD}/:/mim -w /mim \
##                 -e REDIS_HOST=host.docker.internal \
##                 ubuntu tools/test-redis.sh

REDIS_HOST="${REDIS_HOST:-localhost}"
REDIS_PORT="${REDIS_PORT:-6379}"
tools/wait-for-it.sh "${REDIS_HOST}:${REDIS_PORT}"

function download_redish()
{
  local filename="${1:-redi.sh}"
  wget https://raw.githubusercontent.com/crypt1d/redi.sh/master/redi.sh -O "$filename"
  chmod a+x "$filename"
}

## MB download redi.sh
[ -f tools/redi.sh ] || download_redish tools/redi.sh

date=$(date)
echo "date is $date"

set +e

echo "store date_var"
echo "$date" | tools/redi.sh -H "${REDIS_HOST}" -P "${REDIS_PORT}" -s date_var
echo "ret=$?"

echo "retrieve date_var"
tools/redi.sh -H "${REDIS_HOST}" -P "${REDIS_PORT}" -g date_var
echo "ret=$?"

tools/redi.sh -H "${REDIS_HOST}" -P "${REDIS_PORT}" -g date_var | grep -qF "$date"
