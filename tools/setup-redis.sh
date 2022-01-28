#!/usr/bin/env bash

source tools/common-vars.sh
source tools/db-versions.sh
NAME=$(db_name redis)
REDIS_PORT=${REDIS_PORT:-6379}

$DOCKER rm -v -f $NAME || echo "Skip removing the previous container"
$DOCKER run -d --name $NAME \
    -p $REDIS_PORT:6379 \
    --health-cmd='redis-cli -h "127.0.0.1" ping' \
    redis:$REDIS_VERSION

tools/wait_for_healthcheck.sh $NAME
tools/wait_for_service.sh $NAME $REDIS_PORT
