#!/usr/bin/env bash

source tools/travis-common-vars.sh
NAME=$(db_name redis)
REDIS_PORT=${REDIS_PORT:-6379}
docker rm -v -f $NAME || echo "Skip removing the previous container"
docker run -d --name $NAME \
    -p $REDIS_PORT:6379 \
    --health-cmd='redis-cli -h "127.0.0.1" ping' \
    redis
