#!/usr/bin/env sh

NAME=mongooseim-redis
docker rm -f $NAME || echo "Skip removing the previous container"
docker run -d --name $NAME \
    -p 6379:6379 \
    --health-cmd='redis-cli -h "127.0.0.1" ping' \
    redis
