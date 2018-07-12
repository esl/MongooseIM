#!/usr/bin/env sh
docker rm -f mongooseim-redis || echo "Skip removing the previous container"
docker run -d --name mongooseim-redis \
    -p 6379:6379 \
    --health-cmd='redis-cli -h "127.0.0.1" ping' \
    redis
