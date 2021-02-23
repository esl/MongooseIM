#!/usr/bin/env bash

source tools/travis-common-vars.sh
NAME=$(db_name rmq)
RMQ_PORT=5672

docker rm -v -f $NAME || echo "Skip removing the previous container"
docker run -d \
       --name $NAME \
       -p $RMQ_PORT:$RMQ_PORT \
       rabbitmq:3.7

tools/wait_for_service.sh $NAME $RMQ_PORT
