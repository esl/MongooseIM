#!/usr/bin/env bash

source tools/common-vars.sh
source tools/db-versions.sh
NAME=$(db_name rmq)
RMQ_PORT=5672
RMQ_HTTP_PORT=15672

$DOCKER rm -v -f $NAME || echo "Skip removing the previous container"
$DOCKER run -d \
       --name $NAME \
       -p $RMQ_PORT:5672 \
       -p $RMQ_HTTP_PORT:15672 \
       rabbitmq:$RMQ_VERSION

tools/wait_for_service.sh $NAME $RMQ_PORT
