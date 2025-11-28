#!/usr/bin/env bash

set +euo pipefail

source tools/common-vars.sh
source tools/db-versions.sh
ENTRYPOINT=$(entrypoint)

NAME=$(db_name rmq)
RMQ_PORT=5672
RMQ_TLS_PORT=5671
RMQ_HTTP_PORT=15672

$DOCKER rm -v -f $NAME || echo "Skip removing the previous container"

MIM_CERT=$(cat32 tools/ssl/mongooseim/cert.pem)
MIM_KEY=$(cat32 tools/ssl/mongooseim/key.pem)
CACERT=$(cat32 tools/ssl/ca/cacert.pem)
TLS_CONFIG=$(cat32 tools/db_configs/rmq/20-tls.conf)

HEALTH_CMD="rabbitmq-diagnostics check_protocol_listener"

$DOCKER run -d \
    --name $NAME \
    -e OLD_ENTRYPOINT="docker-entrypoint.sh rabbitmq-server" \
    -e ENV_FILE_TLS_CONFIG_PATH="/etc/rabbitmq/conf.d/20-tls.conf" \
    -e ENV_FILE_TLS_CONFIG_DATA="$TLS_CONFIG" \
    -e ENV_FILE_CERT_PATH="/certs/cert.pem" \
    -e ENV_FILE_CERT_DATA="$MIM_CERT" \
    -e ENV_FILE_KEY_PATH="/certs/privkey.pem" \
    -e ENV_FILE_KEY_DATA="$MIM_KEY" \
    -e ENV_FILE_CACERT_PATH="/certs/cacert.pem" \
    -e ENV_FILE_CACERT_DATA="$CACERT" \
    -p $RMQ_PORT:5672 \
    -p $RMQ_TLS_PORT:5671 \
    -p $RMQ_HTTP_PORT:15672 \
    --health-cmd="$HEALTH_CMD amqp/ssl && $HEALTH_CMD amqp" \
    --entrypoint=/bin/sh rabbitmq:$RMQ_VERSION -c "$ENTRYPOINT"

tools/wait_for_service.sh $NAME $RMQ_PORT
tools/wait_for_healthcheck.sh $NAME
