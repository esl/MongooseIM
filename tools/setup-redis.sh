#!/usr/bin/env bash

source tools/common-vars.sh
source tools/db-versions.sh
ENTRYPOINT=$(entrypoint)

NAME=$(db_name redis)
REDIS_PORT=${REDIS_PORT:-6379}

$DOCKER rm -v -f $NAME || echo "Skip removing the previous container"

MIM_CERT=$(cat32 tools/ssl/mongooseim/cert.pem)
MIM_KEY=$(cat32 tools/ssl/mongooseim/key.pem)
CACERT=$(cat32 tools/ssl/ca/cacert.pem)
ENTRYCMD="redis-server --tls-port 6379 --port 0 --tls-cert-file /certs/cert.pem --tls-key-file /certs/privkey.pem --tls-ca-cert-file /certs/cacert.pem --tls-auth-clients no"
IMAGE=redis:${REDIS_VERSION}
$DOCKER run -d --name $NAME \
    -e OLD_ENTRYPOINT="$ENTRYCMD" \
    -e ENV_FILE_CERT_PATH="/certs/cert.pem" \
    -e ENV_FILE_CERT_DATA="$MIM_CERT" \
    -e ENV_FILE_KEY_PATH="/certs/privkey.pem" \
    -e ENV_FILE_KEY_DATA="$MIM_KEY" \
    -e ENV_FILE_CACERT_PATH="/certs/cacert.pem" \
    -e ENV_FILE_CACERT_DATA="$CACERT" \
    -p $REDIS_PORT:6379 \
    --health-cmd="redis-cli --tls --cacert /certs/cacert.pem -h 127.0.0.1 -p 6379 ping" \
    --entrypoint=/bin/sh $IMAGE -c "$ENTRYPOINT"

tools/wait_for_healthcheck.sh $NAME
tools/wait_for_service.sh $NAME $REDIS_PORT

# To test: redis-cli --tls --cacert tools/ssl/ca/cacert.pem -h 127.0.0.1 -p 6379 ping


