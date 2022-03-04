#!/usr/bin/env bash

set -e
source tools/common-vars.sh
source tools/db-versions.sh
LDAP_ROOTPASS=mongooseim_secret

NAME=$(db_name ldap)

LDAP_ROOT="cn=admin,dc=esl,dc=com"
LDAP_DOMAIN="esl.com"
LDAP_ORGANISATION="Erlang Solutions"

$DOCKER rm -v -f $NAME || echo "Skip removing previous container"

# Host on non-standard higher ports 3389 and 3636 to avoid problems with lower ports
# Default LDAP ports are 389 (TCP) and 636 (TLS)
LDAP_PORT=${LDAP_PORT:-3389}
LDAP_SECURE_PORT=${LDAP_SECURE_PORT:-3636}

LDAP_SCHEMA=$(cat32 tools/db_configs/ldap/init_entries.ldif)
LDAP_SETUP=$(cat32 tools/db_configs/ldap/init_script.sh)
MIM_CERT=$(cat32 tools/ssl/mongooseim/cert.pem)
MIM_KEY=$(cat32 tools/ssl/mongooseim/key.pem)
CACERT=$(cat32 tools/ssl/ca/cacert.pem)
MIM_DHSERVER=$(cat32 tools/ssl/mongooseim/dh_server.pem)

IMAGE=osixia/openldap:$LDAP_VERSION
ENTRYPOINT=$(entrypoint)

$DOCKER run -d \
    --name $NAME \
    -p $LDAP_PORT:389 \
    -p $LDAP_SECURE_PORT:636 \
    -e LDAP_DOMAIN="$LDAP_DOMAIN" \
    -e LDAP_ADMIN_PASSWORD="$LDAP_ROOTPASS" \
    -e LDAP_ORGANISATION="$LDAP_ORGANISATION" \
    -e LDAP_TLS_CRT_FILENAME=cert.pem \
    -e LDAP_TLS_KEY_FILENAME=key.pem \
    -e LDAP_TLS_CA_CRT_FILENAME=cacert.pem \
    -e LDAP_TLS_DH_PARAM_FILENAME=dh_server.pem \
    -e OLD_ENTRYPOINT="/init_script.sh && /container/tool/run --copy-service" \
    -e ENV_FILE_SH_PATH="/init_script.sh" \
    -e ENV_FILE_SH_DATA="$LDAP_SETUP" \
    -e ENV_FILE_SH_MODE=755 \
    -e ENV_FILE_SCHEMA_PATH="/container/service/slapd/assets/config/bootstrap/ldif/custom/init_entries.ldif" \
    -e ENV_FILE_SCHEMA_DATA="$LDAP_SCHEMA" \
    -e ENV_FILE_CERT_PATH="/container/service/slapd/assets/certs/cert.pem" \
    -e ENV_FILE_CERT_DATA="$MIM_CERT" \
    -e ENV_FILE_KEY_PATH="/container/service/slapd/assets/certs/key.pem" \
    -e ENV_FILE_KEY_DATA="$MIM_KEY" \
    -e ENV_FILE_CACERT_PATH="/container/service/slapd/assets/certs/cacert.pem" \
    -e ENV_FILE_CACERT_DATA="$CACERT" \
    -e ENV_FILE_DHSERVER_PATH="/container/service/slapd/assets/certs/dh_server.pem" \
    -e ENV_FILE_DHSERVER_DATA="$MIM_DHSERVER" \
    --health-cmd='ldapwhoami -x' \
    --entrypoint=/bin/sh $IMAGE -c "$ENTRYPOINT"

echo "Waiting for ldap"
n=0
until [ $n -ge 10 ]
do
    echo "Q" | openssl s_client -connect localhost:$LDAP_SECURE_PORT \
                       -cert tools/ssl/mongooseim/cert.pem -key tools/ssl/mongooseim/key.pem \
                       -CAfile tools/ssl/ca/cacert.pem && break
    n=$[$n+1]
    sleep 15
done

./tools/wait_for_healthcheck.sh "$NAME" || { $DOCKER logs $NAME; exit 1; }
