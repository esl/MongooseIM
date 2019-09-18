#!/usr/bin/env bash

set -e
source tools/travis-common-vars.sh
LDAP_ROOTPASS=mongooseim_secret

NAME=$(db_name ldap)

LDAP_ROOT="cn=admin,dc=esl,dc=com"
LDAP_DOMAIN="esl.com"
LDAP_ORGANISATION="Erlang Solutions"

echo "configuring slapd"

LDAP_ROOT_DIR="$(mktempdir mongoose_ldap_root)"
LDAP_SCHEMAS_DIR="$LDAP_ROOT_DIR/prepopulate"
LDAP_DATA_DIR="$LDAP_ROOT_DIR/data"
LDAP_CONFIG_DIR="$LDAP_ROOT_DIR/config"
LDAP_CERT_DIR="$LDAP_ROOT_DIR/certs"

echo "LDAP_ROOT_DIR=$LDAP_ROOT_DIR"

mkdir -p "$LDAP_SCHEMAS_DIR" "$LDAP_DATA_DIR" "$LDAP_CONFIG_DIR" "$LDAP_CERT_DIR"

function write_init_entries
{
    cat > "$LDAP_SCHEMAS_DIR/init_entries$1.ldif" << EOL
dn: ou=Users$1,dc=esl,dc=com
objectClass: organizationalUnit
EOL
}

write_init_entries

# Make Users1, Users2, ... Users10 OU-s, which can be used for parallel jobs
for i in {1..10}; do
    write_init_entries $i
done

cp tools/ssl/mongooseim/{cert,key,dh_server}.pem "$LDAP_CERT_DIR"
cp tools/ssl/ca/cacert.pem "$LDAP_CERT_DIR"

docker rm -v -f $NAME || echo "Skip removing previous container"
# Host on non-standard higher ports 3389 and 3636 to avoid problems with lower ports
# Default LDAP ports are 389 (TCP) and 636 (TLS)

for i in "$LDAP_CERT_DIR/"*; do
    echo "Print $i"
    cat "$i"
    echo ""
done

LDAP_PORT=${LDAP_PORT:-3389}
LDAP_SECURE_PORT=${LDAP_SECURE_PORT:-3636}

docker run -d \
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
    $(data_on_volume -v "$LDAP_CONFIG_DIR:/etc/ldap/slapd.d") \
    $(data_on_volume -v "$LDAP_DATA_DIR:/var/lib/ldap") \
    $(mount_ro_volume "$LDAP_SCHEMAS_DIR" /container/service/slapd/assets/config/bootstrap/ldif/custom/) \
    $(mount_ro_volume "$LDAP_CERT_DIR" /container/service/slapd/assets/certs/) \
    --health-cmd='ldapwhoami -x' \
    osixia/openldap:1.2.4 \
    --copy-service

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

./tools/wait_for_healthcheck.sh "$NAME" || { docker logs $NAME; exit 1; }
