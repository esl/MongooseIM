#!/usr/bin/env bash

set -e
source tools/travis-common-vars.sh
LDAP_ROOTPASS=mongooseim_secret

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

cat > "$LDAP_SCHEMAS_DIR/init_entries.ldif" << EOL
dn: ou=Users,dc=esl,dc=com
objectClass: organizationalUnit
ou: users
EOL

cp tools/ssl/mongooseim/{cert,key}.pem "$LDAP_CERT_DIR"
cp tools/ssl/ca/cacert.pem "$LDAP_CERT_DIR"

docker rm -f mongooseim-ldap || echo "Skip removing previous container"
# Host on non-standard higher ports 3389 and 3636 to avoid problems with lower ports
# Default LDAP ports are 389 (TCP) and 636 (TLS)

docker run -d \
    --name mongooseim-ldap \
    -p 3389:389 \
    -p 3636:636 \
    -e LDAP_DOMAIN="$LDAP_DOMAIN" \
    -e LDAP_ADMIN_PASSWORD="$LDAP_ROOTPASS" \
    -e LDAP_ORGANISATION="$LDAP_ORGANISATION" \
    -e LDAP_TLS_CRT_FILENAME=cert.pem \
    -e LDAP_TLS_KEY_FILENAME=key.pem \
    -e LDAP_TLS_CA_CRT_FILENAME=cacert.pem \
    $(data_on_volume -v "$LDAP_CONFIG_DIR:/etc/ldap/slapd.d") \
    $(data_on_volume -v "$LDAP_DATA_DIR:/var/lib/ldap") \
    $(mount_ro_volume "$LDAP_SCHEMAS_DIR" /container/service/slapd/assets/config/bootstrap/ldif/custom/) \
    $(mount_ro_volume "$LDAP_CERT_DIR" /container/service/slapd/assets/certs/) \
    osixia/openldap:1.2.4 \
    --copy-service

n=0
until [ $n -ge 10 ]
do
    echo "Q" | openssl s_client -connect localhost:3636 \
                       -cert tools/ssl/mongooseim/cert.pem -key tools/ssl/mongooseim/key.pem \
                       -CAfile tools/ssl/ca/cacert.pem && break
    n=$[$n+1]
    sleep 15
done
