#!/bin/bash

LDIF_FILE_PATH="/home/go/init_entries.ldif"

LDAP_PORT=389
LDAP_HOST=localhost  #script to be run where ldap server is
LDAP_ROOTPASS="mongoose"

cat > ${LDIF_FILE_PATH} <<EOF
 EOL
dn: ou=Users,dc=esl,dc=com
objectClass: organizationalUnit
ou: users
EOL

STATUS=1
RETRIES=3
until [ $STATUS -eq 0 ] || [ $RETRIES -eq 0 ]; do
    sleep 1
    ldapadd -h ${LDAP_HOST} -p ${LDAP_PORT} -c -x -D cn=admin,dc=esl,dc=com \
        -w ${LDAP_ROOTPASS} -f ${LDIF_FILE_PATH}
    STATUS=$?
    (( RETRIES-- ))
done
EOF
