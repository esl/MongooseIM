#!/bin/bash

LDAP_INSTANCE=mongooseim-ldap
LDAP_HOST="http://${1}:389"
SSH_DOCKERMACHINE_ALIAS="backendhost"
PROVISIONING_LDAP_FULLFILENAME="setup_ldap-remote.sh"

echo "uploading script to remotely initialize ldap server ${LDAP_HOST} ..."
rsync -avz -e ssh "${PROVISIONING_LDAP_FULLFILENAME}" ${SSH_DOCKERMACHINE_ALIAS}:/home/go

echo "executing remote ldap setup..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'source ~/setup_ldap-remote.sh'

