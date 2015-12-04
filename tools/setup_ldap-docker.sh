#!/bin/bash


LDAP_INSTANCE=mongooseim-ldap
LDAP_HOST="http://${1}:389"
SSH_DOCKERMACHINE_ALIAS="docker_ldap"
PROVISIONING_LDAP_FULLFILENAME="setup_ldap-remote.sh"

echo "uploading script to remotely initialize ldap server ${LDAP_HOST} ..."
rsync -avz -e ssh "${PROVISIONING_LDAP_FULLFILENAME}" ${SSH_DOCKERMACHINE_ALIAS}:/home/go


# ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${LDAP_INSTANCE}' riak-admin bucket-type create users ''\{\"props\":\{\"datatype\":\"map\"\}\}'''

# ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${LDAP_INSTANCE}' riak-admin bucket-type create users ''\{\"props\":\{\"datatype\":\"map\"\}\}'''


