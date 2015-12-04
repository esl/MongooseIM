#!/bin/bash

INSTANCE_NAME="mongooseim-ldap"
IMAGE_NAME="nickstenning/slapd"

# SSH alias for target docker host machine
SSH_DOCKERMACHINE_ALIAS="docker_ldap"
PORT1=389

echo "trying to stop docker container...";
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker stop '${INSTANCE_NAME}''
echo "trying to remove docker container...";
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker rm '${INSTANCE_NAME}''
echo "starting the container with ldap (slapd)..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker run --name '${INSTANCE_NAME}' \
      -p '${PORT1}:${PORT1}' \
      -e LDAP_DOMAIN=esl.com \
      -e LDAP_ORGANIZATION="MongooseIM" \
      -e LDAP_ROOTPASS=mongoose \
      -d '${IMAGE_NAME}''

sleep 5

