#!/bin/bash

INSTANCE_NAME="mongooseim-redis"
IMAGE_NAME="redis:3.0.5"

# SSH alias for target docker host machine
SSH_DOCKERMACHINE_ALIAS="backendhost"

if [ $# -eq 1 ]
then
    PORT=$1
    echo "port: "  "$1"
else
    PORT=6379
fi

echo "trying to stop docker container...";
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker stop '${INSTANCE_NAME}''
echo "trying to remove docker container...";
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker rm '${INSTANCE_NAME}''
echo "starting the container with redis..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker run --name '${INSTANCE_NAME}' -p '${PORT}:${PORT}' -d '${IMAGE_NAME}''
