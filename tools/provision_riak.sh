#!/bin/bash

INSTANCE_NAME="mongooseim-riak"
IMAGE_NAME="tutum/riak"

# SSH alias for target docker host machine
SSH_DOCKERMACHINE_ALIAS="docker_riak"
PORT1=8087
PORT2=8098

echo "trying to stop docker container...";
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker stop '${INSTANCE_NAME}''
echo "trying to remove docker container...";
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker rm '${INSTANCE_NAME}''
echo "starting the container with riak..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker run --name '${INSTANCE_NAME}'\
 -p '${PORT1}:${PORT1}' -p '${PORT2}:${PORT2}' -e RIAK_PASS="test" -d '${IMAGE_NAME}''

