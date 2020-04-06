#!/usr/bin/env bash
# We cannot just connect to 127.0.0.1:9042 because Docker is very "smart" and
# exposes ports before the service is ready

if [ "$#" -ne 2 ]; then
    exit "Illegal number of parameters"
fi

set -e

CONTAINER="$1"
PORT="$2"
IP=$(docker inspect -f {{.NetworkSettings.IPAddress}} "$CONTAINER")
echo "$CONTAINER IP is $IP"

if [ `uname` = "Darwin" ]; then
  # Direct access to IPs is not supported on Mac
  # https://docs.docker.com/docker-for-mac/networking/
  # But we can run wait-for-it from another container
  docker run --rm -d --name wait-helper ubuntu sleep infinity || echo "We can continue if the wait-helper exists"
  docker cp tools/wait-for-it.sh wait-helper:/wait-for-it.sh
  echo "Wait for $IP:$PORT"
  docker exec -t wait-helper /wait-for-it.sh -h "$IP" -p "$PORT"
else
  tools/wait-for-it.sh -h "$IP" -p "$PORT"
fi
