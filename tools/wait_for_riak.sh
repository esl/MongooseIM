#!/usr/bin/env bash
# We cannot just connect to 127.0.0.1:8098 because Docker is very "smart" and
# exposes ports before the service is ready
IP=$(/usr/bin/docker inspect -f {{.NetworkSettings.IPAddress}} riak)
echo "riak IP is $IP"

tools/wait-for-it.sh -h $IP -p 8098
