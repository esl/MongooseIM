#!/usr/bin/env bash

if [ ! -z "$CASSANDRA_IP" ]; then
    sed -i "s/\"service-hostname\": \".*\"/\"service-hostname\": \"$CASSANDRA_IP\"/g" "/data/zazkia-routes.json"
fi
