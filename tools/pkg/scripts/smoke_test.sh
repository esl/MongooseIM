#!/usr/bin/env bash

# Use bash "strict mode"
# Based on http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -eu pipefail
IFS=$'\n\t'

echo "Starting mongooseim via 'mongooseimctl start'"
mongooseimctl start

echo "Waiting for the port 5222 to accept TCP connections"
./wait-for-it.sh -h localhost -p 5222 -t 60

echo "Checking status via 'mongooseimctl status'"
mongooseimctl status

echo "Trying to register a user with 'mongooseimctl register localhost a_password'"
mongooseimctl register localhost a_password

echo "Trying to register a user with 'mongooseimctl register_identified user localhost a_password_2'"
mongooseimctl register_identified user localhost a_password_2

echo "Checking if 2 users are registered on host 'localhost'"
expected=2
registered=$(mongooseimctl registered_users localhost | wc -l)
if [ ${registered} -ne ${expected} ]; then
    echo "registered value is ${registered} but expected ${expected}"
    exit 1
fi

echo "Stopping mongooseim via 'mongooseimctl stop'"
mongooseimctl stop

