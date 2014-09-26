#!/bin/bash

TRAVIS_DB_PASSWORD=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)
echo ${TRAVIS_DB_PASSWORD} > /tmp/travis_db_password