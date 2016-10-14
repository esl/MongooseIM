#!/bin/bash

source tools/travis-common-vars.sh

TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)

${TOOLS}/set-odbc-password vars ${TRAVIS_DB_PASSWORD}

./tools/configure $REL_CONFIG

cat configure.out
echo ""

make dev

