#!/bin/bash

source tools/travis-common-vars.sh

TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)

${TOOLS}/set-odbc-password vars ${TRAVIS_DB_PASSWORD}

make devclean devrel
echo -n "starting MongooseIM node 1: "
${EJD1CTL} start && echo ok || echo failed
echo -n "starting MongooseIM node 2: "
${EJD2CTL} start && echo ok || echo failed
sleep 1
echo -n "pinging MongooseIM node 1: "
${EJD1CTL} ping
echo -n "pinging MongooseIM node 2: "
${EJD2CTL} ping

make test_deps
${TOOLS}/set-odbc-password test ${TRAVIS_DB_PASSWORD}

