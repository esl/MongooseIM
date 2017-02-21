#!/bin/bash

source tools/travis-common-vars.sh

TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)
${TOOLS}/set-odbc-password test ${TRAVIS_DB_PASSWORD}

cd ${BASE}/test.disabled/ejabberd_tests && make prepare

