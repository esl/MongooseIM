#!/bin/bash

source tools/travis-common-vars.sh

TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)
${TOOLS}/set-odbc-password test ${TRAVIS_DB_PASSWORD}

make test_deps && cd ${BASE}/test/ejabberd_tests && make prepare

