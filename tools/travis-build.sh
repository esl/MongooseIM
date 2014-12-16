#!/bin/bash

source tools/travis-common-vars.sh

TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)

${TOOLS}/set-odbc-password vars ${TRAVIS_DB_PASSWORD}

${TOOLS}/configure $REL_CONFIG
cat configure.out

make devclean devrel
make test_deps

${TOOLS}/set-odbc-password test ${TRAVIS_DB_PASSWORD}

cd ${BASE}/test/ejabberd_tests && make prepare

