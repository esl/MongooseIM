#!/bin/bash

source tools/travis-common-vars.sh

TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)
${TOOLS}/set-odbc-password test ${TRAVIS_DB_PASSWORD}

# rebar doesn't like Unicode in commit messages #1674
# So, we unset commit message env variable
# TODO: Remove it when we switch to rebar3 for big tests.
export TRAVIS_COMMIT_MESSAGE=no_message
cd ${BASE}/test.disabled/ejabberd_tests && make prepare

