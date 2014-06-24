#!/bin/bash

G_API="https://api.github.com"

echo "pull request: ${TRAVIS_PULL_REQUEST}"
if [ ${TRAVIS_PULL_REQUEST} != "false" ]; then
    wget http://stedolan.github.io/jq/download/linux64/jq
    chmod +x jq
    JQ='./jq -a -M'
    PULL_REQ_FILE="/tmp/mongoose_pull_req"
    curl ${G_API}/repos/esl/MongooseIM/pulls/${TRAVIS_PULL_REQUEST} > ${PULL_REQ_FILE}
    M_PULL_USER=`${JQ} '.head.user.login' ${PULL_REQ_FILE}`
    M_PULL_USER=${M_PULL_USER//\"/}
    M_PULL_BRANCH=`${JQ} '.head.ref' ${PULL_REQ_FILE}`
    M_PULL_BRANCH=${M_PULL_BRANCH//\"/}
    M_PULL_USER_EJD_TESTS_BRANCH="${G_API}/repos/${M_PULL_USER}/ejabberd_tests/branches/${M_PULL_BRANCH}"
    TEST_BRANCH_FILE="/tmp/tests_branch"
    curl ${M_PULL_USER_EJD_TESTS_BRANCH} > ${TEST_BRANCH_FILE}
    if [ `${JQ} '.name' ${TEST_BRANCH_FILE}` != "null" ]; then
        echo "Using tests branch for given pull request"
        GIT_REPO="git:\/\/github.com\/${M_PULL_USER}\/ejabberd_tests.git"
        NEW_TEST_LINE="{ejabberd_tests, \\\".*\\\", {git, \\\"${GIT_REPO}\\\", {branch, \\\"${M_PULL_BRANCH}\\\"}}}"
        sed "s/{ejabberd_tests.*/${NEW_TEST_LINE}/" rebar.tests.config > rebar.tests.config2
        mv rebar.tests.config2 rebar.tests.config
    fi
fi