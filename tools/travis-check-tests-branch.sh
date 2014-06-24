#!/bin/bash

G_API="https://api.github.com"

echo "pull request: ${TRAVIS_PULL_REQUEST}"
if [ ${TRAVIS_PULL_REQUEST} != "false" ]; then
    wget http://stedolan.github.io/jq/download/linux64/jq
    chmod +x jq
    JQ='./jq -a -M'
    PULL_REQ_FILE="/tmp/mongoose_pull_req"
    MONGOOSE_REPO_URL="${G_API}/repos/esl/MongooseIM"
    curl ${MONGOOSE_REPO_URL}/pulls/${TRAVIS_PULL_REQUEST} > ${PULL_REQ_FILE}
    M_PULL_USER=`${JQ} '.head.user.login' ${PULL_REQ_FILE}`
    M_PULL_USER=${M_PULL_USER//\"/}
    M_PULL_BRANCH=`${JQ} '.head.ref' ${PULL_REQ_FILE}`
    M_PULL_BRANCH=${M_PULL_BRANCH//\"/}
    M_PULL_TEST_REPO_URL=${G_API}/repos/${M_PULL_USER}/ejabberd_tests
    M_PULL_USER_EJD_TESTS_BRANCH="${M_PULL_TEST_REPO_URL}/branches/${M_PULL_BRANCH}"
    TEST_BRANCH_FILE="/tmp/tests_branch"
    curl ${M_PULL_USER_EJD_TESTS_BRANCH} > ${TEST_BRANCH_FILE}
    if [ `${JQ} '.name' ${TEST_BRANCH_FILE}` != "null" ]; then
        GITHUB_TOKEN="ffead32f44b837d57d64b3ecd13158ea7bd03fc4"
        echo "Using tests branch for given pull request"
        GIT_REPO="git:\/\/github.com\/${M_PULL_USER}\/ejabberd_tests.git"
        NEW_TEST_LINE="{ejabberd_tests, \\\".*\\\", {git, \\\"${GIT_REPO}\\\", {branch, \\\"${M_PULL_BRANCH}\\\"}}}"
        sed "s/{ejabberd_tests.*/${NEW_TEST_LINE}/" rebar.tests.config > rebar.tests.config2
        mv rebar.tests.config2 rebar.tests.config
        M_COMMENTS_FILE="/tmp/pull_comments"
        M_PULL_COMMENTS_URL="${MONGOOSE_REPO_URL}/issues/${TRAVIS_PULL_REQUEST}/comments"
        curl ${M_PULL_COMMENTS_URL} > ${M_COMMENTS_FILE}
        MY_COMMENTS=`${JQ} 'map(select(.user.login == "mongoose-travis-commenter")) | length' ${M_COMMENTS_FILE}`
        if [ ${MY_COMMENTS} = 0 ]; then
            M_PULL_COMMENT="travis is using test branch \`${M_PULL_BRANCH}\` from https://github.com/${M_PULL_USER}/ejabberd_tests/tree/${M_PULL_BRANCH}"
            curl -H "Authorization: token ${GITHUB_TOKEN}" \
                 -X POST -d "{\"body\":\"${M_PULL_COMMENT}\"" \
                 ${M_PULL_COMMENTS_URL}
        fi
    fi
fi