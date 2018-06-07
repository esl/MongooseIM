#!/usr/bin/env bash

source tools/travis-helpers.sh

set -e

if [ -z "$TRAVIS_PULL_REQUEST" ]; then
    echo "\$TRAVIS_PULL_REQUEST is empty. Do nothing"
    exit 0
fi

if [ "$TRAVIS_PULL_REQUEST" = "false" ]; then
    echo "Not a pull request. Do nothing"
    exit 0
fi

if [ -z "$COMMENTER_GITHUB_TOKEN" ]; then
    echo "\$COMMENTER_GITHUB_TOKEN is empty. Do nothing"
    exit 0
fi

PRESET="${PRESET:-default}"
TRAVIS_OTP_RELEASE="${TRAVIS_OTP_RELEASE:-unknown}"


if [ -z "$AWS_SECRET_ACCESS_KEY" ]; then
    REPORTS_URL_BODY="Reports are not uploaded"
else
    CT_REPORTS=$(ct_reports_dir)
    REPORTS_URL=$(s3_url ${CT_REPORTS})
    BUILD_NAME="$PRESET $TRAVIS_OTP_RELEASE"
    REPORTS_URL_BODY="[Reports URL for $BUILD_NAME](${REPORTS_URL})"
fi

ERRORS_BODY="$(cat /tmp/ct_markdown || echo '/tmp/ct_markdown missing')"
BODY="${REPORTS_URL_BODY}${ERRORS_BODY}"
POST_BODY=$(BODY_ENV="$BODY" jq -n '{body: env.BODY_ENV}')
TRAVIS_REPO_SLUG=${TRAVIS_REPO_SLUG:-esl/MongooseIM}

curl -o /dev/null -i \
    -H "Authorization: token $COMMENTER_GITHUB_TOKEN" \
    -H "Content-Type: application/json" \
    -X POST -d "$POST_BODY" \
    https://api.github.com/repos/$TRAVIS_REPO_SLUG/issues/$TRAVIS_PULL_REQUEST/comments
