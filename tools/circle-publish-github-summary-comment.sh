#!/usr/bin/env bash

if [[ ! -f /tmp/ct_summary ]] ; then
    echo 'File "/tmp/ct_summary" is not there, aborting.'
    exit
fi

if [ -z "$COMMENTER_GITHUB_TOKEN" ]; then
    echo "\$COMMENTER_GITHUB_TOKEN is empty. Do nothing"
    exit 0
fi

function make_body
{
    echo "[Build]($CIRCLE_BUILD_URL) on $CIRCLE_BRANCH / $PRESET"
    if [ ! -z "$CIRCLE_PULL_REQUEST" ]; then
        echo "PR $CIRCLE_PULL_REQUEST"
    fi
    cat /tmp/ct_summary
}

REPO_SLUG="$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME"

function post_new_comment
{
    POST_BODY=$(BODY_ENV="$BODY" jq -n '{body: env.BODY_ENV}')
    curl -v -o /dev/null -i \
        -H "Authorization: token $COMMENTER_GITHUB_TOKEN" \
        -H "Content-Type: application/json" \
        -X POST -d "$POST_BODY" \
        https://api.github.com/repos/$REPO_SLUG/issues/3685/comments
}

BODY=$(make_body)
post_new_comment
