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

# COMMENTER_GITHUB_USER is nickname of a special github user.
# COMMENTER_GITHUB_TOKEN is token with scope public repo.
# Token can be obtained here https://github.com/settings/tokens/new
# Don't use ANY REAL user as a commenter, because GitHub access scopes are too wide.
echo "Used vars (you can use them to debug locally):"
echo "export COMMENTER_GITHUB_TOKEN=fillme"
echo "export COMMENTER_GITHUB_USER=$COMMENTER_GITHUB_USER"
echo "export PRESET=$PRESET"
echo "export TRAVIS_COMMIT=$TRAVIS_COMMIT"
echo "export TRAVIS_JOB_ID=$TRAVIS_JOB_ID"
echo "export TRAVIS_JOB_NUMBER=$TRAVIS_JOB_NUMBER"
echo "export TRAVIS_REPO_SLUG=$TRAVIS_REPO_SLUG"
echo "export TRAVIS_OTP_RELEASE=$TRAVIS_OTP_RELEASE"
echo "export TRAVIS_PULL_REQUEST=$TRAVIS_PULL_REQUEST"

PRESET="${PRESET:-default}"
TRAVIS_OTP_RELEASE="${TRAVIS_OTP_RELEASE:-unknown}"

function remove_ct_log_links
{
    mv /tmp/ct_markdown /tmp/ct_markdown_original
    grep -v "Report log" /tmp/ct_markdown_original > /tmp/ct_markdown
}

# https://stackoverflow.com/questions/407523/escape-a-string-for-a-sed-replace-pattern
function replace_string {
    sed -i "s/$(
        echo "$1" | sed -e 's/\([[\/.*]\|\]\)/\\&/g'
                  | sed -e 's:\t:\\t:g'
    )/$(
        echo "$2" | sed -e 's/[\/&]/\\&/g'
                  | sed -e 's:\t:\\t:g'
    )/g" "$3"
}

function rewrite_log_links_to_s3
{
    # REPORTS_URL is
    # http://esl.github.io/mongooseim-ct-reports/s3_reports.html?prefix=PR/1916/4855/ldap_mnesia.19.3
    # redirects to
    # http://mongooseim-ct-results.s3-eu-west-1.amazonaws.com/PR/1885/4744/mysql_redis.19.3/big/
    REPORTS_URL="$1"
    CT_REPORT=big_tests/ct_report
    CT_REPORT_ABS=$(realpath "$CT_REPORT")
    S3_REPORT="$REPORTS_URL/big"
    cp /tmp/ct_markdown /tmp/ct_markdown_original
    replace_string "$CT_REPORT_ABS" "$S3_REPORT" /tmp/ct_markdown
    # URL escape for s3_reports.html script
    replace_string "ct_run.test@" "ct_run.test%40" /tmp/ct_markdown
}

if [ -z "$AWS_SECRET_ACCESS_KEY" ]; then
    REPORTS_URL_BODY="Reports are not uploaded"
    remove_ct_log_links
else
    CT_REPORTS=$(ct_reports_dir)
    REPORTS_URL=$(s3_url ${CT_REPORTS})
    REPORTS_URL_BODY="[Reports URL](${REPORTS_URL})"
    rewrite_log_links_to_s3 "$REPORTS_URL"
fi

# Link to a travis job
JOB_URL="https://travis-ci.org/$TRAVIS_REPO_SLUG/jobs/$TRAVIS_JOB_ID"
DESC_BODY="[$TRAVIS_JOB_NUMBER]($JOB_URL) / Erlang $TRAVIS_OTP_RELEASE / $PRESET / $TRAVIS_COMMIT"$'\n'
# This file is created by ct_markdown_errors_hook
ERRORS_BODY="$(cat /tmp/ct_markdown || echo '/tmp/ct_markdown missing')"
BODY="${DESC_BODY}${REPORTS_URL_BODY}${ERRORS_BODY}"
# SLUG is the same for both GitHub and Travis CI
TRAVIS_REPO_SLUG=${TRAVIS_REPO_SLUG:-esl/MongooseIM}

function post_new_comment
{
# Create a comment GitHub API doc
# https://developer.github.com/v3/issues/comments/#create-a-comment
echo "Posting a new comment"
POST_BODY=$(BODY_ENV="$BODY" jq -n '{body: env.BODY_ENV}')
curl -o /dev/null -i \
    -H "Authorization: token $COMMENTER_GITHUB_TOKEN" \
    -H "Content-Type: application/json" \
    -X POST -d "$POST_BODY" \
    https://api.github.com/repos/$TRAVIS_REPO_SLUG/issues/$TRAVIS_PULL_REQUEST/comments
}

function append_comment
{
# Concat old comment text and some extra text
# Keep a line separator between them
# $'\n' is a new line in bash
#
# Edit commment GitHub API doc
# https://developer.github.com/v3/issues/comments/#edit-a-comment
COMMENT_ID="$1"
echo "Patch comment $COMMENT_ID"
BODY_FROM_GH="$(cat /tmp/gh_comment | jq -r .body)"
BODY="${BODY_FROM_GH}"$'\n'$'\n'"---"$'\n'$'\n'"${BODY}"
PATCH_BODY=$(BODY_ENV="${BODY}" jq -n '{body: env.BODY_ENV}')
curl -o /dev/null -i \
    -H "Authorization: token $COMMENTER_GITHUB_TOKEN" \
    -H "Content-Type: application/json" \
    -X PATCH -d "$PATCH_BODY" \
    https://api.github.com/repos/$TRAVIS_REPO_SLUG/issues/comments/$COMMENT_ID
}

# List comments
# https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue
curl -s -S -o /tmp/gh_comments -L \
    -H "Authorization: token $COMMENTER_GITHUB_TOKEN" \
    -H "Content-Type: application/json" \
    https://api.github.com/repos/$TRAVIS_REPO_SLUG/issues/$TRAVIS_PULL_REQUEST/comments

# Filter out all comments for a particular user
# Then filter out all comments that have a git commit rev in the body text
# Then take first comment (we don't expect more comments anyway)
cat /tmp/gh_comments | jq "map(select(.user.login == \"$COMMENTER_GITHUB_USER\")) | map(select(.body | contains(\"$TRAVIS_COMMIT\")))[0]" > /tmp/gh_comment
COMMENT_ID=$(cat /tmp/gh_comment | jq .id)

if [ "$COMMENT_ID" = "null" ]; then
    post_new_comment
else
    append_comment "$COMMENT_ID"
fi
