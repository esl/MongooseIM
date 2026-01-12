#!/usr/bin/env bash

source tools/helpers.sh

set -e

if [ -z "$CIRCLE_BRANCH" ]; then
    echo "\$CIRCLE_BRANCH is empty. Do nothing"
    exit 0
fi

if [ -z "$CIRCLE_PULL_REQUEST" ]; then
    echo "\$CIRCLE_PULL_REQUEST is empty. Do nothing"
    exit 0
fi

if [ "$CIRCLE_PULL_REQUEST" = "false" ]; then
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
echo "export CIRCLE_SHA1=$CIRCLE_SHA1"
echo "export CIRCLE_BUILD_URL=$CIRCLE_BUILD_URL"
echo "export CIRCLE_JOB=$CIRCLE_JOB"
echo "export CIRCLE_PROJECT_USERNAME=$CIRCLE_PROJECT_USERNAME"
echo "export CIRCLE_PROJECT_REPONAME=$CIRCLE_PROJECT_REPONAME"
echo "export CIRCLE_PULL_REQUEST=$CIRCLE_PULL_REQUEST"

PR_NUM="${CIRCLE_PULL_REQUEST##*/}"

REPO_SLUG="$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME"

# IF we only run small tests, the file is missing
touch /tmp/ct_markdown

PRESET="${PRESET:-default}"

function remove_ct_log_links
{
    mv /tmp/ct_markdown /tmp/ct_markdown_original
    # grep fails if nothing matches
    { grep -v "Report log" /tmp/ct_markdown_original || echo ""; } > /tmp/ct_markdown
}

# https://stackoverflow.com/questions/407523/escape-a-string-for-a-sed-replace-pattern
function replace_string {
    sed -i "s/$( \
        echo "$1" | sed -e 's/\([[\/.*]\|\]\)/\\&/g' \
                  | sed -e 's:\t:\\t:g' \
    )/$( \
        echo "$2" | sed -e 's/[\/&]/\\&/g' \
                  | sed -e 's:\t:\\t:g' \
    )/g" "$3"
}

function rewrite_log_links_to_s3
{
    local CT_REPORT=big_tests/ct_report
    local CT_REPORT_ABS=$(./tools/abs_dirpath.sh "$CT_REPORT")
    local CT_REPORTS=$(ct_reports_dir)
    local BIG_TESTS_URL="$(archive_reader_url big ${CT_REPORTS})"
    cp /tmp/ct_markdown /tmp/ct_markdown_original
    replace_string "$CT_REPORT_ABS" "$BIG_TESTS_URL" /tmp/ct_markdown
    # URL escape for s3_reports.html script
    replace_string "ct_run.test@" "ct_run.test%40" /tmp/ct_markdown
}

function last_ct_run_name
{
    ls -1 -t big_tests/ct_report/ | grep ct_run | head -n1
}


function last_small_ct_run_name
{
    if [ -d _build/test/logs ]; then
        ls -1 -t _build/test/logs/ | grep ct_run | head -n1
    fi
}

function ct_run_url
{
    local CT_REPORTS=$(ct_reports_dir)
    local BIG_TESTS_URL="$(archive_reader_url big ${CT_REPORTS})"
    local RUN_PART=$(echo "$(last_ct_run_name)" | sed "s/@/%40/g")
    echo "$BIG_TESTS_URL/$RUN_PART/index.html"
}

function ct_small_url
{
    local CT_REPORTS=$(ct_reports_dir)
    local SMALL_TESTS_URL="$(archive_reader_url small ${CT_REPORTS})"
    local RUN_PART=$(echo "$(last_small_ct_run_name)" | sed "s/@/%40/g")
    echo "$SMALL_TESTS_URL/$RUN_PART/index.html"
}

function reports_url
{
    local CT_REPORTS=$(ct_reports_dir)
    # Something like that:
    # http://esl.github.io/circleci-mim-results/s3_reports.html?prefix=PR/3173/60934/pgsql_mnesia.23.0.3-1
    s3_url "${CT_REPORTS}"
}

if [ -z "$AWS_SECRET_ACCESS_KEY" ]; then
    REPORTS_URL_BODY="Reports are not uploaded"$'\n'
    remove_ct_log_links
else
    REPORTS_URL="$(reports_url)"
    CT_RUN_URL="$(ct_run_url)"
    SMALL_CT_URL="$(ct_small_url)"
    REPORTS_BIG_URL_BODY=""
    REPORTS_SMALL_URL_BODY=""
    if [ -f big_tests/ct_report/index.html ]; then
        rewrite_log_links_to_s3
        REPORTS_BIG_URL_BODY="/ [big]($CT_RUN_URL)"
    fi
    if [ -d _build/test/logs ]; then
        REPORTS_SMALL_URL_BODY=" / [small]($SMALL_CT_URL)"
    fi
    REPORTS_URL_BODY="Reports [root](${REPORTS_URL})${REPORTS_BIG_URL_BODY}${REPORTS_SMALL_URL_BODY}"$'\n'
fi

COUNTERS_FILE=/tmp/ct_stats_vars
COUNTERS_BODY=""
STATUS_LINE=""
if [ -f "$COUNTERS_FILE" ]; then
    . "$COUNTERS_FILE"
    COUNTERS_BODY="**OK: $CT_COUNTER_OK** "
    if [ "$CT_COUNTER_FAILED" != "0" ]; then
        COUNTERS_BODY="$COUNTERS_BODY/ **Failed: $CT_COUNTER_FAILED** "
        STATUS_LINE="Status: 🔴 Failed"$'\n'
    else
        COUNTERS_BODY="$COUNTERS_BODY/ Failed: 0 "
        STATUS_LINE="Status: 🟢 Passed"$'\n'
    fi
    if [ "$CT_COUNTER_USER_SKIPPED" != "0" ]; then
        COUNTERS_BODY="$COUNTERS_BODY/ **User-skipped: $CT_COUNTER_USER_SKIPPED** "
    else
        COUNTERS_BODY="$COUNTERS_BODY/ User-skipped: 0 "
    fi
    if [ "$CT_COUNTER_AUTO_SKIPPED" != "0" ]; then
        COUNTERS_BODY="$COUNTERS_BODY/ **Auto-skipped: $CT_COUNTER_AUTO_SKIPPED** "
    else
        COUNTERS_BODY="$COUNTERS_BODY/ Auto-skipped: 0 "
    fi
    COUNTERS_BODY="$COUNTERS_BODY"$'\n'
fi

TRUNCATED_BODY=""
# Number of truncated failed tests if file exists
TRUNCATED_FILE="/tmp/ct_markdown_truncated"
if [ -f "$TRUNCATED_FILE" ]; then
    TRUNCATED_COUNTER=$(cat "$TRUNCATED_FILE")
    TRUNCATED_BODY=$'\n'$'\n'"$TRUNCATED_COUNTER errors were truncated"
fi

# Link to a CircleCI job
JOB_URL="$CIRCLE_BUILD_URL"
DESC_BODY="[$CIRCLE_JOB]($JOB_URL) / $PRESET / $CIRCLE_SHA1"$'\n'
# This file is created by ct_markdown_errors_hook
ERRORS_BODY="$(cat /tmp/ct_markdown || echo '/tmp/ct_markdown missing')"

# One persistent marker per PR so subsequent commits edit the same comment
COMMENT_MARKER="<!-- circleci-comment:pr-${PR_NUM} -->"
SECTION_BEGIN="<!-- circleci-section:${CIRCLE_JOB}:${PRESET}:begin -->"
SECTION_END="<!-- circleci-section:${CIRCLE_JOB}:${PRESET}:end -->"

SECTION_BODY="${DESC_BODY}${STATUS_LINE}${REPORTS_URL_BODY}${COUNTERS_BODY}${ERRORS_BODY}${TRUNCATED_BODY}"$'\n'
SECTION="${SECTION_BEGIN}"$'\n'"---"$'\n'"${SECTION_BODY}"$'\n'"${SECTION_END}"$'\n'

# Body for a brand new comment
BODY_NEW="CircleCI results for $CIRCLE_SHA1"$'\n'$'\n'"${SECTION}"$'\n'"${COMMENT_MARKER}"$'\n'

BODY_TO_PUBLISH="$BODY_NEW"

function post_new_comment
{
# Create a comment GitHub API doc
# https://developer.github.com/v3/issues/comments/#create-a-comment
echo "Posting a new comment"
POST_BODY=$(BODY_ENV="$BODY_TO_PUBLISH" jq -n '{body: env.BODY_ENV}')
curl -o /dev/null -i \
    -H "Authorization: token $COMMENTER_GITHUB_TOKEN" \
    -H "Content-Type: application/json" \
    -X POST -d "$POST_BODY" \
    https://api.github.com/repos/$REPO_SLUG/issues/$PR_NUM/comments
}

function update_comment
{
# Edit commment GitHub API doc
# https://developer.github.com/v3/issues/comments/#edit-a-comment
COMMENT_ID="$1"
echo "Updating comment $COMMENT_ID"
PATCH_BODY=$(BODY_ENV="$BODY_TO_PUBLISH" jq -n '{body: env.BODY_ENV}')
curl -o /dev/null -i \
    -H "Authorization: token $COMMENTER_GITHUB_TOKEN" \
    -H "Content-Type: application/json" \
    -X PATCH -d "$PATCH_BODY" \
    https://api.github.com/repos/$REPO_SLUG/issues/comments/$COMMENT_ID
}

# List comments
# https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue

function get_comments_page {
    PAGE=$1
    FILE=/tmp/gh_comments_page$1
    curl -s -S -o $FILE -L \
        -H "Authorization: token $COMMENTER_GITHUB_TOKEN" \
        -H "Content-Type: application/json" \
        "https://api.github.com/repos/$REPO_SLUG/issues/$PR_NUM/comments?per_page=100&page=$PAGE"
    if test "$(cat "$FILE" | jq length)" = "100"; then
        # Get next page
        ((PAGE=PAGE+1))
        get_comments_page $PAGE
    fi
}

get_comments_page 1
# Add all comments into one document
jq -s "map(.) | add" /tmp/gh_comments_page* > /tmp/gh_comments

# Filter out all comments for a particular user
# Then filter out all comments that have our marker in the body text (new behavior)
# Then take the most recent matching comment.
cat /tmp/gh_comments | jq "
    map(select(.user.login == \"$COMMENTER_GITHUB_USER\"))
    | map(select(.body | contains(\"$COMMENT_MARKER\")))
    | sort_by(.created_at)
    | last
" > /tmp/gh_comment
COMMENT_ID=$(cat /tmp/gh_comment | jq .id)

if [ "$COMMENT_ID" = "null" ]; then
    post_new_comment
else
    EXISTING_BODY=$(cat /tmp/gh_comment | jq -r '.body // ""')
    BODY_TO_PUBLISH=$(EXISTING_BODY_ENV="$EXISTING_BODY" \
        SECTION_BEGIN_ENV="$SECTION_BEGIN" \
        SECTION_END_ENV="$SECTION_END" \
        SECTION_ENV="$SECTION" \
        COMMENT_MARKER_ENV="$COMMENT_MARKER" \
        jq -rn '
            def rtrim: sub("\n+$"; "");
            def escape_regex: gsub("([\\\\^$.|?*+()\\[\\]{}])"; "\\\\$1");
            . as $null
            | (env.EXISTING_BODY_ENV) as $existing
            | (env.SECTION_BEGIN_ENV) as $begin
            | (env.SECTION_END_ENV) as $end
            | (env.SECTION_ENV | rtrim) as $section
            | (env.COMMENT_MARKER_ENV) as $marker

            | ($begin | escape_regex) as $begin_re
            | ($end | escape_regex) as $end_re
            | ("(?s)" + $begin_re + ".*" + $end_re) as $pattern

            | if ($existing | test($pattern)) then
                $existing | sub($pattern; $section)
              elif ($existing | contains($marker)) then
                (($existing | split($marker)) as $parts
                 | ($parts[0] | rtrim) + "\n\n" + $section + "\n\n" + $marker + ($parts[1] // ""))
              else
                (($existing | rtrim) + "\n\n" + $section + "\n\n" + $marker + "\n")
              end
            | if endswith("\n") then . else . + "\n" end
        ')
    update_comment "$COMMENT_ID"
fi
