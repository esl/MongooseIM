#!/usr/bin/env bash

set -euo pipefail

if [ -z "${CIRCLECI_TOKEN:-}" ]; then
  echo "CIRCLECI_TOKEN is not set" >&2
  exit 1
fi

branch="$1"

echo "Branch: $branch" >&2

response="$(
  curl -sS -w '\n%{http_code}' https://circleci.com/api/v2/project/gh/esl/mongooseim-packages/pipeline \
    -X POST \
    -H "Circle-Token: $CIRCLECI_TOKEN" \
    -H 'Content-Type: application/json' \
    -d "{
          \"branch\": \"main\",
          \"parameters\": {
            \"run\": true,
            \"branch\": \"$branch\"
          }
        }"
)"

http_code="${response##*$'\n'}"
http_body="${response%$'\n'*}"

pipeline_id="$(jq -r '.id // empty' <<< "$http_body" 2>/dev/null || true)"

if [ -z "$pipeline_id" ]; then
  echo "Failed to trigger pipeline (HTTP $http_code) for branch '$branch'. Response:" >&2
  echo "$http_body" >&2
  exit 1
fi

echo "Triggered pipeline $pipeline_id (HTTP $http_code)" >&2
echo "$pipeline_id"
