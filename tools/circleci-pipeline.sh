#!/usr/bin/env bash

set -euo pipefail

branch="$1"

curl -s https://circleci.com/api/v2/project/gh/esl/mongooseim-packages/pipeline \
  -X POST \
  -H "Circle-Token: $CIRCLECI_TOKEN" \
  -H 'Content-Type: application/json' \
  -d "
    {
      \"branch\": \"main\",
      \"parameters\": {
        \"run\": true,
        \"branch\": \"$branch\"
      }
    }
  " \
  | jq -r .id

