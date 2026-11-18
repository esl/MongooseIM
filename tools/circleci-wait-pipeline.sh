#!/usr/bin/env bash

set -euo pipefail

if [ -z "${CIRCLECI_TOKEN:-}" ]; then
  echo "CIRCLECI_TOKEN is not set" >&2
  exit 1
fi

pipeline_id="${1:-}"

if [ -z "$pipeline_id" ]; then
  echo "Invalid pipeline id: '$pipeline_id'" >&2
  exit 1
fi

while true; do
  response="$(
    curl -sS -w '\n%{http_code}' "https://circleci.com/api/v2/pipeline/$pipeline_id/workflow" \
      -H "Circle-Token: $CIRCLECI_TOKEN"
  )"

  http_code="${response##*$'\n'}"
  http_body="${response%$'\n'*}"

  if ! status="$(jq -er '.items | map(.status) | unique | join(",")' <<< "$http_body" 2>/dev/null)"; then
    echo "Unexpected response (HTTP $http_code):" >&2
    echo "$http_body" >&2
    exit 1
  fi

  case "$status" in
    *running* | *on_hold* | "")
      echo .
      sleep 10;;

    success)
      exit 0;;

    *)
      echo "Workflow failed: $status" >&2
      exit 1;;
  esac
done
