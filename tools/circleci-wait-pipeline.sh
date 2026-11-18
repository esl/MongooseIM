#!/usr/bin/env bash

set -euo pipefail

readonly NOT_FOUND_RETRIES=5

fetch_workflow() {
  local response
  response="$(curl -sS -w '\n%{http_code}' "https://circleci.com/api/v2/pipeline/$pipeline_id/workflow" \
    -H "Circle-Token: $CIRCLECI_TOKEN")"

  http_code="${response##*$'\n'}"
  http_body="${response%$'\n'*}"
}

if [ -z "${CIRCLECI_TOKEN:-}" ]; then
  echo "CIRCLECI_TOKEN is not set" >&2
  exit 1
fi

pipeline_id="${1:-}"

if [ -z "$pipeline_id" ]; then
  echo "Pipeline ID is empty or not provided" >&2
  exit 1
fi

not_found_count=0

while true; do
  fetch_workflow

  if [ "$http_code" != 404 ]; then
    break
  fi

  if (( not_found_count >= NOT_FOUND_RETRIES )); then
    break
  fi

  not_found_count="$((not_found_count + 1))"

  echo /
  sleep 2
done

if [ "$http_code" == 404 ]; then
  echo "Pipeline remained unavailable after $NOT_FOUND_RETRIES retries (HTTP 404):" >&2
  echo "$http_body" >&2
  exit 1
fi

while true; do
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

  fetch_workflow
done
