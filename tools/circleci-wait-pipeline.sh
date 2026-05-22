#!/usr/bin/env bash

set -euo pipefail

id="$1"

while true; do
  status="$(
    curl -s "https://circleci.com/api/v2/pipeline/$id/workflow" \
      -H "Circle-Token: $CIRCLECI_TOKEN" \
      | jq -r '.items | map(.status) | join(",")'
  )"

  case "$status" in
    *running* | "")
      sleep 10;;

    success)
      exit 0;;

    *)
      echo "Workflow failed: $status" >&2
      exit 1;;
  esac
done
