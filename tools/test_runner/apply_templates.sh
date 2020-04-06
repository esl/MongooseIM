#!/usr/bin/env bash
# This script generates release templates

set -e

# Go to the repo directory
cd "$( dirname "${BASH_SOURCE[0]}" )"/../..

erlc tools/test_runner/apply_templates.erl
erl -noinput \
    -pa _build/default/lib/*/ebin \
    -pa tools/test_runner \
    -s apply_templates main $@
