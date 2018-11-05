#!/usr/bin/env bash
# This script reads test.config and returns preset list

set -e

# Go to the repo directory
cd "$( dirname "${BASH_SOURCE[0]}" )"/../..

erlc tools/test_runner/list_presets.erl
erl -noinput \
    -pa tools/test_runner \
    -s list_presets main $@
