#!/usr/bin/env bash
# This script converts a list of presets into a list of databases based on test.config

set -e

# Go to the repo directory
cd "$( dirname "${BASH_SOURCE[0]}" )"/../..

erlc tools/test_runner/presets_to_dbs.erl
erl -noinput \
    -pa tools/test_runner \
    -s presets_to_dbs main $@
