#!/usr/bin/env bash
# This script converts a list of test hosts into a list of erlang nodes based on test.config
#
# Usage:
# ./tools/test_runner/hosts_to_nodes.sh mim mim2
# mongooseim@localhost
# ejabberd2@localhost

set -e

# Go to the repo directory
cd "$( dirname "${BASH_SOURCE[0]}" )"/../..

erlc tools/test_runner/hosts_to_nodes.erl
erl -noinput \
    -pa tools/test_runner \
    -s hosts_to_nodes main $@
