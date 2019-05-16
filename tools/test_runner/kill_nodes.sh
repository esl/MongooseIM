#!/usr/bin/env bash
# This script kills a list of nodes passed as arguments
#
# Usage:
# ./tools/test_runner/kill_nodes.sh 'mongooseim@localhost'

set -e

# Go to the repo directory
cd "$( dirname "${BASH_SOURCE[0]}" )"/../..

erlc tools/test_runner/kill_nodes.erl
erl -noinput \
    -sname killer_$$@localhost \
    -setcookie ejabberd \
    -pa tools/test_runner \
    -s kill_nodes main $@
