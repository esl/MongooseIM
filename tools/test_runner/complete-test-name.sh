#!/usr/bin/env bash
# This script returns a list of groups and suites, one per line.
#
# Format of the output:
# - "SUITE:GROUP"
# - "SUITE:GROUP:TESTCASE"
# - "SUITE:GROUP1:GROUP2:...:GROUP_N:TESTCASE"
# - "SUITE:TESTCASE"
#
# Example:
# ./tools/test_runner/complete-test-name.sh "mam"
# ./tools/test_runner/complete-test-name.sh "mam:"
# ./tools/test_runner/complete-test-name.sh "mam:blabla"
set -e

# Go to the repo directory
cd "$( dirname "${BASH_SOURCE[0]}" )"/../..

SUITE=$(echo "$1" | cut -d':' -f1)

if [ -f "_build/test/lib/mongooseim/test/${SUITE}_SUITE.erl" ] || [ -f "big_tests/tests/${SUITE}_SUITE.erl" ]; then
erlc tools/test_runner/complete_test_name.erl
erl -noinput \
    -pa tools/test_runner \
    -pa _build/test/lib/mongooseim/test/ \
    -pa big_tests/_build/default/lib/ejabberd_tests/ebin/ \
    -pa big_tests/tests/ \
    -s complete_test_name main "${SUITE}"
fi
