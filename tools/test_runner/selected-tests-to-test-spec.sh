#!/usr/bin/env bash
# This file creates two spec files:
# - auto_big_tests.spec
# - auto_small_tests.spec (if needed)

set -e

# Go to the repo directory
cd "$( dirname "${BASH_SOURCE[0]}" )"/../..

echo "Selected tests: $@"

rm -f auto_small_tests.spec big_tests/auto_big_tests.spec
# Remove the link, if exist
rm -f _build/test/lib/mongooseim/auto_small_tests.spec

# Fill default specs
cp big_tests/default.spec big_tests/auto_big_tests.spec

# If there are arguments
if [ "$#" -ne 0 ]; then
erlc tools/test_runner/selected_tests_to_test_spec.erl
erl -noinput \
    -pa tools/test_runner \
    -pa _build/test/lib/mongooseim/test/ \
    -pa big_tests/tests/ \
    -pa big_tests/_build/default/lib/ejabberd_tests/ebin/ \
    -s selected_tests_to_test_spec main $@
fi

# Rebar3 does not copy spec file on macosx
# This is a workaround
if [[ -f auto_small_tests.spec ]]; then
    mkdir -p _build/test/lib/mongooseim
    cp auto_small_tests.spec _build/test/lib/mongooseim/auto_small_tests.spec
fi
