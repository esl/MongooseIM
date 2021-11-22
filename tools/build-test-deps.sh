#!/usr/bin/env bash

set -e

cd big_tests
mv src src_old
mv tests tests_old
../rebar3 compile
rm -rf src tests
mv src_old src
mv tests_old tests
