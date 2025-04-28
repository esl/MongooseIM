#!/usr/bin/env bash

set -e

cd big_tests
../rebar3 compile --deps_only
