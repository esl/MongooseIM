#!/usr/bin/env bash
# Env variable:
# - DEV_NODES - a list of releases to build
#
# By default all releases are built
# When DEV_NODES is empty, no releases are built

# Use bash "strict mode"
# Based on http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -eu

DEV_NODES="${DEV_NODES-devrel}"

# "make devrel", but for a list of dev nodes
if [ -z "$DEV_NODES" ]; then
    echo "Skip make devrel"
else
    echo "Build $DEV_NODES"
    make $DEV_NODES
fi
