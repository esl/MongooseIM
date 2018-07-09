#!/usr/bin/env bash
# Env variable:
# - DEV_NAMES - a list of releases to build
#
# By default all releases are built
# When DEV_NAMES is empty, no releases are built

DEV_NAMES="${DEV_NAMES-devrel}"

# "make devrel", but for a list of dev nodes
if [ -z "$DEV_NAMES" ]; then
    echo "Skip make devrel"
else
    echo "Build $DEV_NAMES"
    make $DEV_NAMES
fi
