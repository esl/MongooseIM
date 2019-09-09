#!/usr/bin/env bash
# Env variable:
# - DEV_NODES - a list of releases to build
#
# When DEV_NODES is empty, all development releases are built

# Use bash "strict mode"
# Based on http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -eu

DEV_NODES="${DEV_NODES-devrel}"
BUILD_MIM="${BUILD_MIM-true}"
COPY_RELEASE="${COPY_RELEASE-true}"

if [ "$DEV_NODES" = "devrel" ]; then
    DEV_NODES=$(make print_devnodes)
fi


FIRST_NODE=
function try_copy_release
{
    local NODE=$1

    if [[ "$FIRST_NODE" = "" ]]; then
        FIRST_NODE=$NODE
        make $NODE
    else
        # Clone from first node
        rsync \
            --exclude rel/mongooseim/Mnesia.* \
            --exclude rel/mongooseim/var \
            --exclude rel/mongooseim/log \
            -al _build/$FIRST_NODE/ _build/$NODE/
        ./tools/test_runner/apply_templates.sh "$NODE" "$(pwd)/_build/$NODE/"
    fi
}


# "make devrel", but for a list of dev nodes
if [ -z "$DEV_NODES" ] || [ "$BUILD_MIM" = false ]; then
    echo "Skip make devrel"

elif [ "$COPY_RELEASE" = true ]; then
    for NODE in $DEV_NODES; do
        time try_copy_release $NODE
    done
else
    echo "Build $DEV_NODES"
    make $DEV_NODES
fi
