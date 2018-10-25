#!/usr/bin/env bash

# This script has no arguments
# You can override a list of nodes to stop by using DEV_NODES env variable
# DEV_NODES="mim1 mim2" ./tools/stop-nodes.sh

# Stop script on an error
set -e

# We use BASE and DEV_NODES_ARRAY variables from here
source tools/travis-common-vars.sh

# Stops node
# First argument is node directory name
# Does not fail if the node is already running (but prints a message)
# Fails if release for the node is not compiled
stop_node() {
  echo -n "${1} stop: "
  ${BASE}/_build/${1}/rel/mongooseim/bin/mongooseimctl stop && echo ok || echo failed
  echo
}

# DEV_NODES_ARRAY is defined in travis-common-vars.sh
# and contains node names mim1, mim2, ...
for node in ${DEV_NODES_ARRAY[@]}; do
  stop_node $node;
done
