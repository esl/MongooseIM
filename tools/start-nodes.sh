#!/usr/bin/env bash

# This script has no arguments
# You can override a list of nodes to start by using DEV_NODES env variable
# DEV_NODES="mim1 mim2" ./tools/start-nodes.sh

# Stop script on an error
set -e

# We use BASE and DEV_NODES_ARRAY variables from here
source tools/travis-common-vars.sh

# Starts node in background
# First argument is node directory name
# Does not fail if the node is already running (but prints a message)
# Fails if release for the node is not compiled
start_node() {
  echo -n "${1} start: "
  ${BASE}/_build/${1}/rel/mongooseim/bin/mongooseimctl start && echo ok || echo failed
  echo
}

# Ensures that node is up
# Prints node information
# First argument is node directory name
# Fails if the node does not appear after 1 minute
wait_for_node() {
  echo -n "waiting for ${1}: "
  echo
  ${BASE}/_build/${1}/rel/mongooseim/bin/mongooseimctl started
  ${BASE}/_build/${1}/rel/mongooseim/bin/mongooseimctl status
  echo
}

# DEV_NODES_ARRAY is defined in travis-common-vars.sh
# and contains node names mim1, mim2, ...
for node in ${DEV_NODES_ARRAY[@]}; do
  start_node $node;
done

for node in ${DEV_NODES_ARRAY[@]}; do
  wait_for_node $node;
done
