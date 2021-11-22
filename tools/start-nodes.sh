#!/usr/bin/env bash

# This script has no arguments
# You can override a list of nodes to start by using DEV_NODES env variable
# DEV_NODES="mim1 mim2" ./tools/start-nodes.sh

# Stop script on an error
set -e

# We use BASE and DEV_NODES_ARRAY variables from here
source tools/common-vars.sh

start_nodes() {
  for node in ${DEV_NODES_ARRAY[@]}; do
    ${BASE}/_build/${node}/rel/mongooseim/bin/mongooseim start_clean &
  done
}

if [ "$START_NODES" = true ]; then
  start_nodes
else
  echo "Skipping MongooseIM nodes start"
fi
