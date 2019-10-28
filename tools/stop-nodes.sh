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
  ${BASE}/_build/${1}/rel/mongooseim/bin/mongooseimctl force_stop && echo ok
}

async_helper() {
  local ret_val=0 output=""
  output="$("$@")" || ret_val="$?"
  echo; echo "$output"; echo
  return "$ret_val"
}

wait_for_pids() {
  ## wait for all pids
  wait "$@" || true
  ## wait for pids one by one, so script can be stopped on error
  for pid in "$@"; do
    wait "$pid"
  done
}

# DEV_NODES_ARRAY is defined in travis-common-vars.sh
# and contains node names mim1, mim2, ...
stop_nodes() {
  local pids=()
  for node in ${DEV_NODES_ARRAY[@]}; do
    async_helper stop_node $node &
    pids+=("$!")
  done
  wait_for_pids "${pids[@]}"
}

stop_nodes
