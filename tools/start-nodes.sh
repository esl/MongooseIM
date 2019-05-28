#!/usr/bin/env bash

# This script has no arguments
# You can override a list of nodes to start by using DEV_NODES env variable
# DEV_NODES="mim1 mim2" ./tools/start-nodes.sh

# Stop script on an error
set -e

# We use BASE and DEV_NODES_ARRAY variables from here
source tools/travis-common-vars.sh

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

# Starts node in background
# First argument is node directory name
# Does not fail if the node is already running (but prints a message)
# Fails if release for the node is not compiled
start_node() {
  echo -n "${1} start: "
  ${BASE}/_build/${1}/rel/mongooseim/bin/mongooseimctl start && echo ok || echo failed
}

check_node() {
 if [ "$START_NODES" = true ]; then
   ${BASE}/_build/${1}/rel/mongooseim/bin/mongooseimctl started
 else
   ${BASE}/_build/${1}/rel/mongooseim/bin/mongooseim ping | grep pong >/dev/null
 fi
}

# Ensures that node is up
# Prints node information
# First argument is node directory name
# Fails if the node does not appear after 1 minute
wait_for_node() {
  echo "waiting for ${1}: "
  check_node "$1" || { echo "not started"; return 1;}
  ${BASE}/_build/${1}/rel/mongooseim/bin/mongooseimctl status
}

# DEV_NODES_ARRAY is defined in travis-common-vars.sh
# and contains node names mim1, mim2, ...
start_nodes() {
  local pids=()
  for node in ${DEV_NODES_ARRAY[@]}; do
    async_helper start_node $node &
    pids+=("$!")
  done
  wait_for_pids "${pids[@]}"
}

wait_for_nodes() {
  local pids=()
  for node in ${DEV_NODES_ARRAY[@]}; do
    async_helper  wait_for_node $node &
    pids+=("$!")
  done
  wait_for_pids "${pids[@]}"
}

if [ "$START_NODES" = true ]; then
  start_nodes
else
  echo "Skipping MongooseIM nodes start"
fi

wait_for_nodes