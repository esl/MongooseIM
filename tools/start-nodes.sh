#!/usr/bin/env bash

# This script has no arguments
# You can override a list of nodes to start by using DEV_NODES env variable
# DEV_NODES="mim1 mim2" ./tools/start-nodes.sh

# Stop script on an error
set -e

# We use BASE and DEV_NODES_ARRAY variables from here
source tools/travis-common-vars.sh

# This script adds a marker into logs, so we would know which lines are new
START_NODES_DATE=$(date)

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

print_logs() {
    echo "Print logs for $1"
    # Show only extra lines by matching from the marker, set in start_node function in this script

    # Match from TERMINATE till the end of file example:
    # sed -n -e '/TERMINATE/,$p'
    # https://stackoverflow.com/questions/7103531/how-to-get-the-part-of-file-after-the-line-that-matches-grep-expression-first
    cat _build/$1/rel/mongooseim/log/erlang.log.1 | "$SED" -n -e '/start_node '"$START_NODES_DATE"'/,$p' | "$SED" -e 's/^/[erlang.log.1]    /'
}

# Starts node in background
# First argument is node directory name
# Does not fail if the node is already running (but prints a message)
# Fails if release for the node is not compiled
start_node() {
  mkdir -p _build/$1/rel/mongooseim/log
  # Make a marker to know which lines are new
  # We would not handle log rotation though
  echo "start_node $START_NODES_DATE" >> _build/$1/rel/mongooseim/log/erlang.log.1 || true

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
  exit_code=0
  check_node "$1" || exit_code="$?"
  if [ $exit_code -ne 0 ]; then
      echo "Node $1 not running"
      print_logs $1
  fi
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
