#!/usr/bin/env bash

# This script has no arguments

source tools/travis-common-vars.sh

MIM1=${BASE}/_build/mim1/rel/mongooseim
MIM2=${BASE}/_build/mim2/rel/mongooseim
MIM3=${BASE}/_build/mim3/rel/mongooseim
FED1=${BASE}/_build/fed1/rel/mongooseim
MIM1CTL=${MIM1}/bin/mongooseimctl
MIM2CTL=${MIM2}/bin/mongooseimctl
MIM3CTL=${MIM3}/bin/mongooseimctl
FED1CTL=${FED1}/bin/mongooseimctl

NODES=(${MIM1CTL} ${MIM2CTL} ${MIM3CTL} ${FED1CTL})

start_node() {
  echo -n "${1} start: "
  ${1} start && echo ok || echo failed
  echo
}

wait_for_node() {
  echo -n "waiting for ${1}: "
  echo
  ${1} started
  ${1} status
  echo
}

for node in ${NODES[@]}; do
  start_node $node;
done

for node in ${NODES[@]}; do
  wait_for_node $node;
done
