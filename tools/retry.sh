#!/usr/bin/env bash

n=1
max=5
delay=3
while true; do
  "$@" && break || {
    if [[ $n -lt $max ]]; then
      ((n++))
      echo "Command failed. Attempt $n/$max:"
      sleep $delay;
    else
      echo "The command has failed after $n attempts."
      exit 1
    fi
  }
done
