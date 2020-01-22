#!/bin/bash
## author: sebastian.bucki@erlang-solutions.com

set -euo pipefail
ARGS=("$@")
ARGS_LEN="${#ARGS[@]}"

usage() {
  echo "Usage: $0 <debian_version_of_package>"
}

# Require valid number of arguments
if [[ ! $ARGS_LEN -eq 1 ]]; then
  usage && exit 1
fi
# Require `awk`
if ! hash awk 2>/dev/null; then
  echo "No 'awk' command available" && exit 1
fi

DEBIAN_VERSION_OF_PACKAGE=$1
SOFTWARE_VERSION="$(echo "$DEBIAN_VERSION_OF_PACKAGE" | awk -F ':|-' '{print $2}')"
PLAIN_SOFTWARE_VERSION="$(echo "${SOFTWARE_VERSION//.}")"

echo "$PLAIN_SOFTWARE_VERSION" 
