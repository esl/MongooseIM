#!/bin/bash
# Securely erases file contents under a path given by argument.

set -eu

PATH="$1"
if [ ! -f "${PATH}" ]; then exit 1; fi
BYTE_COUNT=$(wc -c "${PATH}" | awk '{print $1}')
dd if=/dev/urandom of="${PATH}" bs=1 conv=notrunc count=${BYTE_COUNT}
rm "${PATH}"
