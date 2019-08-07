#!/usr/bin/env bash

# Travis packs all cache directories into one archive for caching.
# So, we put files into tar, not tar.gz to improve compression
# (because the files are shared between mongooseim and big_tests)

set -e
CACHE_DIR=~/.cache/mim_certs

# Get hash of last commit in tools/ssl
SHA_HASH=$(cd tools/ssl && git log -1 --pretty=format:"%h" --abbrev=40 .)

EXT=.tar
TMP_FILE=/tmp/mim_cache

echo "SHA_HASH=$SHA_HASH"

ARCHIVE_FILE=$CACHE_DIR/${SHA_HASH}${EXT}

if [ "$1" = 'restore' ]; then
    if test -f "$ARCHIVE_FILE"; then
        echo "Build cache found, restore"
        tar xf "$ARCHIVE_FILE"
    fi
elif [ "$1" = 'store' ]; then
    if ! test -f "$ARCHIVE_FILE"; then
        rm -f "$TMP_FILE"
        tar \
            -cf "$TMP_FILE" "tools/ssl"
        mkdir -p "$CACHE_DIR"
        # Remove any previous files to preserve space
        rm -f "$CACHE_DIR/*"
        mv "$TMP_FILE" "$ARCHIVE_FILE"
    fi
fi
