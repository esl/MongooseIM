#!/usr/bin/env bash

set -e
ERLANG=${TRAVIS_OTP_RELEASE:-default}
PROJECT_DIR=.
CACHE_DIR=~/.cache/mim_builds

## Use array assignment to get just hash without filename
## https://stackoverflow.com/a/5773761
SHA_ARRAY=($(cat "$PROJECT_DIR/rebar.config" "$PROJECT_DIR/rebar.lock" | shasum))
SHA_HASH=$(echo $SHA_ARRAY)

echo "SHA_HASH=$SHA_HASH"

ARCHIVE_FILE=$CACHE_DIR/$ERLANG-${SHA_HASH}.tar.gz

if [ "$1" = 'restore' ]; then
    if test -f "$ARCHIVE_FILE"; then
        echo "Build cache found, restore"
        tar xzf "$ARCHIVE_FILE"
    fi
elif [ "$1" = 'store' ]; then
    if ! test -f "$ARCHIVE_FILE"; then
        rm -f cache.tar.gz
        tar -czf cache.tar.gz "$PROJECT_DIR/_build/default/"
        mkdir -p "$CACHE_DIR"
        mv cache.tar.gz "$ARCHIVE_FILE"

        # Remove files, generated more that 7 days ago.
        # It would be nice to track access time, but there is no easy way to do so.
        find "$CACHE_DIR" -type f -mtime +7 -exec rm {} \;
    fi
fi
