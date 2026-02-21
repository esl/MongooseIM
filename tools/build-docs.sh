#!/usr/bin/env bash

set -e

retry() {
    local max_attempts="$1"
    shift
    local attempt=1

    while [ "$attempt" -le "$max_attempts" ]; do
        if "$@"; then
            return 0
        fi
        echo "Attempt $attempt failed. Retrying..." >&2
        ((attempt++))
    done

    return 1
}

## Build GraphQL static docs
npx spectaql -t doc/graphql-api -f admin-graphql-doc.html doc/graphql-api/Admin-GraphQL_spectaql.yml
npx spectaql -C -J -t doc/graphql-api -f user-graphql-doc.html doc/graphql-api/User-GraphQL_spectaql.yml

## Build docs

## Sometimes public plantuml server is too busy and mkdocs build fails,
## so we need to retry it a few times. Successfully built diagrams a cached locally,
## so next attempts will be faster and more likely to succeed.
retry 10 mkdocs build --strict
