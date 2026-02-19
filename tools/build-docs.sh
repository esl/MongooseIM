#!/usr/bin/env bash

set -e

## Build GraphQL static docs
npx spectaql -t doc/graphql-api -f admin-graphql-doc.html doc/graphql-api/Admin-GraphQL_spectaql.yml
npx spectaql -C -J -t doc/graphql-api -f user-graphql-doc.html doc/graphql-api/User-GraphQL_spectaql.yml

## Build docs
mkdocs build --strict
