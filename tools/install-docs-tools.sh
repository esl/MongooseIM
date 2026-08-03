#!/usr/bin/env sh

set -e

## spectaql (for building GraphQL static docs)
npm install --global spectaql

## Zensical itself
pip3 install zensical

## mike for Zensical
pip3 install git+https://github.com/squidfunk/mike.git
