#!/usr/bin/env sh

set -e

## Install spectaql for building GraphQL static docs
npm install --global cheerio@1.0.0-rc.12 spectaql

## Install mkdocs and plugins
pip3 install mkdocs
pip3 install mkdocs-material
pip3 install mkdocs-include-markdown-plugin
pip3 install mkdocs_puml
pip3 install mkdocs-mermaid2-plugin
