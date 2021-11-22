#!/usr/bin/env bash

set -e

# Hide our code from rebar to not compile it at this stage
mv src src_old
mv asn1 asn1_old
./rebar3 compile
# rebar3 could create src directory on its own
rm -rf src asn1 asngen
mv src_old src
mv asn1_old asn1
