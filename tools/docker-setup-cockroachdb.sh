#!/bin/bash

certs_dir="certs"
lifetime="--lifetime=10h"
default_listen_addr_host="127.0.0.1"
advertise_addr_host=$default_listen_addr_host

# Copying certificates to CockroachDB's certs_dir
cp -r /tmp /cockroach/$certs_dir

# Changing permissions
chmod 777 /cockroach/$certs_dir
chmod -R 740 /cockroach/$certs_dir

# Generating node certificate
/cockroach/cockroach cert create-node --certs-dir="$certs_dir" \
 --ca-key="$certs_dir"/ca.key "$advertise_addr_host" "$default_listen_addr_host" $lifetime

# Generating root certificate
/cockroach/cockroach cert create-client --certs-dir="$certs_dir" \
 --ca-key="$certs_dir"/ca.key $lifetime root

# Starting original entrypoint, that launches CockroachDB
echo "Start original entrypoint /cockroach/cockroach.sh start-single-node"
eval "/cockroach/cockroach.sh start-single-node"
