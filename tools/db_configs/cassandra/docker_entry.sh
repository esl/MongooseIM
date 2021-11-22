#!/bin/bash

set -e
password=fake_server

cat - >>"${CASSANDRA_CONFIG}/cassandra.yaml" <<-EOF

	client_encryption_options:
	    enabled:  true
	    optional: false
	    keystore: ${CASSANDRA_CONFIG}/fake_server.jks
	    keystore_password: ${password}

	EOF

openssl pkcs12 -export                                     \
               -out "${CASSANDRA_CONFIG}/fake_server.p12"  \
               -in /ssl/mongooseim/cert.pem                      \
               -inkey /ssl/mongooseim/privkey.pem                \
               -password "pass:${password}"

keytool -importkeystore                                     \
        -destkeystore "${CASSANDRA_CONFIG}/fake_server.jks" \
        -deststorepass "${password}"                        \
        -srckeystore "${CASSANDRA_CONFIG}/fake_server.p12"  \
        -srcstorepass "${password}"                         \
        -srcstoretype 'PKCS12'

function my_cqlsh {
    SSL_CERTFILE=/ssl/ca/cacert.pem cqlsh "127.0.0.1" --ssl "$@"
}

function wait_for_server {
    while ! my_cqlsh -e 'describe cluster' ; do
        echo "Waiting for cassandra"
        sleep 1
    done
}

function apply_schema {
    # Apply schemas
    echo "Apply Cassandra schema"
    # For some reason, "cqlsh -f" does not create schema and no error is reported.
    my_cqlsh -e "source '/schemas/mim.cql'"
    my_cqlsh -e "source '/schemas/test.cql'"
    echo "Verify Cassandra schema"
    # Would fail with reason and exit code 2:
    # <stdin>:1:InvalidRequest: Error from server: code=2200 [Invalid query] message="unconfigured table mam_config"
    my_cqlsh -e "select * from mongooseim.mam_config;"
}

function listen_for_ready {
    if [ -z "$SCHEMA_READY_PORT" ]; then
        echo "SCHEMA_READY_PORT not provided"
    else
        # Listen on a port to signal for healthcheck that we are ready
        echo "Listening for $SCHEMA_READY_PORT for healthcheck"
        perl -MIO::Socket::INET -ne 'BEGIN{$l=IO::Socket::INET->new(LocalPort => '${SCHEMA_READY_PORT}', Proto=>"tcp", Listen=>5, ReuseAddr=>1); $l=$l->accept}'
    fi
}

function wait_and_apply_schema {
    wait_for_server
    apply_schema
    listen_for_ready
}

echo "Executing $@"

wait_and_apply_schema &
/docker-entrypoint.sh cassandra -f
