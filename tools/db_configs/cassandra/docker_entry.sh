#!/bin/bash

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

echo "Executing $@"
exec "$@"
