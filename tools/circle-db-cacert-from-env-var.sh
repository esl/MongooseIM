#!/usr/bin/env bash

if [ -z "$DB_CACERT" ]
then
    echo "DB_CACERT is empty"
else
    echo "Apply DB_CACERT"
    echo "$DB_CACERT" | base32 --decode > tools/ssl/ca/db_cacert.pem
fi

if [ -z "$LDAP_FAKE_KEY" ]
then
    echo "LDAP_FAKE_KEY is empty"
else
    echo "Apply LDAP_FAKE_KEY"
    echo "$LDAP_FAKE_KEY" | base32 --decode > tools/ssl/mongooseim/ldap_fake_key.pem
fi

if [ -z "$LDAP_FAKE_CERT" ]
then
    echo "LDAP_FAKE_CERT is empty"
else
    echo "Apply LDAP_FAKE_CERT"
    echo "$LDAP_FAKE_CERT" | base32 --decode > tools/ssl/mongooseim/ldap_fake_cert.pem
fi
