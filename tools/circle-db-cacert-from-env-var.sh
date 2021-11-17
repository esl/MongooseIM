#!/usr/bin/env bash

if [ -z "$DB_CACERT" ]
then
    echo "DB_CACERT is empty"
else
    echo "Apply DB_CACERT"
    echo "$DB_CACERT" | base32 --decode > tools/ssl/ca/db_cacert.pem
fi
