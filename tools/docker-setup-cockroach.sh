#!/bin/bash

# Configuring postgres and creating schema
./cockroach sql --insecure --execute "CREATE DATABASE mongooseim"
./cockroach sql --insecure --execute "CREATE USER mongooseim"
./cockroach sql --insecure --execute "GRANT ALL ON DATABASE mongooseim TO mongooseim"
./cockroach sql --insecure \
    --database "mongooseim" \
    --user "mongooseim" \
    < ${SQL_TEMP_DIR}/cockroach-pg.sql
