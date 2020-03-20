#!/bin/bash

# Configuring postgres and creating schema
./cockroach sql --insecure --execute "CREATE DATABASE ejabberd"
./cockroach sql --insecure --execute "CREATE USER ejabberd"
./cockroach sql --insecure --execute "GRANT ALL ON DATABASE ejabberd TO ejabberd"
./cockroach sql --insecure \
    --database "ejabberd" \
    --user "ejabberd" \
    < ${SQL_TEMP_DIR}/cockroach-pg.sql
