#!/bin/bash

# Preparing SSL certificates
cp ${SQL_TEMP_DIR}/fake_cert.pem ${PGDATA}/.
cp ${SQL_TEMP_DIR}/fake_key.pem ${PGDATA}/.
chmod 600 ${PGDATA}/fake*
chown postgres:postgres ${PGDATA}/fake*

#Copy custom config
cp ${SQL_TEMP_DIR}/postgresql.conf ${PGDATA}/.
cp ${SQL_TEMP_DIR}/pg_hba.conf ${PGDATA}/.

#Copy db scheme
cp ${SQL_TEMP_DIR}/pg.sql ${PGDATA}/.

# Configuring postgres
psql -U postgres -c "CREATE ROLE mongooseim PASSWORD 'mongooseim_secret' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;"
psql -U postgres -c "CREATE DATABASE mongooseim;"
# Creating schema
psql -U postgres -q -d mongooseim -f ${PGDATA}/pg.sql
