#!/bin/bash

PGSQLDIR=/var/lib/postgresql/data

# Preparing SSL certificates
cp ${SQL_TEMP_DIR}/fake_cert.pem ${PGSQLDIR}/.
cp ${SQL_TEMP_DIR}/fake_key.pem ${PGSQLDIR}/.
chmod 600 ${PGSQLDIR}/fake*
chown postgres:postgres ${PGSQLDIR}/fake*

#Copy custom config
cp ${SQL_TEMP_DIR}/postgresql.conf ${PGSQLDIR}/.
cp ${SQL_TEMP_DIR}/pg_hba.conf ${PGSQLDIR}/.

#Copy db scheme
cp ${SQL_TEMP_DIR}/pg.sql ${PGSQLDIR}/.

# Configuring postgres
psql -U postgres -c "CREATE ROLE mongooseim PASSWORD 'mongooseim_secret' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;"
psql -U postgres -c "CREATE DATABASE mongooseim;"
# Creating schema
psql -U postgres -q -d mongooseim -f ${PGSQLDIR}/pg.sql
