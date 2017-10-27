#bin/bash

PGSQLDIR=/var/lib/postgresql/data

# Preparing SSL certificates
cp ${PGSQL_TEMP_DIR}/fake_cert.pem ${PGSQLDIR}/.
cp ${PGSQL_TEMP_DIR}/fake_key.pem ${PGSQLDIR}/.
chmod 600 ${PGSQLDIR}/fake*
chown postgres:postgres ${PGSQLDIR}/fake*

#Copy custom config
cp ${PGSQL_TEMP_DIR}/postgresql.conf ${PGSQLDIR}/.
cp ${PGSQL_TEMP_DIR}/pg_hba.conf ${PGSQLDIR}/.

#Copy db scheme
cp ${PGSQL_TEMP_DIR}/pg.sql ${PGSQLDIR}/.

# Configuring postgres
psql -U postgres -c "CREATE ROLE ejabberd PASSWORD '${TRAVIS_DB_PASSWORD}' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;"
psql -U postgres -c "CREATE DATABASE ejabberd;"
# Creating schema
psql -U postgres -q -d ejabberd -f ${PGSQLDIR}/pg.sql
