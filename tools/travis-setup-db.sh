#!/bin/bash

TOOLS=`dirname $0`

echo $DB

source tools/travis-common-vars.sh

SQLDIR=${BASE}/apps/ejabberd/priv

TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)

if [ $DB = 'mysql' ]; then
    echo "Configuring mysql"
    mysql -u root -e 'create database IF NOT EXISTS ejabberd'
    mysql -u root -e 'create user ejabberd'
    mysql -u root -e "grant all on ejabberd.* to 'ejabberd'@'localhost' identified by '${TRAVIS_DB_PASSWORD}'"
    echo "Creating schema"
    mysql -u ejabberd --password=${TRAVIS_DB_PASSWORD} ejabberd < ${SQLDIR}/mysql.sql
elif [ $DB = 'pgsql' ]; then
    echo "Configuring postgres"
    psql -U postgres -c "CREATE ROLE ejabberd PASSWORD '${TRAVIS_DB_PASSWORD}' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;"
    psql -U postgres -c "CREATE DATABASE ejabberd;"
    echo "Creating schema"
    psql -U postgres -q -d ejabberd -f ${SQLDIR}/pg.sql
    cat > ~/.odbc.ini << EOL
[ejabberd-pgsql]
Driver      = PostgreSQL Unicode
ServerName  = localhost
Port        = 5432
Database    = ejabberd
Username    = ejabberd
Password    = ${TRAVIS_DB_PASSWORD}
Protocol    = 9.3.5
Debug       = 1
EOL

fi