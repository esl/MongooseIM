#!/bin/bash

TOOLS=`dirname $0`

echo $DB

if [ `uname` = "Darwin" ]; then
    BASE=$(cd "$TOOLS/.."; pwd -P)
else
    BASE=`readlink -f ${TOOLS}/..`
fi

SQLDIR=${BASE}/apps/ejabberd/priv
TRAVIS_DB_PASSWORD=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)
echo ${TRAVIS_DB_PASSWORD} > /tmp/travis_db_password

if [ $DB = 'mysql' ]; then
    echo "Configuring mysql"
    mysql -u root -e 'create database IF NOT EXISTS ejabberd'
    mysql -u root -e 'create user ejabberd'
    mysql -u root -e "grant all on ejabberd.* to 'ejabberd'@'localhost' identified by '${TRAVIS_DB_PASSWORD}'"
    echo "Creating schema"
    mysql -u ejabberd --password=${TRAVIS_DB_PASSWORD} ejabberd < ${SQLDIR}/mysql.sql
elif [ $DB = 'psql' ]; then
    echo "Configuring postgres"
    psql -c "CREATE DATABASE ejabberd;" -U postgres
    echo "Creating schema"
    psql -U postgres -q -d ejabberd -f ${SQLDIR}/pg.sql
    psql -c "CREATE USER ejabberd WITH PASSWORD '${TRAVIS_DB_PASSWORD}'" -U postgres
    psql -c "GRANT ALL PRIVILEGES ON DATABASE ejabberd to ejabberd;" -U postgres
fi