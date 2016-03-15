#!/bin/bash

TOOLS=`dirname $0`

echo $DB

source tools/travis-common-vars.sh

SQLDIR=${BASE}/apps/ejabberd/priv

TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)

maybe_install_mysql() {
    service mysql status
    if [ $? -ne 0 ]; then
        sudo apt-get -y install mysql-server mysql-client
    fi
}

if [ $DB = 'mysql' ]; then
    maybe_install_mysql
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
Driver               = PostgreSQL Unicode
ServerName           = localhost
Port                 = 5432
Database             = ejabberd
Username             = ejabberd
Password             = ${TRAVIS_DB_PASSWORD}
Protocol             = 9.3.5
Debug                = 1
ByteaAsLongVarBinary = 1
EOL

elif [ $DB = 'riak' ]; then
    # Make riak image with search enabled
    docker build -t riak $(pwd)/tools/docker/riak
    docker run -d -p 8087:8087 -p 8098:8098 \
        -e DOCKER_RIAK_BACKEND=leveldb \
        -e DOCKER_RIAK_CLUSTER_SIZE=1 \
        --name=riak riak
    tools/wait_for_riak.sh || docker logs riak
    tools/setup_riak

fi
