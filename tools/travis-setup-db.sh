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
    riak version
    sudo tools/setup_riak

elif [ $DB = 'cassandra' ]; then
    docker run -d -p 9042:9042 -e MAX_HEAP_SIZE=128M -e HEAP_NEWSIZE=64M --name=cassandra cassandra:${CASSANDRA_VERSION}
    tools/wait_for_cassandra.sh || docker logs cassandra

    # Deleted --rm on travis for speedup
    docker run -it -v "$(pwd)/apps/ejabberd/priv/cassandra.cql:/cassandra.cql:ro" \
        --link cassandra:cassandra \
        cassandra:${CASSANDRA_VERSION} \
        sh -c 'exec cqlsh "$CASSANDRA_PORT_9042_TCP_ADDR" -f /cassandra.cql'
fi
