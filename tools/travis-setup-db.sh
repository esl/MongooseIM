#!/bin/bash

TOOLS=`dirname $0`

echo $DB

source tools/travis-common-vars.sh

SQLDIR=${BASE}/apps/ejabberd/priv

PGSQL_CONF_DIR=${BASE}/${TOOLS}/db_configs/postgres

SQL_TEMP_DIR=/tmp/sql

MYSQL_CONF_DIR=/etc/mysql/conf.d

PGSQL_ODBC_CERT_DIR=~/.postgresql

TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)

SSLDIR=${BASE}/${TOOLS}/ssl

if [ $DB = 'mysql' ]; then
    echo "Configuring mysql"
    sudo service mysql stop || echo "Failed to stop mysql"
    mkdir ${SQL_TEMP_DIR}
    cp ${SSLDIR}/fake_cert.pem ${SQL_TEMP_DIR}/.
    openssl rsa -in ${SSLDIR}/fake_key.pem -out ${SQL_TEMP_DIR}/fake_key.pem
    docker run -d \
        -e SQL_TEMP_DIR=${SQL_TEMP_DIR} \
        -e MYSQL_ROOT_PASSWORD=secret \
        -e MYSQL_DATABASE=ejabberd \
        -e MYSQL_USER=ejabberd \
        -e MYSQL_PASSWORD=$TRAVIS_DB_PASSWORD \
        -v ${SQLDIR}/mysql.cnf:${MYSQL_CONF_DIR}/mysql.cnf:ro \
        -v ${SQLDIR}/mysql.sql:/docker-entrypoint-initdb.d/mysql.sql:ro \
        -v ${BASE}/${TOOLS}/docker-setup-mysql.sh:/docker-entrypoint-initdb.d/docker-setup-mysql.sh \
        -v ${SQL_TEMP_DIR}:${SQL_TEMP_DIR} \
        -p 3306:3306 --name=mongooseim-mysql mysql

elif [ $DB = 'pgsql' ]; then
    echo "Configuring postgres with SSL"
    sudo service postgresql stop || echo "Failed to stop psql"
    mkdir ${SQL_TEMP_DIR}
    cp ${SSLDIR}/fake_cert.pem ${SQL_TEMP_DIR}/.
    cp ${SSLDIR}/fake_key.pem ${SQL_TEMP_DIR}/.
    cp ${PGSQL_CONF_DIR}/postgresql.conf ${SQL_TEMP_DIR}/.
    cp ${PGSQL_CONF_DIR}/pg_hba.conf ${SQL_TEMP_DIR}/.
    cp ${SQLDIR}/pg.sql ${SQL_TEMP_DIR}/.
    docker run -d \
           -e SQL_TEMP_DIR=${SQL_TEMP_DIR} \
           -e TRAVIS_DB_PASSWORD=${TRAVIS_DB_PASSWORD} \
           -v ${SQL_TEMP_DIR}:${SQL_TEMP_DIR} \
           -v ${BASE}/${TOOLS}/docker-setup-postgres.sh:/docker-entrypoint-initdb.d/docker-setup-postgres.sh \
           -p 5432:5432 --name=mongooseim-pgsql postgres
    mkdir ${PGSQL_ODBC_CERT_DIR} || echo "PGSQL odbc cert dir already exists"
    cp ${SSLDIR}/ca/cacert.pem ${PGSQL_ODBC_CERT_DIR}/root.crt
    cat > ~/.odbc.ini << EOL
[ejabberd-pgsql]
Driver               = PostgreSQL Unicode
ServerName           = localhost
Port                 = 5432
Database             = ejabberd
Username             = ejabberd
Password             = ${TRAVIS_DB_PASSWORD}
sslmode              = verify-full
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
        --name=mongooseim-riak riak
    tools/wait_for_service.sh mongooseim-riak 8098 || docker logs riak
    tools/setup_riak

elif [ $DB = 'cassandra' ]; then
    docker run -d -p 9042:9042 -e MAX_HEAP_SIZE=128M -e HEAP_NEWSIZE=64M --name=cassandra cassandra:${CASSANDRA_VERSION}
    tools/wait_for_service.sh cassandra 9042 || docker logs cassandra

    # Deleted --rm on travis for speedup
    docker run -it -v "$(pwd)/apps/ejabberd/priv/cassandra.cql:/cassandra.cql:ro" \
        --link cassandra:cassandra \
        cassandra:${CASSANDRA_VERSION} \
        sh -c 'exec cqlsh "$CASSANDRA_PORT_9042_TCP_ADDR" -f /cassandra.cql'
fi
