#!/bin/bash

TOOLS=`dirname $0`

echo $DB

source tools/travis-common-vars.sh

MIM_PRIV_DIR=${BASE}/priv

DB_CONF_DIR=${BASE}/${TOOLS}/db_configs/$DB

SQL_TEMP_DIR=/tmp/sql

MYSQL_DIR=/etc/mysql/conf.d

PGSQL_ODBC_CERT_DIR=~/.postgresql

RIAK_DIR=/etc/riak

SSLDIR=${BASE}/${TOOLS}/ssl

if [ $DB = 'mysql' ]; then
    echo "Configuring mysql"
    sudo -n service mysql stop || echo "Failed to stop mysql"
    mkdir -p ${SQL_TEMP_DIR}
    cp ${SSLDIR}/fake_cert.pem ${SQL_TEMP_DIR}/.
    openssl rsa -in ${SSLDIR}/fake_key.pem -out ${SQL_TEMP_DIR}/fake_key.pem
    docker run -d \
        -e SQL_TEMP_DIR=${SQL_TEMP_DIR} \
        -e MYSQL_ROOT_PASSWORD=secret \
        -e MYSQL_DATABASE=ejabberd \
        -e MYSQL_USER=ejabberd \
        -e MYSQL_PASSWORD=mongooseim_secret \
        -v ${DB_CONF_DIR}/mysql.cnf:${MYSQL_DIR}/mysql.cnf:ro \
        -v ${MIM_PRIV_DIR}/mysql.sql:/docker-entrypoint-initdb.d/mysql.sql:ro \
        -v ${BASE}/${TOOLS}/docker-setup-mysql.sh:/docker-entrypoint-initdb.d/docker-setup-mysql.sh \
        -v ${SQL_TEMP_DIR}:${SQL_TEMP_DIR} \
        -p 3306:3306 --name=mongooseim-mysql mysql

elif [ $DB = 'pgsql' ]; then
    echo "Configuring postgres with SSL"
    sudo service postgresql stop || echo "Failed to stop psql"
    mkdir ${SQL_TEMP_DIR}
    cp ${SSLDIR}/fake_cert.pem ${SQL_TEMP_DIR}/.
    cp ${SSLDIR}/fake_key.pem ${SQL_TEMP_DIR}/.
    cp ${DB_CONF_DIR}/postgresql.conf ${SQL_TEMP_DIR}/.
    cp ${DB_CONF_DIR}/pg_hba.conf ${SQL_TEMP_DIR}/.
    cp ${MIM_PRIV_DIR}/pg.sql ${SQL_TEMP_DIR}/.
    docker run -d \
           -e SQL_TEMP_DIR=${SQL_TEMP_DIR} \
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
Password             = mongooseim_secret
sslmode              = verify-full
Protocol             = 9.3.5
Debug                = 1
ByteaAsLongVarBinary = 1
EOL

elif [ $DB = 'riak' ]; then
    echo "Configuring Riak with SSL"
    sudo cp ${SSLDIR}/fake_cert.pem ${RIAK_DIR}/cert.pem
    sudo cp ${SSLDIR}/fake_key.pem ${RIAK_DIR}/key.pem
    sudo cp ${SSLDIR}/ca/cacert.pem ${RIAK_DIR}/cacertfile.pem
    sudo cp ${DB_CONF_DIR}/advanced.config ${RIAK_DIR}/advanced.config
    sudo cp ${DB_CONF_DIR}/riak.conf ${RIAK_DIR}/riak.conf
    sudo service riak restart
    echo "Setup Riak"
    sudo tools/setup_riak mongooseim_secret

elif [ $DB = 'cassandra' ]; then
    docker image pull cassandra:${CASSANDRA_VERSION}

    opts="$(docker inspect -f '{{range .Config.Entrypoint}}{{println}}{{.}}{{end}}' cassandra:${CASSANDRA_VERSION})"
    opts+="$(docker inspect -f '{{range .Config.Cmd}}{{println}}{{.}}{{end}}' cassandra:${CASSANDRA_VERSION})"
    readarray -t -s 1 init_opts <<< "$opts"
    echo -e "cassandra startup cmd:\n\t${init_opts[@]}"

    docker_entry="${DB_CONF_DIR}/docker_entry.sh"

    docker run -d -p 9042:9042                   \
               -e MAX_HEAP_SIZE=128M             \
               -e HEAP_NEWSIZE=64M               \
               -v "${SSLDIR}:/ssl:ro"            \
               -v "${docker_entry}:/entry.sh:ro" \
               --name=cassandra                  \
               --entrypoint "/entry.sh"          \
               cassandra:${CASSANDRA_VERSION}    \
               "${init_opts[@]}"

    tools/wait_for_service.sh cassandra 9042 || docker logs cassandra

    # Deleted --rm on travis for speedup
    docker run -it -e SSL_CERTFILE=/cacert.pem                  \
               -v "${SSLDIR}/ca/cacert.pem:/cacert.pem:ro"      \
               -v "$(pwd)/priv/cassandra.cql:/cassandra.cql:ro" \
               --link cassandra:cassandra                       \
               cassandra:${CASSANDRA_VERSION}                   \
               sh -c 'exec cqlsh "$CASSANDRA_PORT_9042_TCP_ADDR" --ssl -f /cassandra.cql'
fi
