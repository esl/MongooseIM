#!/usr/bin/env bash

# Environment variable DB is used by this script.
# If DB is undefined, than this script does nothing.

set -e

TOOLS=`dirname $0`

source tools/travis-common-vars.sh

MIM_PRIV_DIR=${BASE}/priv

DB_CONF_DIR=${BASE}/${TOOLS}/db_configs/$DB

SQL_TEMP_DIR=/tmp/sql

MYSQL_DIR=/etc/mysql/conf.d

PGSQL_ODBC_CERT_DIR=~/.postgresql

SSLDIR=${BASE}/${TOOLS}/ssl

if [ "$DB" = 'mysql' ]; then
    echo "Configuring mysql"
    # TODO We should not use sudo
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
        --health-cmd='mysqladmin ping --silent' \
        -p 3306:3306 --name=mongooseim-mysql mysql

elif [ "$DB" = 'pgsql' ]; then
    echo "Configuring postgres with SSL"
    sudo service postgresql stop || echo "Failed to stop psql"
    mkdir -p ${SQL_TEMP_DIR}
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

elif [ "$DB" = 'riak' ]; then
    echo "Configuring Riak with SSL"
    # Instead of docker run, use "docker create" + "docker start".
    # So we can prepare our container.
    # We use HEALTHCHECK here, check "docker ps" to get healthcheck status.
    # We can't use volumes for riak.conf, because:
    # - we want to change it
    # - container starting code runs sed on it and gets IO error,
    #   if it's a volume
    time docker create -p 8087:8087 -p 8098:8098 \
        -e DOCKER_RIAK_BACKEND=leveldb \
        -e DOCKER_RIAK_CLUSTER_SIZE=1 \
        --name=mongooseim-riak \
        -v "${DB_CONF_DIR}/advanced.config:/etc/riak/advanced.config:ro" \
        -v "${SSLDIR}/fake_cert.pem:/etc/riak/cert.pem:ro" \
        -v "${SSLDIR}/fake_key.pem:/etc/riak/key.pem:ro" \
        -v "${SSLDIR}/ca/cacert.pem:/etc/riak/ca/cacertfile.pem:ro" \
        --health-cmd='riak-admin status' \
        "michalwski/docker-riak:1.0.6"
    # Use a temporary file to store config
    TEMP_RIAK_CONF=$(mktemp)
    # Export config from a container
    docker cp "mongooseim-riak:/etc/riak/riak.conf" "$TEMP_RIAK_CONF"
    # Enable search
    sed -i "s/^search = \(.*\)/search = on/" "$TEMP_RIAK_CONF"
    # Enable ssl by appending settings from riak.conf.ssl
    cat "${DB_CONF_DIR}/riak.conf.ssl" >> "$TEMP_RIAK_CONF"
    # Import config back into container
    docker cp "$TEMP_RIAK_CONF" "mongooseim-riak:/etc/riak/riak.conf"
    # Erase temporary config file
    rm "$TEMP_RIAK_CONF"
    docker start mongooseim-riak
    echo "Waiting for docker healthcheck"
    echo ""
    tools/wait_for_healthcheck.sh mongooseim-riak
    echo "Waiting for a listener to appear"
    tools/wait_for_service.sh mongooseim-riak 8098
    # Use riak-admin from inside the container
    export RIAK_ADMIN="docker exec mongooseim-riak riak-admin"
    tools/setup_riak
    # Use this command to read Riak's logs if something goes wrong
    # docker exec -it mongooseim-riak bash -c 'tail -f /var/log/riak/*'

elif [ "$DB" = 'cassandra' ]; then
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

elif [ "$DB" = 'mssql' ]; then
    # MSSQL wants secure passwords
    docker run -d -p 1433:1433                                  \
               --name=mongoose-mssql                            \
               -e "ACCEPT_EULA=Y"                               \
               -e "SA_PASSWORD=mongooseim_secret+ESL123"        \
               -v "$(pwd)/priv/mssql2012.sql:/mongoose.sql:ro"  \
               --health-cmd='/opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" -Q "SELECT 1"' \
               microsoft/mssql-server-linux
    tools/wait_for_healthcheck.sh mongoose-mssql
    tools/wait_for_service.sh mongoose-mssql 1433

    docker exec -it mongoose-mssql \
        /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" -Q "CREATE DATABASE ejabberd"
    docker exec -it mongoose-mssql \
        /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" -i mongoose.sql

    # Be aware, that underscore in TDS_Version is required.
    # It can't be just "TDS Version = 7.1".
    #
    # To check that connection works use:
    #
    # {ok, Conn} = odbc:connect("DSN=mongoose-mssql;UID=sa;PWD=mongooseim_secret+ESL123",[]).
    #
    # To check that TDS version is correct, use:
    #
    # odbc:sql_query(Conn, "select cast(1 as bigint)").
    #
    # It should return:
    # {selected,[[]],[{"1"}]}
    #
    # It should not return:
    # {selected,[[]],[{1.0}]}
    #
    # Be aware, that Driver and Setup values are for Ubuntu.
    # CentOS would use different ones.
    cat > ~/.odbc.ini << EOL
[mongoose-mssql]
Driver      = /usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so
Setup       = /usr/lib/x86_64-linux-gnu/odbc/libtdsS.so
Server      = 127.0.0.1
Port        = 1433
Database    = ejabberd
Username    = sa
Password    = mongooseim_secret+ESL123
Charset     = UTF-8
TDS_Version = 7.1
EOL

else
    echo "Skip setting up database"
fi
