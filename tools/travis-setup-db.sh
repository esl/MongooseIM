#!/usr/bin/env bash

# Environment variable DB is used by this script.
# If DB is undefined, than this script does nothing.

# Docker for Mac should be used on Mac (not docker-machine!)
# https://store.docker.com/editions/community/docker-ce-desktop-mac

set -e
TOOLS=`dirname $0`

source tools/travis-common-vars.sh

MIM_PRIV_DIR=${BASE}/priv

MYSQL_DIR=/etc/mysql/conf.d

PGSQL_ODBC_CERT_DIR=~/.postgresql

SSLDIR=${BASE}/${TOOLS}/ssl

# Don't need it for travis for speed up
RM_FLAG=" --rm "
if [ "$TRAVIS" = 'true' ]; then
    echo "Disable --rm flag on Travis"
    RM_FLAG=""
fi

# DATA_ON_VOLUME variable and data_on_volume function come from travis-common-vars.sh
echo "DATA_ON_VOLUME is $DATA_ON_VOLUME"

# Default cassandra version
CASSANDRA_VERSION=${CASSANDRA_VERSION:-3.9}

# Default ElasticSearch version
ELASTICSEARCH_VERSION=${ELASTICSEARCH_VERSION:-5.6.9}

# There is one odbc.ini for both mssql and pgsql
# Allows to run both in parallel
function install_odbc_ini
{
# CLIENT OS CONFIGURING STUFF
#
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
TDS_Version = 7.2
client_charset = UTF-8

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
}

# Stores all the data needed by the container
SQL_ROOT_DIR="$(mktempdir mongoose_sql_root)"
echo "SQL_ROOT_DIR is $SQL_ROOT_DIR"

# A directory, that contains resources that needed to bootstrap a container
# i.e. certificates and config files
SQL_TEMP_DIR="$SQL_ROOT_DIR/temp"

# A directory, that contains database server data files
# It's good to keep it outside of a container, on a volume
SQL_DATA_DIR="$SQL_ROOT_DIR/data"
mkdir -p "$SQL_TEMP_DIR" "$SQL_DATA_DIR"

function setup_db(){
db=${1:-none}
echo "Setting up db: $db"
DB_CONF_DIR=${BASE}/${TOOLS}/db_configs/$db


if [ "$db" = 'mysql' ]; then
    echo "Configuring mysql"
    # TODO We should not use sudo
    sudo -n service mysql stop || echo "Failed to stop mysql"
    docker rm -f mongooseim-mysql || echo "Skip removing previous container"
    cp ${SSLDIR}/mongooseim/cert.pem ${SQL_TEMP_DIR}/fake_cert.pem
    openssl rsa -in ${SSLDIR}/mongooseim/key.pem -out ${SQL_TEMP_DIR}/fake_key.pem
    # mysql_native_password is needed until mysql-otp implements caching-sha2-password
    # https://github.com/mysql-otp/mysql-otp/issues/83
    docker run -d \
        -e SQL_TEMP_DIR=/tmp/sql \
        -e MYSQL_ROOT_PASSWORD=secret \
        -e MYSQL_DATABASE=ejabberd \
        -e MYSQL_USER=ejabberd \
        -e MYSQL_PASSWORD=mongooseim_secret \
	$(mount_ro_volume ${DB_CONF_DIR}/mysql.cnf ${MYSQL_DIR}/mysql.cnf) \
        $(mount_ro_volume ${MIM_PRIV_DIR}/mysql.sql /docker-entrypoint-initdb.d/mysql.sql) \
	$(mount_ro_volume ${BASE}/${TOOLS}/docker-setup-mysql.sh /docker-entrypoint-initdb.d/docker-setup-mysql.sh) \
	$(mount_ro_volume ${SQL_TEMP_DIR} /tmp/sql) \
        $(data_on_volume -v ${SQL_DATA_DIR}:/var/lib/mysql) \
        --health-cmd='mysqladmin ping --silent' \
        -p 3306:3306 --name=mongooseim-mysql \
        mysql --default-authentication-plugin=mysql_native_password

elif [ "$db" = 'pgsql' ]; then
    # If you see "certificate verify failed" error in Mongoose logs, try:
    # Inside tools/ssl/:
    # make clean && make
    # Than rerun the script to create a new docker container.
    echo "Configuring postgres with SSL"
    sudo -n service postgresql stop || echo "Failed to stop psql"
    docker rm -f mongooseim-pgsql || echo "Skip removing previous container"
    cp ${SSLDIR}/mongooseim/cert.pem ${SQL_TEMP_DIR}/fake_cert.pem
    cp ${SSLDIR}/mongooseim/key.pem ${SQL_TEMP_DIR}/fake_key.pem
    cp ${DB_CONF_DIR}/postgresql.conf ${SQL_TEMP_DIR}/.
    cp ${DB_CONF_DIR}/pg_hba.conf ${SQL_TEMP_DIR}/.
    cp ${MIM_PRIV_DIR}/pg.sql ${SQL_TEMP_DIR}/.
    docker run -d \
           -e SQL_TEMP_DIR=/tmp/sql \
           $(mount_ro_volume ${SQL_TEMP_DIR} /tmp/sql) \
           $(data_on_volume -v ${SQL_DATA_DIR}:/var/lib/postgresql/data) \
           $(mount_ro_volume ${BASE}/${TOOLS}/docker-setup-postgres.sh /docker-entrypoint-initdb.d/docker-setup-postgres.sh) \
           -p 5432:5432 --name=mongooseim-pgsql postgres
    mkdir -p ${PGSQL_ODBC_CERT_DIR}
    cp ${SSLDIR}/ca/cacert.pem ${PGSQL_ODBC_CERT_DIR}/root.crt
    install_odbc_ini

elif [ "$db" = 'riak' ]; then
    echo "Configuring Riak with SSL"
    docker rm -f mongooseim-riak || echo "Skip removing previous container"
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
	$(mount_ro_volume "${DB_CONF_DIR}/advanced.config" "/etc/riak/advanced.config") \
	$(mount_ro_volume "${SSLDIR}/mongooseim/cert.pem" "/etc/riak/cert.pem") \
	$(mount_ro_volume "${SSLDIR}/mongooseim/key.pem" "/etc/riak/key.pem") \
	$(mount_ro_volume "${SSLDIR}/ca/cacert.pem" "/etc/riak/ca/cacertfile.pem") \
        $(data_on_volume -v ${SQL_DATA_DIR}:/var/lib/riak) \
        --health-cmd='riak-admin status' \
        "michalwski/docker-riak:1.0.6"
    # Use a temporary file to store config
    TEMP_RIAK_CONF=$(mktemp)
    # Export config from a container
    docker cp "mongooseim-riak:/etc/riak/riak.conf" "$TEMP_RIAK_CONF"
    # Enable search
    $SED -i "s/^search = \(.*\)/search = on/" "$TEMP_RIAK_CONF"
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

elif [ "$db" = 'cassandra' ]; then
    docker image pull cassandra:${CASSANDRA_VERSION}
    docker rm -f mongooseim-cassandra mongooseim-cassandra-proxy || echo "Skip removing previous container"

    opts="$(docker inspect -f '{{range .Config.Entrypoint}}{{println}}{{.}}{{end}}' cassandra:${CASSANDRA_VERSION})"
    opts+="$(docker inspect -f '{{range .Config.Cmd}}{{println}}{{.}}{{end}}' cassandra:${CASSANDRA_VERSION})"
    while read -r line; do
        if [ ! -z "$line" ]; then
             init_opts+=("$line")
        fi
    done <<<"$opts"
    echo -e "cassandra startup cmd:\n\t${init_opts[@]}"

    docker_entry="${DB_CONF_DIR}/docker_entry.sh"

    docker run -d                                \
               -e MAX_HEAP_SIZE=128M             \
               -e HEAP_NEWSIZE=64M               \
	       $(mount_ro_volume "${SSLDIR}" "/ssl") \
	       $(mount_ro_volume "${docker_entry}" "/entry.sh") \
               $(data_on_volume -v ${SQL_DATA_DIR}:/var/lib/cassandra) \
               --name=mongooseim-cassandra       \
               --entrypoint "/entry.sh"          \
               cassandra:${CASSANDRA_VERSION}    \
               "${init_opts[@]}"
    tools/wait_for_service.sh mongooseim-cassandra 9042 || docker logs mongooseim-cassandra

    # Start TCP proxy
    CASSANDRA_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' mongooseim-cassandra)
    echo "Connecting TCP proxy to Cassandra on $CASSANDRA_IP..."
    cp ${DB_CONF_DIR}/proxy/zazkia-routes.json "$SQL_TEMP_DIR/"
    $SED -i "s/\"service-hostname\": \".*\"/\"service-hostname\": \"$CASSANDRA_IP\"/g" "$SQL_TEMP_DIR/zazkia-routes.json"
    docker run -d                               \
               -p 9042:9042                     \
               -p 9191:9191                     \
               $(mount_ro_volume "$SQL_TEMP_DIR" /data)  \
               --name=mongooseim-cassandra-proxy \
               emicklei/zazkia
    tools/wait_for_service.sh mongooseim-cassandra-proxy 9042 || docker logs mongooseim-cassandra-proxy

    MIM_SCHEMA=$(pwd)/priv/cassandra.cql
    TEST_SCHEMA=$(pwd)/big_tests/tests/mongoose_cassandra_SUITE_data/schema.cql
    for cql_file in $MIM_SCHEMA $TEST_SCHEMA; do
        echo "Apply ${cql_file}"
        docker run -it $RM_FLAG -e SSL_CERTFILE=/cacert.pem         \
	               $(mount_ro_volume "${SSLDIR}/ca/cacert.pem" "/cacert.pem")  \
                       $(mount_ro_volume "${cql_file}" "/cassandra.cql")           \
                       --link mongooseim-cassandra:cassandra        \
                       cassandra:${CASSANDRA_VERSION}               \
                       sh -c 'exec cqlsh "$CASSANDRA_PORT_9042_TCP_ADDR" --ssl -f /cassandra.cql'
    done

elif [ "$db" = 'elasticsearch' ]; then
    ELASTICSEARCH_IMAGE=docker.elastic.co/elasticsearch/elasticsearch:$ELASTICSEARCH_VERSION
    ELASTICSEARCH_PORT=9200
    ELASTICSEARCH_NAME=mongoooseim-elasticsearch

    echo $ELASTICSEARCH_IMAGE
    docker image pull $ELASTICSEARCH_IMAGE
    echo "Starting ElasticSearch $ELASTICSEARCH_VERSION from Docker container"
    docker run -d $RM_FLAG \
           -p $ELASTICSEARCH_PORT:9200 \
           -e "http.host=0.0.0.0" \
           -e "transport.host=127.0.0.1" \
           -e "xpack.security.enabled=false" \
           --name $ELASTICSEARCH_NAME \
           $ELASTICSEARCH_IMAGE
    echo "Waiting for ElasticSearch to start listening on port"
    tools/wait_for_service.sh $ELASTICSEARCH_NAME $ELASTICSEARCH_PORT || docker logs $ELASTICSEARCH_NAME

    ELASTICSEARCH_URL=http://localhost:$ELASTICSEARCH_PORT
    ELASTICSEARCH_PM_MAPPING="$(pwd)/priv/elasticsearch/pm.json"
    ELASTICSEARCH_MUC_MAPPING="$(pwd)/priv/elasticsearch/muc.json"
    echo "Putting ElasticSearch mappings"
    (curl -X PUT $ELASTICSEARCH_URL/messages -d "@$ELASTICSEARCH_PM_MAPPING" -w "status: %{http_code}" | grep "status: 200" > /dev/null) || \
        echo "Failed to put PM mapping into ElasticSearch"
    (curl -X PUT $ELASTICSEARCH_URL/muc_messages -d "@$ELASTICSEARCH_MUC_MAPPING" -w "status: %{http_code}" | grep "status: 200" > /dev/null) || \
        echo "Failed to put MUC mapping into ElasticSearch"

elif [ "$db" = 'mssql' ]; then
    # LICENSE STUFF, IMPORTANT
    #
    # SQL Server Developer edition
    # http://download.microsoft.com/download/4/F/7/4F7E81B0-7CEB-401D-BCFA-BF8BF73D868C/EULAs/License_Dev_Linux.rtf
    #
    # Information from that license:
    # > a. General.
    # > You may install and use copies of the software on any device,
    # > including third party shared devices, to design, develop, test and
    # > demonstrate your programs.
    # > You may not use the software on a device or server in a
    # > production environment.
    #
    # > We collect data about how you interact with this software.
    #   READ MORE...
    #
    # > BENCHMARK TESTING.
    # > You must obtain Microsoft's prior written approval to disclose to
    # > a third party the results of any benchmark test of the software.

    # SCRIPTING STUFF
    docker rm -f mongoose-mssql || echo "Skip removing previous container"
    docker volume rm -f mongoose-mssql-data || echo "Skip removing previous volume"
    #
    # MSSQL wants secure passwords
    # i.e. just "mongooseim_secret" would not work.
    #
    # We don't overwrite --entrypoint, but it's possible.
    # It has no '/docker-entrypoint-initdb.d/'-like interface.
    # So we would put schema into some random place and
    # apply it inside 'docker-exec' command.
    #
    # ABOUT VOLUMES
    # Just using /var/opt/mssql volume is not enough.
    # We need mssql-data-volume.
    #
    # Both on Mac and Linux
    # https://github.com/Microsoft/mssql-docker/issues/12
    #
    # Otherwise we get an error in logs
    # Error 87(The parameter is incorrect.) occurred while opening file '/var/opt/mssql/data/master.mdf'
    docker run -d -p 1433:1433                                  \
               --name=mongoose-mssql                            \
               -e "ACCEPT_EULA=Y"                               \
               -e "SA_PASSWORD=mongooseim_secret+ESL123"        \
               $(mount_ro_volume "$(pwd)/priv/mssql2012.sql" "/mongoose.sql")  \
               $(data_on_volume -v ${SQL_DATA_DIR}:/var/opt/mssql) \
               $(data_on_volume -v mongoose-mssql-data:/var/opt/mssql/data) \
               --health-cmd='/opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" -Q "SELECT 1"' \
               microsoft/mssql-server-linux
    tools/wait_for_healthcheck.sh mongoose-mssql
    tools/wait_for_service.sh mongoose-mssql 1433

    docker exec -it mongoose-mssql \
        /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" \
        -Q "CREATE DATABASE ejabberd"
    docker exec -it mongoose-mssql \
        /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" \
        -Q "ALTER DATABASE ejabberd SET READ_COMMITTED_SNAPSHOT ON"
    docker exec -it mongoose-mssql \
        /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" \
        -i mongoose.sql

    install_odbc_ini

elif [ "$db" = 'redis' ]; then
    tools/setup-redis.sh

elif [ "$db" = 'ldap' ]; then
    tools/travis-setup-ldap.sh

else
    echo "Skip setting up database"
fi
}

# The DB env var may contain single value "mysql"
# or list of values separated with a space "elasticsearch cassandra"
# in case of list of values all listed database will be set up (or at least tried)

for db in ${DB}; do
    setup_db $db
done
