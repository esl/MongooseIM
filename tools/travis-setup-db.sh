#!/usr/bin/env bash

# Environment variable DB is used by this script.
# If DB is undefined, than this script does nothing.

# Docker for Mac should be used on Mac (not docker-machine!)
# https://store.docker.com/editions/community/docker-ce-desktop-mac

set -e

source tools/travis-common-vars.sh

MIM_PRIV_DIR=${BASE}/priv

MYSQL_DIR=/etc/mysql/conf.d

PGSQL_ODBC_CERT_DIR=~/.postgresql

SSLDIR=${TOOLS}/ssl

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
Port        = $MSSQL_PORT
Database    = ejabberd
Username    = sa
Password    = mongooseim_secret+ESL123
Charset     = UTF-8
TDS_Version = 7.2
client_charset = UTF-8
EOL
}

function riak_solr_is_up
{
    docker exec $1 curl 'http://localhost:8093/internal_solr/mam/admin/ping?wt=json' | grep '"status":"OK"'
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
DB_CONF_DIR=${TOOLS}/db_configs/$db


if [ "$db" = 'mysql' ]; then
    NAME=$(db_name mysql)
    MYSQL_PORT=${MYSQL_PORT:-3306}
    echo "Configuring mysql"
    # TODO We should not use sudo
    sudo -n service mysql stop || echo "Failed to stop mysql"
    docker rm -v -f $NAME || echo "Skip removing previous container"
    cp ${SSLDIR}/mongooseim/cert.pem ${SQL_TEMP_DIR}/fake_cert.pem
    openssl rsa -in ${SSLDIR}/mongooseim/key.pem -out ${SQL_TEMP_DIR}/fake_key.pem
    chmod a+r ${SQL_TEMP_DIR}/fake_key.pem
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
    $(mount_ro_volume ${TOOLS}/docker-setup-mysql.sh /docker-entrypoint-initdb.d/docker-setup-mysql.sh) \
    $(mount_ro_volume ${SQL_TEMP_DIR} /tmp/sql) \
        $(data_on_volume -v ${SQL_DATA_DIR}:/var/lib/mysql) \
        --health-cmd='mysqladmin ping --silent' \
        -p $MYSQL_PORT:3306 --name=$NAME \
        mysql --default-authentication-plugin=mysql_native_password
    tools/wait_for_healthcheck.sh $NAME

elif [ "$db" = 'pgsql' ]; then
    NAME=$(db_name pgsql)
    PGSQL_PORT=${PGSQL_PORT:-5432}
    # If you see "certificate verify failed" error in Mongoose logs, try:
    # Inside tools/ssl/:
    # make clean && make
    # Than rerun the script to create a new docker container.
    echo "Configuring postgres with SSL"
    sudo -n service postgresql stop || echo "Failed to stop psql"
    docker rm -v -f $NAME || echo "Skip removing previous container"
    cp ${SSLDIR}/mongooseim/cert.pem ${SQL_TEMP_DIR}/fake_cert.pem
    cp ${SSLDIR}/mongooseim/key.pem ${SQL_TEMP_DIR}/fake_key.pem
    cp ${DB_CONF_DIR}/postgresql.conf ${SQL_TEMP_DIR}/.
    cp ${DB_CONF_DIR}/pg_hba.conf ${SQL_TEMP_DIR}/.
    cp ${MIM_PRIV_DIR}/pg.sql ${SQL_TEMP_DIR}/.
    docker run -d \
           -e SQL_TEMP_DIR=/tmp/sql \
           $(mount_ro_volume ${SQL_TEMP_DIR} /tmp/sql) \
           $(data_on_volume -v ${SQL_DATA_DIR}:/var/lib/postgresql/data) \
           $(mount_ro_volume ${TOOLS}/docker-setup-postgres.sh /docker-entrypoint-initdb.d/docker-setup-postgres.sh) \
           -p $PGSQL_PORT:5432 --name=$NAME postgres
    mkdir -p ${PGSQL_ODBC_CERT_DIR}
    cp ${SSLDIR}/ca/cacert.pem ${PGSQL_ODBC_CERT_DIR}/root.crt

elif [ "$db" = 'riak' ]; then
    NAME=$(db_name riak)
    # Expose for setup_riak script
    export RIAK_PORT=${RIAK_PORT:-8098}
    RIAK_PB_PORT=${RIAK_PB_PORT:-8087}
    echo "Configuring Riak with SSL"
    docker rm -v -f $NAME || echo "Skip removing previous container"
    # Instead of docker run, use "docker create" + "docker start".
    # So we can prepare our container.
    # We use HEALTHCHECK here, check "docker ps" to get healthcheck status.
    # We can't use volumes for riak.conf, because:
    # - we want to change it
    # - container starting code runs sed on it and gets IO error,
    #   if it's a volume
    time docker create -p $RIAK_PB_PORT:8087 -p $RIAK_PORT:8098 \
        -e DOCKER_RIAK_BACKEND=leveldb \
        -e DOCKER_RIAK_CLUSTER_SIZE=1 \
        --name=$NAME \
        $(mount_ro_volume "${DB_CONF_DIR}/advanced.config" "/etc/riak/advanced.config") \
        $(mount_ro_volume "${SSLDIR}/mongooseim/cert.pem" "/etc/riak/cert.pem") \
        $(mount_ro_volume "${SSLDIR}/mongooseim/key.pem" "/etc/riak/key.pem") \
        $(mount_ro_volume "${SSLDIR}/ca/cacert.pem" "/etc/riak/ca/cacertfile.pem") \
        $(mount_ro_volume "$TOOLS/setup_riak.escript" "/setup_riak.escript") \
        $(mount_ro_volume "$TOOLS/mam_search_schema.xml" "/mam_search_schema.xml") \
        $(mount_ro_volume "$TOOLS/vcard_search_schema.xml" "/vcard_search_schema.xml") \
        $(data_on_volume -v ${SQL_DATA_DIR}:/var/lib/riak) \
        --health-cmd='riak-admin status' \
        "michalwski/docker-riak:1.0.6" \
        /sbin/my_init --skip-startup-files
    # Use a temporary file to store config
    TEMP_RIAK_CONF=$(mktemp)
    # Export config from a container
    docker cp "$NAME:/etc/riak/riak.conf" "$TEMP_RIAK_CONF"
    # Enable search
    $SED -i "s/^search = \(.*\)/search = on/" "$TEMP_RIAK_CONF"
    # Solr is sloow on travis
    $SED -i "s/^search.solr.start_timeout = \(.*\)/search.solr.start_timeout = 2m/" "$TEMP_RIAK_CONF"
    # Enable ssl by appending settings from riak.conf.ssl
    cat "${DB_CONF_DIR}/riak.conf.ssl" >> "$TEMP_RIAK_CONF"
    # Import config back into container
    docker cp "$TEMP_RIAK_CONF" "$NAME:/etc/riak/riak.conf"
    # Erase temporary config file
    rm "$TEMP_RIAK_CONF"
    docker start $NAME
    echo "Waiting for docker healthcheck"
    echo ""
    tools/wait_for_healthcheck.sh $NAME
    echo "Waiting for a listener to appear"
    tools/wait_for_service.sh $NAME 8098
    time docker exec -e RIAK_PORT="$RIAK_PORT" $NAME riak escript /setup_riak.escript
    tools/wait_for_service.sh $NAME 8087
    # Use this command to read Riak's logs if something goes wrong
    # docker exec -t $NAME bash -c 'tail -f /var/log/riak/*'

    # Wait for solr
    for i in {1..30}; do
        if riak_solr_is_up $NAME; then
             break
        fi
        echo -n "."
        sleep 1
    done

elif [ "$db" = 'cassandra' ]; then
    NAME=$(db_name cassandra)
    PROXY_NAME=$(db_name cassandra-proxy)
    CASSANDRA_PROXY_API_PORT=${CASSANDRA_PROXY_API_PORT:-9191}
    CASSANDRA_PORT=${CASSANDRA_PORT:-9042}
    docker image pull cassandra:${CASSANDRA_VERSION}
    docker rm -v -f $NAME $PROXY_NAME || echo "Skip removing previous container"

    opts="$(docker inspect -f '{{range .Config.Entrypoint}}{{println}}{{.}}{{end}}' cassandra:${CASSANDRA_VERSION})"
    opts+="$(docker inspect -f '{{range .Config.Cmd}}{{println}}{{.}}{{end}}' cassandra:${CASSANDRA_VERSION})"
    while read -r line; do
        if [ ! -z "$line" ]; then
             init_opts+=("$line")
        fi
    done <<<"$opts"
    echo -e "cassandra startup cmd:\n\t${init_opts[@]}"

    docker_entry="${DB_CONF_DIR}/docker_entry.sh"

    MIM_SCHEMA=$(pwd)/priv/cassandra.cql
    TEST_SCHEMA=$(pwd)/big_tests/tests/mongoose_cassandra_SUITE_data/schema.cql

    docker run -d                                \
               -e MAX_HEAP_SIZE=128M             \
               -e HEAP_NEWSIZE=64M               \
               $(mount_ro_volume "${SSLDIR}" "/ssl") \
               $(mount_ro_volume "${docker_entry}" "/entry.sh") \
               $(mount_ro_volume "$MIM_SCHEMA" "/schemas/mim.cql") \
               $(mount_ro_volume "$TEST_SCHEMA" "/schemas/test.cql") \
               $(data_on_volume -v ${SQL_DATA_DIR}:/var/lib/cassandra) \
               --name=$NAME       \
               --entrypoint "/entry.sh"          \
               cassandra:${CASSANDRA_VERSION}    \
               "${init_opts[@]}"
    tools/wait_for_service.sh $NAME 9200 || docker logs $NAME

    # Start TCP proxy
    CASSANDRA_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $NAME)
    echo "Connecting TCP proxy to Cassandra on $CASSANDRA_IP..."
    cp ${DB_CONF_DIR}/proxy/zazkia-routes.json "$SQL_TEMP_DIR/"
    $SED -i "s/\"service-hostname\": \".*\"/\"service-hostname\": \"$CASSANDRA_IP\"/g" "$SQL_TEMP_DIR/zazkia-routes.json"
    docker run -d                               \
               -p $CASSANDRA_PORT:9042                   \
               -p $CASSANDRA_PROXY_API_PORT:9191         \
               $(mount_ro_volume "$SQL_TEMP_DIR" /data)  \
               --name=$PROXY_NAME \
               emicklei/zazkia
    tools/wait_for_service.sh $PROXY_NAME 9042 || docker logs $PROXY_NAME

    CQLSH_DEBUG=""
    if [ "${VERBOSE:-0}" = "1" ]; then
        CQLSH_DEBUG=" --debug "
    fi

    function cqlsh
    {
        docker exec \
        -e SSL_CERTFILE=/ssl/ca/cacert.pem \
        "$NAME" \
        cqlsh "127.0.0.1" --ssl $CQLSH_DEBUG "$@"
    }

    while ! cqlsh -e 'describe cluster' ; do
        echo "Waiting for cassandra"
        sleep 1
    done

    # Apply schemas
    echo "Apply Cassandra schema"
    # For some reason, "cqlsh -f" does not create schema and no error is reported.
    cqlsh -e "source '/schemas/mim.cql'"
    cqlsh -e "source '/schemas/test.cql'"
    echo "Verify Cassandra schema"
    # Would fail with reason and exit code 2:
    # <stdin>:1:InvalidRequest: Error from server: code=2200 [Invalid query] message="unconfigured table mam_config"
    cqlsh -e "select * from mongooseim.mam_config;"
    echo "Cassandra setup done"

elif [ "$db" = 'elasticsearch' ]; then
    ELASTICSEARCH_IMAGE=docker.elastic.co/elasticsearch/elasticsearch:$ELASTICSEARCH_VERSION
    ELASTICSEARCH_PORT=${ELASTICSEARCH_PORT:-9200}
    NAME=$(db_name elasticsearch)

    echo $ELASTICSEARCH_IMAGE
    docker image pull $ELASTICSEARCH_IMAGE
    docker rm -v -f $NAME || echo "Skip removing previous container"

    echo "Starting ElasticSearch $ELASTICSEARCH_VERSION from Docker container"
    docker run -d $RM_FLAG \
           -p $ELASTICSEARCH_PORT:9200 \
           -e "http.host=0.0.0.0" \
           -e "transport.host=127.0.0.1" \
           -e "xpack.security.enabled=false" \
           --name $NAME \
           $ELASTICSEARCH_IMAGE
    echo "Waiting for ElasticSearch to start listening on port"
    tools/wait_for_service.sh $NAME 9200 || docker logs $NAME

    ELASTICSEARCH_URL=http://localhost:$ELASTICSEARCH_PORT
    ELASTICSEARCH_PM_MAPPING="$(pwd)/priv/elasticsearch/pm.json"
    ELASTICSEARCH_MUC_MAPPING="$(pwd)/priv/elasticsearch/muc.json"
    echo "Putting ElasticSearch mappings"
    (curl -X PUT $ELASTICSEARCH_URL/messages -d "@$ELASTICSEARCH_PM_MAPPING" -w "status: %{http_code}" | grep "status: 200" > /dev/null) || \
        echo "Failed to put PM mapping into ElasticSearch"
    (curl -X PUT $ELASTICSEARCH_URL/muc_messages -d "@$ELASTICSEARCH_MUC_MAPPING" -w "status: %{http_code}" | grep "status: 200" > /dev/null) || \
        echo "Failed to put MUC mapping into ElasticSearch"

elif [ "$db" = 'mssql' ]; then
    NAME=$(db_name mssql)
    MSSQL_PORT=${MSSQL_PORT:-1433}
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
    docker rm -v -f $NAME || echo "Skip removing previous container"
    docker volume rm -f $NAME-data || echo "Skip removing previous volume"
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
    docker run -d -p $MSSQL_PORT:1433                           \
               --name=$NAME                                     \
               -e "ACCEPT_EULA=Y"                               \
               -e "SA_PASSWORD=mongooseim_secret+ESL123"        \
               $(mount_ro_volume "$(pwd)/priv/mssql2012.sql" "/mongoose.sql")  \
               $(data_on_volume -v ${SQL_DATA_DIR}:/var/opt/mssql) \
               $(data_on_volume -v $NAME-data:/var/opt/mssql/data) \
               --health-cmd='/opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" -Q "SELECT 1"' \
               microsoft/mssql-server-linux
    tools/wait_for_healthcheck.sh $NAME
    tools/wait_for_service.sh $NAME 1433

    tools/setup-mssql-database.sh

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
