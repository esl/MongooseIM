#!/usr/bin/env bash

# Environment variable DB is used by this script.
# If DB is undefined, than this script does nothing.

# Docker for Mac should be used on Mac (not docker-machine!)
# https://store.docker.com/editions/community/docker-ce-desktop-mac

set -e

source tools/common-vars.sh
source tools/db-versions.sh

MIM_PRIV_DIR=${BASE}/priv

MYSQL_DIR=/etc/mysql/conf.d

PGSQL_ODBC_CERT_DIR=~/.postgresql

SSLDIR=${TOOLS}/ssl

# Don't need it for CI for speed up
RM_FLAG=" --rm "
if [ "$CI" = 'true' ]; then
    echo "Disable --rm flag on CI"
    RM_FLAG=""
fi

# DATA_ON_VOLUME variable and data_on_volume function come from common-vars.sh
echo "DATA_ON_VOLUME is $DATA_ON_VOLUME"

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
        mysql:$MYSQL_VERSION
    tools/wait_for_healthcheck.sh $NAME

elif [ "$db" = 'pgsql' ]; then
    NAME=$(db_name pgsql)
    PGSQL_PORT=${PGSQL_PORT:-5432}
    # If you see "certificate verify failed" error in Mongoose logs, try:
    # Inside tools/ssl/:
    # make clean && make
    # Than rerun the script to create a new docker container.
    echo "Configuring postgres with SSL"
    sudo -n service postgresql stop || echo "Failed to stop pgsql"
    docker rm -v -f $NAME || echo "Skip removing previous container"
    cp ${SSLDIR}/mongooseim/cert.pem ${SQL_TEMP_DIR}/fake_cert.pem
    cp ${SSLDIR}/mongooseim/key.pem ${SQL_TEMP_DIR}/fake_key.pem
    cp ${DB_CONF_DIR}/postgresql.conf ${SQL_TEMP_DIR}/.
    cp ${DB_CONF_DIR}/pg_hba.conf ${SQL_TEMP_DIR}/.
    cp ${MIM_PRIV_DIR}/pg.sql ${SQL_TEMP_DIR}/.
    docker run -d \
           -e SQL_TEMP_DIR=/tmp/sql \
           -e POSTGRES_PASSWORD=password \
           $(mount_ro_volume ${SQL_TEMP_DIR} /tmp/sql) \
           $(data_on_volume -v ${SQL_DATA_DIR}:/var/lib/postgresql/data) \
           $(mount_ro_volume ${TOOLS}/docker-setup-postgres.sh /docker-entrypoint-initdb.d/docker-setup-postgres.sh) \
           -p $PGSQL_PORT:5432 --name=$NAME postgres:$PGSQL_VERSION
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
        -e DOCKER_RIAK_BACKEND=memory \
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
    # Solr is sloow on CI
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
    CASSANDRA_PORT=${CASSANDRA_PORT:-9142}
    docker rm -v -f $NAME $PROXY_NAME || echo "Skip removing previous container"

    MIM_SCHEMA=$(pwd)/priv/cassandra.cql
    TEST_SCHEMA=$(pwd)/big_tests/tests/mongoose_cassandra_SUITE_data/schema.cql
    SCHEMA_READY_PORT=9242

    docker run -d                                \
               -e MAX_HEAP_SIZE=128M             \
               -e HEAP_NEWSIZE=64M               \
               -e SCHEMA_READY_PORT=$SCHEMA_READY_PORT \
               $(mount_ro_volume "${SSLDIR}/ca/cacert.pem" "/ssl/ca/cacert.pem") \
               $(mount_ro_volume "${SSLDIR}/mongooseim/cert.pem" "/ssl/mongooseim/cert.pem") \
               $(mount_ro_volume "${SSLDIR}/mongooseim/privkey.pem" "/ssl/mongooseim/privkey.pem") \
               $(mount_ro_volume "${DB_CONF_DIR}/docker_entry.sh" "/entry.sh") \
               $(mount_ro_volume "$MIM_SCHEMA" "/schemas/mim.cql") \
               $(mount_ro_volume "$TEST_SCHEMA" "/schemas/test.cql") \
               $(data_on_volume -v ${SQL_DATA_DIR}:/var/lib/cassandra) \
               --name=$NAME \
               --entrypoint "/entry.sh" \
               cassandra:${CASSANDRA_VERSION}
    tools/wait_for_service.sh $NAME 9200 || docker logs $NAME

    # Start TCP proxy on 9142 port, instead of the default 9042
    CASSANDRA_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $NAME)
    echo "Connecting TCP proxy to Cassandra on $CASSANDRA_IP..."
    cp ${DB_CONF_DIR}/proxy/zazkia-routes.json "$SQL_TEMP_DIR/"
    $SED -i "s/\"service-hostname\": \".*\"/\"service-hostname\": \"$CASSANDRA_IP\"/g" "$SQL_TEMP_DIR/zazkia-routes.json"
    docker run -d                               \
               -p $CASSANDRA_PORT:9142                   \
               -p $CASSANDRA_PROXY_API_PORT:9191         \
               $(mount_ro_volume "$SQL_TEMP_DIR" /data)  \
               --name=$PROXY_NAME \
               emicklei/zazkia
    tools/wait_for_service.sh $PROXY_NAME 9142 || docker logs $PROXY_NAME
    # Wait for ready schema
    tools/wait_for_service.sh $NAME $SCHEMA_READY_PORT || docker logs $NAME
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
           -e ES_JAVA_OPTS="-Xms750m -Xmx750m" \
           -e "http.host=0.0.0.0" \
           -e "transport.host=127.0.0.1" \
           -e "xpack.security.enabled=false" \
           --name $NAME \
           $ELASTICSEARCH_IMAGE
    echo "Waiting for ElasticSearch to start listening on port"
    tools/wait_for_service.sh $NAME 9200 || docker logs $NAME

    ELASTICSEARCH_PORT=$ELASTICSEARCH_PORT ./tools/setup-elasticsearch.sh

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
               --user root                                      \
               --name=$NAME                                     \
               -e "ACCEPT_EULA=Y"                               \
               -e "SA_PASSWORD=mongooseim_secret+ESL123"        \
               -e "DB_NAME=ejabberd"                            \
               -e "SQL_FILE=/tmp/mongoose.sql"                  \
               $(mount_ro_volume "$(pwd)/priv/mssql2012.sql" "/tmp/mongoose.sql")  \
               $(mount_ro_volume "$(pwd)/tools/docker-setup-mssql.sh" "/tmp/docker-setup-mssql.sh")  \
               $(data_on_volume -v ${SQL_DATA_DIR}:/var/opt/mssql) \
               $(data_on_volume -v $NAME-data:/var/opt/mssql/data) \
               --health-cmd='/opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" -Q "SELECT 1"' \
               mcr.microsoft.com/mssql/server
    tools/wait_for_healthcheck.sh $NAME
    tools/wait_for_service.sh $NAME 1433
    docker exec $NAME /tmp/docker-setup-mssql.sh

    MSSQL_PORT=$MSSQL_PORT tools/install_odbc_ini.sh

elif [ "$db" = 'redis' ]; then
    tools/setup-redis.sh

elif [ "$db" = 'ldap' ]; then
    tools/setup-ldap.sh

elif [ "$db" = "minio" ]; then
    tools/setup_minio.sh

elif [ "$db" = "rmq" ]; then
    tools/setup-rmq.sh

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
