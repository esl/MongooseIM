#!/usr/bin/env bash

# Environment variable DB is used by this script.
# If DB is undefined, than this script does nothing.

# Docker for Mac should be used on Mac (not docker-machine!)
# https://store.docker.com/editions/community/docker-ce-desktop-mac

set -e

# Switch to the repository directory
cd "$( dirname "${BASH_SOURCE[0]}" )/.."

source tools/common-vars.sh
source tools/parallel.sh
source tools/db-versions.sh

MIM_PRIV_DIR=${BASE}/priv

MYSQL_DIR=/etc/mysql/conf.d

PGSQL_ODBC_CERT_DIR=~/.postgresql

SSLDIR=${TOOLS}/ssl

PARALLEL_ENABLED=${PARALLEL_ENABLED-true}

function setup_db(){
    db=${1:-none}
    echo "Setting up db: $db"
    DB_CONF_DIR=${TOOLS}/db_configs/$db

    ENTRYPOINT=$(entrypoint)

    if [ "$db" = 'mysql' ]; then
        NAME=$(db_name mysql)
        MYSQL_PORT=${MYSQL_PORT:-3306}
        echo "Configuring mysql"
        # TODO We should not use sudo
        sudo -n service mysql stop || echo "Failed to stop mysql"
        $DOCKER rm -v -f $NAME || echo "Skip removing previous container"
        MYSQL_CNF=$(cat32 tools/db_configs/mysql/mysql.cnf)
        MYSQL_SQL=$(cat32 priv/mysql.sql)
        MYSQL_SETUP=$(cat32 tools/docker-setup-mysql.sh)
        MIM_CERT=$(cat32 tools/ssl/mongooseim/cert.pem)
        MIM_KEY=$(cat32 tools/ssl/mongooseim/key.pem)
        IMAGE=mysql:$MYSQL_VERSION
        $DOCKER run -d --name=$NAME \
            -p $MYSQL_PORT:3306 \
            -e SQL_TEMP_DIR=/tmp/sql \
            -e MYSQL_ROOT_PASSWORD=secret \
            -e MYSQL_DATABASE=mongooseim \
            -e MYSQL_USER=mongooseim \
            -e MYSQL_PASSWORD=mongooseim_secret \
            -e OLD_ENTRYPOINT="./entrypoint.sh mysqld" \
            -e ENV_FILE_CFG_PATH="/etc/mysql/conf.d/mysql.cnf" \
            -e ENV_FILE_CFG_DATA="$MYSQL_CNF" \
            -e ENV_FILE_SQL_PATH="/docker-entrypoint-initdb.d/mysql.sql" \
            -e ENV_FILE_SQL_DATA="$MYSQL_SQL" \
            -e ENV_FILE_SH_PATH="/docker-entrypoint-initdb.d/docker-setup-mysql.sh" \
            -e ENV_FILE_SH_DATA="$MYSQL_SETUP" \
            -e ENV_FILE_CERT_PATH="/tmp/sql/fake_cert.pem" \
            -e ENV_FILE_CERT_DATA="$MIM_CERT" \
            -e ENV_FILE_KEY_PATH="/tmp/sql/fake_key.pem" \
            -e ENV_FILE_KEY_DATA="$MIM_KEY" \
            --health-cmd='mysqladmin ping --silent' \
            --entrypoint=/bin/sh $IMAGE -c "$ENTRYPOINT"
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
        $DOCKER rm -v -f $NAME || echo "Skip removing previous container"
        PGSQL_CNF=$(cat32 tools/db_configs/pgsql/postgresql.conf)
        PGSQL_SQL=$(cat32 priv/pg.sql)
        PGSQL_HBA=$(cat32 tools/db_configs/pgsql/pg_hba.conf)
        PGSQL_SETUP=$(cat32 tools/docker-setup-postgres.sh)
        MIM_CERT=$(cat32 tools/ssl/mongooseim/cert.pem)
        MIM_KEY=$(cat32 tools/ssl/mongooseim/key.pem)
        IMAGE=postgres:$PGSQL_VERSION
        $DOCKER run -d --name=$NAME \
            -p $PGSQL_PORT:5432 \
            -e SQL_TEMP_DIR=/tmp/sql \
            -e POSTGRES_PASSWORD=password \
            -e OLD_ENTRYPOINT="docker-entrypoint.sh postgres" \
            -e ENV_FILE_CFG_PATH="/tmp/sql/postgresql.conf" \
            -e ENV_FILE_CFG_DATA="$PGSQL_CNF" \
            -e ENV_FILE_SQL_PATH="/tmp/sql/pg.sql" \
            -e ENV_FILE_SQL_DATA="$PGSQL_SQL" \
            -e ENV_FILE_HBA_PATH="/tmp/sql/pg_hba.conf" \
            -e ENV_FILE_HBA_DATA="$PGSQL_HBA" \
            -e ENV_FILE_SH_PATH="/docker-entrypoint-initdb.d/docker-setup-postgres.sh" \
            -e ENV_FILE_SH_DATA="$PGSQL_SETUP" \
            -e ENV_FILE_CERT_PATH="/tmp/sql/fake_cert.pem" \
            -e ENV_FILE_CERT_DATA="$MIM_CERT" \
            -e ENV_FILE_KEY_PATH="/tmp/sql/fake_key.pem" \
            -e ENV_FILE_KEY_DATA="$MIM_KEY" \
            --entrypoint=/bin/sh $IMAGE -c "$ENTRYPOINT"
        mkdir -p ${PGSQL_ODBC_CERT_DIR}
        cp ${SSLDIR}/ca/cacert.pem ${PGSQL_ODBC_CERT_DIR}/root.crt

    elif [ "$db" = 'cassandra' ]; then
        NAME=$(db_name cassandra)
        PROXY_NAME=$(db_name cassandra-proxy)
        CASSANDRA_PROXY_API_PORT=${CASSANDRA_PROXY_API_PORT:-9191}
        CASSANDRA_PORT=${CASSANDRA_PORT:-9142}
        $DOCKER rm -v -f $NAME $PROXY_NAME || echo "Skip removing previous container"

        SCHEMA_READY_PORT=9242

        CASSA_PROXY_CNF=$(cat32 tools/db_configs/cassandra/proxy/zazkia-routes.json)
        CASSA_ENTRY=$(cat32 tools/db_configs/cassandra/docker_entry.sh)
        CASSA_MIM_CQL_ENTRY=$(cat32 priv/cassandra.cql)
        CASSA_TEST_CQL_ENTRY=$(cat32 big_tests/tests/mongoose_cassandra_SUITE_data/schema.cql)
        CASSA_PROXY_REPLACE_IP=$(cat32 tools/db_configs/cassandra/proxy/replace-ip.sh)
        MIM_CERT=$(cat32 tools/ssl/mongooseim/cert.pem)
        MIM_KEY=$(cat32 tools/ssl/mongooseim/key.pem)
        CACERT=$(cat32 tools/ssl/ca/cacert.pem)

        IMAGE=cassandra:${CASSANDRA_VERSION}
        $DOCKER run -d --name=$NAME \
            -e SCHEMA_READY_PORT=$SCHEMA_READY_PORT \
            -e HEAP_NEWSIZE=64M \
            -e MAX_HEAP_SIZE=128M \
            -e OLD_ENTRYPOINT="/entry.sh" \
            -e ENV_FILE_CERT_PATH="/ssl/mongooseim/cert.pem" \
            -e ENV_FILE_CERT_DATA="$MIM_CERT" \
            -e ENV_FILE_KEY_PATH="/ssl/mongooseim/privkey.pem" \
            -e ENV_FILE_KEY_DATA="$MIM_KEY" \
            -e ENV_FILE_CACERT_PATH="/ssl/ca/cacert.pem" \
            -e ENV_FILE_CACERT_DATA="$CACERT" \
            -e ENV_FILE_CASSA_ENTRY_PATH="/entry.sh" \
            -e ENV_FILE_CASSA_ENTRY_DATA="$CASSA_ENTRY" \
            -e ENV_FILE_CASSA_ENTRY_MODE=755 \
            -e ENV_FILE_CASSA_MIM_CQL_PATH="/schemas/mim.cql" \
            -e ENV_FILE_CASSA_MIM_CQL_DATA="$CASSA_MIM_CQL_ENTRY" \
            -e ENV_FILE_CASSA_TEST_CQL_PATH="/schemas/test.cql" \
            -e ENV_FILE_CASSA_TEST_CQL_DATA="$CASSA_TEST_CQL_ENTRY" \
            -e BASE32DEC="$PYTHON2_BASE32_DEC" \
            --entrypoint=/bin/sh $IMAGE -c "$ENTRYPOINT"
        tools/wait_for_service.sh $NAME 9200 || $DOCKER logs $NAME

        # Start TCP proxy on 9142 port, instead of the default 9042
        CASSANDRA_IP=$($DOCKER inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $NAME)
        echo "Connecting TCP proxy to Cassandra on $CASSANDRA_IP..."

        IMAGE2=emicklei/zazkia
        # Circle CI version of this container does not need replace-ip.sh
        # (so, it does not define ENV_FILE_REPLACE_IP_PATH)
        $DOCKER run -d --name=$PROXY_NAME \
            -p $CASSANDRA_PORT:9142 \
            -p $CASSANDRA_PROXY_API_PORT:9191 \
            -e CASSANDRA_IP="$CASSANDRA_IP" \
            -e OLD_ENTRYPOINT="/replace-ip.sh && ./zazkia -v -f /data/zazkia-routes.json" \
            -e ENV_FILE_REPLACE_IP_PATH="/replace-ip.sh" \
            -e ENV_FILE_REPLACE_IP_DATA="$CASSA_PROXY_REPLACE_IP" \
            -e ENV_FILE_REPLACE_IP_MODE=755 \
            -e ENV_FILE_CFG_PATH="/data/zazkia-routes.json" \
            -e ENV_FILE_CFG_DATA="$CASSA_PROXY_CNF" \
            -e INSTALL_DEPS_CMD="apk update && apk add bash coreutils" \
            --entrypoint=/bin/sh $IMAGE2 -c "$ENTRYPOINT"
        tools/wait_for_service.sh $PROXY_NAME 9142 || $DOCKER logs $PROXY_NAME
        # Wait for ready schema
        tools/wait_for_service.sh $NAME $SCHEMA_READY_PORT || $DOCKER logs $NAME
        echo "Cassandra setup done"

    elif [ "$db" = 'elasticsearch' ]; then
        ELASTICSEARCH_IMAGE=docker.elastic.co/elasticsearch/elasticsearch:$ELASTICSEARCH_VERSION
        ELASTICSEARCH_PORT=${ELASTICSEARCH_PORT:-9200}
        NAME=$(db_name elasticsearch)
        $DOCKER rm -v -f $NAME || echo "Skip removing previous container"

        echo "Starting ElasticSearch $ELASTICSEARCH_VERSION from Docker container"
        $DOCKER run -d --name $NAME \
            -p $ELASTICSEARCH_PORT:9200 \
            -e ES_JAVA_OPTS="-Xms750m -Xmx750m" \
            -e "http.host=0.0.0.0" \
            -e "transport.host=127.0.0.1" \
            -e "xpack.security.enabled=false" \
            $ELASTICSEARCH_IMAGE
        echo "Waiting for ElasticSearch to start listening on port"
        tools/wait_for_service.sh $NAME 9200 || $DOCKER logs $NAME

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
        $DOCKER rm -v -f $NAME || echo "Skip removing previous container"
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
        MSSQL_SQL=$(cat32 priv/mssql2012.sql)
        MSSQL_SETUP=$(cat32 tools/docker-setup-mssql.sh)
        IMAGE=mcr.microsoft.com/mssql/server
        $DOCKER run -d --name=$NAME \
            -p $MSSQL_PORT:1433 \
            --user root \
            -e "ACCEPT_EULA=Y" \
            -e "SA_PASSWORD=mongooseim_secret+ESL123" \
            -e "DB_NAME=mongooseim" \
            -e "SQL_FILE=/tmp/mongoose.sql" \
            -e SCHEMA_READY_PORT=1434 \
            -e SQL_FILE="/tmp/mongoose.sql" \
            -e OLD_ENTRYPOINT='{ /tmp/docker-setup-mssql.sh& } && /opt/mssql/bin/sqlservr' \
            -e ENV_FILE_SQL_PATH="/tmp/mongoose.sql" \
            -e ENV_FILE_SQL_DATA="$MSSQL_SQL" \
            -e ENV_FILE_SH_PATH="/tmp/docker-setup-mssql.sh" \
            -e ENV_FILE_SH_DATA="$MSSQL_SETUP" \
            -e ENV_FILE_SH_MODE=755 \
            --health-cmd='/opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" -Q "SELECT 1"' \
            --entrypoint=/bin/sh $IMAGE -c "$ENTRYPOINT"
        tools/wait_for_healthcheck.sh $NAME
        tools/wait_for_service.sh $NAME 1433

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
        echo "setup_db(): ERROR, Unknown DB '${db}'"
        return 1
    fi
}

init_parallel setup_db

# The DB env var may contain single value "mysql"
# or list of values separated with a space "elasticsearch cassandra"
# in case of list of values all listed database will be set up (or at least tried)

for db in ${DB}; do
    parallel "$db" setup_db "$db"
done

wait_for_parallel setup_db
