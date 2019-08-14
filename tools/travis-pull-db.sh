#!/usr/bin/env bash

# Environment variable DB is used by this script.
# If DB is undefined, then this script does nothing.

# Docker for Mac should be used on Mac (not docker-machine!)
# https://store.docker.com/editions/community/docker-ce-desktop-mac

set -e
TOOLS=`dirname $0`

source tools/travis-common-vars.sh

# Default cassandra version
CASSANDRA_VERSION=${CASSANDRA_VERSION:-3.9}

# Default ElasticSearch version
ELASTICSEARCH_VERSION=${ELASTICSEARCH_VERSION:-5.6.9}

function setup_db(){
db=${1:-none}
echo "Pulling up db: $db"

if [ "$db" = 'mysql' ]; then
    docker image pull mysql

elif [ "$db" = 'pgsql' ]; then
    docker image pull postgres

elif [ "$db" = 'riak' ]; then
    docker image pull "michalwski/docker-riak:1.0.6"

elif [ "$db" = 'cassandra' ]; then
    docker image pull cassandra:${CASSANDRA_VERSION}
    docker image pull emicklei/zazkia

elif [ "$db" = 'elasticsearch' ]; then
    docker image pull docker.elastic.co/elasticsearch/elasticsearch:$ELASTICSEARCH_VERSION

elif [ "$db" = 'mssql' ]; then
    docker image pull microsoft/mssql-server-linux

elif [ "$db" = 'redis' ]; then
    docker image pull redis

elif [ "$db" = 'ldap' ]; then
    docker image pull openfrontier/openldap-server

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
