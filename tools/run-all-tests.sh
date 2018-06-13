#!/usr/bin/env bash

./tools/configure with-all
export PRESET="internal_mnesia internal_redis pgsql_mnesia odbc_mssql_mnesia mysql_mnesia ldap_mnesia riak_mnesia cassandra_mnesia elasticsearch_mnesia"
export DB="mysql pgsql riak cassandra elasticsearch mssql"

make devrel
./tools/travis-build-tests.sh

./tools/setup-redis.sh
./tools/travis-setup-db.sh
./tools/travis-setup-ldap.sh

./tools/travis-tests.sh
