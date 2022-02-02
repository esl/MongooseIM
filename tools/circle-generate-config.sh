#!/usr/bin/env bash

OUT_FILE="$1"

echo | base32 -w0 > /dev/null 2>&1
if [ $? -eq 0 ]; then
      # GNU coreutils base32, '-w' supported
      ENCODER="base32 -w0"
    else
      # Openssl base32, no wrapping by default
      ENCODER="base32"
fi

set -e
source tools/db-versions.sh

function cat32 {
    cat "$1" | $ENCODER
}

MYSQL_CNF=$(cat32 tools/db_configs/mysql/mysql.cnf)
MYSQL_SQL=$(cat32 priv/mysql.sql)
MYSQL_SETUP=$(cat32 tools/docker-setup-mysql.sh)

PGSQL_CNF=$(cat32 tools/db_configs/pgsql/postgresql.conf)
PGSQL_SQL=$(cat32 priv/pg.sql)
PGSQL_HBA=$(cat32 tools/db_configs/pgsql/pg_hba.conf)
PGSQL_SETUP=$(cat32 tools/docker-setup-postgres.sh)

MSSQL_SQL=$(cat32 priv/mssql2012.sql)
MSSQL_SETUP=$(cat32 tools/docker-setup-mssql.sh)

LDAP_SCHEMA=$(cat32 tools/db_configs/ldap/init_entries.ldif)
LDAP_SETUP=$(cat32 tools/db_configs/ldap/init_script.sh)

RIAK_SSL_CFG=$(cat32 tools/db_configs/riak/riak.conf.ssl)
RIAK_ADV_CFG=$(cat32 tools/db_configs/riak/advanced.config)
RIAK_SETUP=$(cat32 tools/setup_riak.escript)
RIAK_MAM_SEARCH_SCHEMA=$(cat32 tools/mam_search_schema.xml)
RIAK_VCARD_SEARCH_SCHEMA=$(cat32 tools/vcard_search_schema.xml)
RIAK_SETUP_SH=$(cat32 tools/db_configs/riak/setup-riak.sh)

CASSA_PROXY_CNF=$(cat32 tools/db_configs/cassandra/proxy/zazkia-routes.json)
CASSA_ENTRY=$(cat32 tools/db_configs/cassandra/docker_entry.sh)
CASSA_MIM_CQL_ENTRY=$(cat32 priv/cassandra.cql)
CASSA_TEST_CQL_ENTRY=$(cat32 big_tests/tests/mongoose_cassandra_SUITE_data/schema.cql)

MIM_CERT=$(cat32 tools/ssl/mongooseim/cert.pem)
MIM_KEY=$(cat32 tools/ssl/mongooseim/key.pem)
MIM_PRIV_KEY=$(cat32 tools/ssl/mongooseim/privkey.pem)
MIM_DHSERVER=$(cat32 tools/ssl/mongooseim/dh_server.pem)
INJECT_FILES=$(cat32 tools/inject-files.sh)
CACERT=$(cat32 tools/ssl/ca/cacert.pem)

PYTHON2_BASE32_DEC="python2 -c 'import base64; import sys; sys.stdout.write(base64.b32decode(sys.stdin.readline().strip()))'"
PYTHON3_BASE32_DEC="python3 -c 'import base64; import sys; sys.stdout.buffer.write(base64.b32decode(sys.stdin.readline().strip()))'"

CERTS_CACHE_KEY=$(cat certs_cache_key)

sed -e "s/__MYSQL_CNF__/${MYSQL_CNF}/" \
    -e "s/__MYSQL_SQL__/${MYSQL_SQL}/" \
    -e "s/__MYSQL_SETUP__/${MYSQL_SETUP}/" \
    -e "s/__MYSQL_VERSION__/${MYSQL_VERSION}/" \
    -e "s/__PGSQL_CNF__/${PGSQL_CNF}/" \
    -e "s/__PGSQL_SQL__/${PGSQL_SQL}/" \
    -e "s/__PGSQL_HBA__/${PGSQL_HBA}/" \
    -e "s/__PGSQL_SETUP__/${PGSQL_SETUP}/" \
    -e "s/__PGSQL_VERSION__/${PGSQL_VERSION}/g" \
    -e "s/__MSSQL_SQL__/${MSSQL_SQL}/" \
    -e "s/__MSSQL_SETUP__/${MSSQL_SETUP}/" \
    -e "s/__REDIS_VERSION__/${REDIS_VERSION}/" \
    -e "s/__LDAP_SCHEMA__/${LDAP_SCHEMA}/" \
    -e "s/__LDAP_SETUP__/${LDAP_SETUP}/" \
    -e "s/__LDAP_VERSION__/${LDAP_VERSION}/" \
    -e "s/__RIAK_SSL_CFG__/${RIAK_SSL_CFG}/" \
    -e "s/__RIAK_ADV_CFG__/${RIAK_ADV_CFG}/" \
    -e "s/__RIAK_SETUP__/${RIAK_SETUP}/" \
    -e "s/__RIAK_SETUP_SH__/${RIAK_SETUP_SH}/" \
    -e "s/__RIAK_MAM_SEARCH_SCHEMA__/${RIAK_MAM_SEARCH_SCHEMA}/" \
    -e "s/__RIAK_VCARD_SEARCH_SCHEMA__/${RIAK_VCARD_SEARCH_SCHEMA}/" \
    -e "s/__CASSA_PROXY_CNF__/${CASSA_PROXY_CNF}/" \
    -e "s/__CASSA_ENTRY__/${CASSA_ENTRY}/" \
    -e "s/__CASSA_MIM_SQL__/${CASSA_MIM_CQL_ENTRY}/" \
    -e "s/__CASSA_TEST_SQL__/${CASSA_TEST_CQL_ENTRY}/" \
    -e "s/__CASSA_VERSION__/${CASSANDRA_VERSION}/" \
    -e "s/__ELASTICSEARCH_VERSION__/${ELASTICSEARCH_VERSION}/" \
    -e "s/__RMQ_VERSION__/${RMQ_VERSION}/" \
    -e "s/__MINIO_VERSION__/${MINIO_VERSION}/" \
    -e "s/__MINIO_MC_VERSION__/${MINIO_MC_VERSION}/" \
    -e "s/__MIM_CERT__/${MIM_CERT}/" \
    -e "s/__MIM_KEY__/${MIM_KEY}/" \
    -e "s/__MIM_PRIV_KEY__/${MIM_PRIV_KEY}/" \
    -e "s/__MIM_DHSERVER__/${MIM_DHSERVER}/" \
    -e "s/__INJECT_FILES__/${INJECT_FILES}/" \
    -e "s/__DB_CACERT__/${CACERT}/" \
    -e "s/__PYTHON2_BASE32_DEC__/${PYTHON2_BASE32_DEC}/" \
    -e "s/__PYTHON3_BASE32_DEC__/${PYTHON3_BASE32_DEC}/" \
    -e "s/__CERTS_CACHE_KEY__/${CERTS_CACHE_KEY}/" \
    .circleci/template.yml \
    > "$OUT_FILE"
