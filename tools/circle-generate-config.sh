#!/usr/bin/env bash

OUT_FILE="$1"

set -euo pipefail

source tools/common-vars.sh
source tools/db-versions.sh

MYSQL_CNF=$(cat32 tools/db_configs/mysql/mysql.cnf)
MYSQL_SQL=$(cat32 priv/mysql.sql)
MYSQL_SETUP=$(cat32 tools/docker-setup-mysql.sh)

PGSQL_CNF=$(cat32 tools/db_configs/pgsql/postgresql.conf)
PGSQL_SQL=$(cat32 priv/pg.sql)
PGSQL_HBA=$(cat32 tools/db_configs/pgsql/pg_hba.conf)
PGSQL_SETUP=$(cat32 tools/docker-setup-postgres.sh)

COCKROACH_SQL=$(cat32 priv/cockroachdb.sql)
COCKROACH_USER_SQL=$(cat32 tools/db_configs/cockroachdb/create_user.sql)
COCKROACH_SETUP=$(cat32 tools/docker-setup-cockroachdb.sh)

LDAP_SCHEMA=$(cat32 tools/db_configs/ldap/init_entries.ldif)
LDAP_SETUP=$(cat32 tools/db_configs/ldap/init_script.sh)

REDIS_ACL=$(cat32 tools/redis.acl)

CASSA_PROXY_CNF=$(cat32 tools/db_configs/cassandra/proxy/zazkia-routes.json)
CASSA_ENTRY=$(cat32 tools/db_configs/cassandra/docker_entry.sh)
CASSA_MIM_CQL_ENTRY=$(cat32 priv/cassandra.cql)
CASSA_TEST_CQL_ENTRY=$(cat32 big_tests/tests/mongoose_cassandra_SUITE_data/schema.cql)

RMQ_TLS_CONFIG=$(cat32 tools/db_configs/rmq/20-tls.conf)

MIM_CERT=$(cat32 tools/ssl/mongooseim/cert.pem)
MIM_KEY=$(cat32 tools/ssl/mongooseim/key.pem)
MIM_PRIV_KEY=$(cat32 tools/ssl/mongooseim/privkey.pem)
MIM_DHSERVER=$(cat32 tools/ssl/mongooseim/dh_server.pem)
INJECT_FILES=$(cat32 tools/inject-files.sh)
CACERT=$(cat32 tools/ssl/ca/cacert.pem)
CAKEY=$(cat32 tools/ssl/ca/cakey.pem)

CERTS_CACHE_KEY=$(cat certs_cache_key)

# Matches plugins list in the rebar.config
REBAR_PLUGINS_HASH=$(cat rebar.config | sed -n '/^{plugins/,/]}./p' | sha1sum | awk '{print $1}')

BIG_TESTS="$(./tools/circle-generate-presets .circleci/presets.config | "$SED" 's/^/      /')"
BIG_TESTS_LIST="$(awk '$1 == "name:" {print "-", $2}' <<< "$BIG_TESTS" | "$SED" 's/^/            /')"

env \
  __MYSQL_CNF__="$MYSQL_CNF" \
  __MYSQL_SQL__="$MYSQL_SQL" \
  __MYSQL_SETUP__="$MYSQL_SETUP" \
  __MYSQL_VERSION__="$MYSQL_VERSION" \
  __PGSQL_CNF__="$PGSQL_CNF" \
  __PGSQL_SQL__="$PGSQL_SQL" \
  __PGSQL_HBA__="$PGSQL_HBA" \
  __PGSQL_SETUP__="$PGSQL_SETUP" \
  __PGSQL_VERSION__="$PGSQL_VERSION" \
  __COCKROACHDB_USER_SQL__="$COCKROACH_USER_SQL" \
  __COCKROACHDB_SQL__="$COCKROACH_SQL" \
  __COCKROACHDB_SETUP__="$COCKROACH_SETUP" \
  __COCKROACHDB_VERSION__="$COCKROACHDB_VERSION" \
  __REDIS_VERSION__="$REDIS_VERSION" \
  __LDAP_SCHEMA__="$LDAP_SCHEMA" \
  __LDAP_SETUP__="$LDAP_SETUP" \
  __LDAP_VERSION__="$LDAP_VERSION" \
  __REDIS_ACL__="$REDIS_ACL" \
  __CASSA_PROXY_CNF__="$CASSA_PROXY_CNF" \
  __CASSA_ENTRY__="$CASSA_ENTRY" \
  __CASSA_MIM_SQL__="$CASSA_MIM_CQL_ENTRY" \
  __CASSA_TEST_SQL__="$CASSA_TEST_CQL_ENTRY" \
  __CASSA_VERSION__="$CASSANDRA_VERSION" \
  __ELASTICSEARCH_VERSION__="$ELASTICSEARCH_VERSION" \
  __RMQ_VERSION__="$RMQ_VERSION" \
  __RMQ_TLS_CONFIG__="$RMQ_TLS_CONFIG" \
  __MINIO_VERSION__="$MINIO_VERSION" \
  __MINIO_MC_VERSION__="$MINIO_MC_VERSION" \
  __MIM_CERT__="$MIM_CERT" \
  __MIM_KEY__="$MIM_KEY" \
  __MIM_PRIV_KEY__="$MIM_PRIV_KEY" \
  __MIM_DHSERVER__="$MIM_DHSERVER" \
  __INJECT_FILES__="$INJECT_FILES" \
  __DB_CACERT__="$CACERT" \
  __DB_CAKEY__="$CAKEY" \
  __PYTHON2_BASE32_DEC__="$PYTHON2_BASE32_DEC" \
  __PYTHON3_BASE32_DEC__="$PYTHON3_BASE32_DEC" \
  __CERTS_CACHE_KEY__="$CERTS_CACHE_KEY" \
  __REBAR_PLUGINS_HASH__="$REBAR_PLUGINS_HASH" \
  __BIG_TESTS__=$'\n'"$BIG_TESTS" \
  __BIG_TESTS_LIST__=$'\n'"$BIG_TESTS_LIST" \
  awk '
    BEGIN {
      for (k in ENVIRON) {
        if (k ~ /^__/) { repl[k] = ENVIRON[k] }
      }
    }
    {
      for (k in repl) { gsub(k, repl[k]) }
      print
    }
  ' .circleci/template.yml \
  > "$OUT_FILE"
