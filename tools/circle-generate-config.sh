#!/usr/bin/env bash

OUT_FILE="$1"

echo | base32 -w0 > /dev/null 2>&1
if [ $? -eq 0 ]; then
      # GNU coreutils base32, '-w' supported
      MYBASE64="base32 -w0"
    else
      # Openssl base32, no wrapping by default
      MYBASE64="base32"
fi

set -e

function cat32 {
    cat "$1" | $MYBASE64
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

MIM_CERT=$(cat32 tools/ssl/mongooseim/cert.pem)
MIM_KEY=$(cat32 tools/ssl/mongooseim/key.pem)
MIM_DHSERVER=$(cat32 tools/ssl/mongooseim/dh_server.pem)
INJECT_FILES=$(cat32 tools/inject-files.sh)
CACERT=$(cat32 tools/ssl/ca/cacert.pem)
LDAP_SETUP=$(cat32 tools/db_configs/ldap/init_script.sh)

YEAR_MONTH=$(cat year_month)
# This should be equal once certs cache is restored
CERTS_VERSION=$(tools/certs-version.sh)

sed -e "s/__MYSQL_CNF__/${MYSQL_CNF}/" \
    -e "s/__MYSQL_SQL__/${MYSQL_SQL}/" \
    -e "s/__MYSQL_SETUP__/${MYSQL_SETUP}/" \
    -e "s/__PGSQL_CNF__/${PGSQL_CNF}/" \
    -e "s/__PGSQL_SQL__/${PGSQL_SQL}/" \
    -e "s/__PGSQL_HBA__/${PGSQL_HBA}/" \
    -e "s/__PGSQL_SETUP__/${PGSQL_SETUP}/" \
    -e "s/__MSSQL_SQL__/${MSSQL_SQL}/" \
    -e "s/__MSSQL_SETUP__/${MSSQL_SETUP}/" \
    -e "s/__LDAP_SCHEMA__/${LDAP_SCHEMA}/" \
    -e "s/__MIM_CERT__/${MIM_CERT}/" \
    -e "s/__MIM_KEY__/${MIM_KEY}/" \
    -e "s/__MIM_DHSERVER__/${MIM_DHSERVER}/" \
    -e "s/__INJECT_FILES__/${INJECT_FILES}/" \
    -e "s/__DB_CACERT__/${CACERT}/" \
    -e "s/__LDAP_SETUP__/${LDAP_SETUP}/" \
    -e "s/__YEAR_MONTH__/${YEAR_MONTH}/" \
    -e "s/__CERTS_VERSION__/${CERTS_VERSION}/" \
    .circleci/template.yml \
    > "$OUT_FILE"
