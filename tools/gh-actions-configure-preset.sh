#!/bin/bash
## GITHUB_ENV=/dev/tty ./gh-actions-configure-preset.sh internal-mnesia

DB_ARRAY=( $(./tools/test_runner/presets_to_dbs.sh "$1" ) )
[ "${#DB_ARRAY[@]}" -gt 0 ] && export DB="${DB_ARRAY[@]}"

case "$1" in
  internal_mnesia)
    export REL_CONFIG="with-all" TLS_DIST=true ;;
  odbc_mssql_mnesia)
    export REL_CONFIG="with-odbc" ;;
  mysql_redis)
    export REL_CONFIG="with-mysql with-redis with-amqp_client" ;;
  pgsql_mnesia)
    export REL_CONFIG="with-pgsql" ;;
  riak_mnesia)
    export REL_CONFIG="with-riak" ;;
  ldap_mnesia)
    export REL_CONFIG="with-none" ;;
  elasticsearch_and_cassandra_mnesia)
    export REL_CONFIG="with-elasticsearch with-cassandra" TESTSPEC=mam.spec
esac

if [ ! -z "$GITHUB_ENV" ]; then
  env | grep -E "^(DB|REL_CONFIG|TLS_DIST|TESTSPEC)=" >> $GITHUB_ENV
fi
