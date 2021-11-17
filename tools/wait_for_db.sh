#!/usr/bin/env bash

DBS=$(./tools/test_runner/presets_to_dbs.sh "$PRESET")
echo "Wait for $DBS"

function wait_for_db {
    case $1 in
        mysql)
            ./tools/wait-for-it.sh -p 3306
        ;;

        pgsql)
            ./tools/wait-for-it.sh -p 5432
        ;;

        mssql)
            ./tools/wait-for-it.sh -p 1433
            ./tools/wait-for-it.sh -p 1434 # SCHEMA_READY_PORT
        ;;

        rmq)
            ./tools/wait-for-it.sh -p 5672
        ;;

        redis)
            ./tools/wait-for-it.sh -p 6379
        ;;

        riak)
            ./tools/wait-for-it.sh -p 8098
            ./tools/wait-for-it.sh -p 8087
        ;;
    esac
}

for db in ${DBS}; do
    wait_for_db $db
done
