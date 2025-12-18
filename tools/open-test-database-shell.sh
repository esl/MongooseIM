#!/usr/bin/env bash

cd "$(dirname "$0")/../"
source tools/common-vars.sh
db=$1

case $db in
    mysql)
        $DOCKER exec -e MYSQL_PWD=secret -it mongooseim-mysql mysql -h localhost -u root -D mongooseim
    ;;
    pgsql)
        $DOCKER exec -e PGPASSWORD=password -it mongooseim-pgsql psql -U postgres -d mongooseim -h 127.0.0.1
    ;;
    cockroachdb)
       $DOCKER exec -it mongooseim-cockroachdb cockroach sql --certs-dir=/cockroach/certs --host=127.0.0.1 --user=mongooseim
    ;;
    *)
        echo "Unknown argument $db"
        exit 1
esac
