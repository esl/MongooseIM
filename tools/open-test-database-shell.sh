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
    mssql-sqlcmd)
        $DOCKER exec -it mongooseim-mssql /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P mongooseim_secret+ESL123 -d mongooseim
    ;;
    mssql)
        $DOCKER run --link mongooseim-mssql -it --rm shellmaster/sql-cli mssql --server mongooseim-mssql --user sa --pass mongooseim_secret+ESL123 --database mongooseim
    ;;
    *)
        echo "Unknown argument $db"
        exit 1
esac
