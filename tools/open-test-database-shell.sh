#!/usr/bin/env bash

db=$1

case $db in
    mysql)
        docker exec -e MYSQL_PWD=secret -it mongooseim-mysql mysql -h localhost -u root -D ejabberd
    ;;
    pgsql)
        docker exec -e PGPASSWORD=password -it mongooseim-pgsql psql -U postgres -d ejabberd -h 127.0.0.1
    ;;
    mssql)
        docker exec -it mongooseim-mssql /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P mongooseim_secret+ESL123 -d ejabberd
    ;;
    *)
        echo "Unknown argument $db"
        exit 1
esac
