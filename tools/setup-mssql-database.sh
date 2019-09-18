#!/usr/bin/env bash

set -e
source tools/travis-common-vars.sh

NAME=$(db_name mssql)
DB_NAME=${DB_NAME:-ejabberd}

echo "Container $NAME"

docker exec -t $NAME \
    /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" \
    -Q "CREATE DATABASE $DB_NAME"
docker exec -t $NAME \
    /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" \
    -Q "ALTER DATABASE $DB_NAME SET READ_COMMITTED_SNAPSHOT ON"
docker exec -t $NAME \
    /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P "mongooseim_secret+ESL123" \
    -d $DB_NAME \
    -i mongoose.sql
