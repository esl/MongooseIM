#!/bin/sh
docker exec -it mongoose-mssql /opt/mssql-tools/bin/sqlcmd \
    -d ejabberd \
    -S localhost \
    -U SA \
    -P "mongooseim_secret+ESL123"
