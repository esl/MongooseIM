#!/usr/bin/env bash

MSSQL_PORT=${MSSQL_PORT:-1433}

# There is one odbc.ini for both mssql and pgsql
# Allows to run both in parallel

# CLIENT OS CONFIGURING STUFF
#
# Be aware, that underscore in TDS_Version is required.
# It can't be just "TDS Version = 7.1".
#
# To check that connection works use:
#
# {ok, Conn} = odbc:connect("DSN=mongoose-mssql;UID=sa;PWD=mongooseim_secret+ESL123",[]).
#
# To check that TDS version is correct, use:
#
# odbc:sql_query(Conn, "select cast(1 as bigint)").
#
# It should return:
# {selected,[[]],[{"1"}]}
#
# It should not return:
# {selected,[[]],[{1.0}]}
#
# Be aware, that Driver and Setup values are for Ubuntu.
# CentOS would use different ones.

echo "Install odbc.ini"

if test -f "/usr/local/lib/libtdsodbc.so"; then
  # Mac
  ODBC_DRIVER="/usr/local/lib/libtdsodbc.so"
fi

if test -f "/usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so"; then
  # Ubuntu
  ODBC_DRIVER="/usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so"
  ODBC_SETUP="/usr/lib/x86_64-linux-gnu/odbc/libtdsS.so"
fi

    cat > ~/.odbc.ini << EOL
[mongoose-mssql]
Setup       = $ODBC_SETUP
Driver      = $ODBC_DRIVER
Server      = 127.0.0.1
Port        = $MSSQL_PORT
Database    = mongooseim
Username    = sa
Password    = mongooseim_secret+ESL123
Charset     = UTF-8
TDS_Version = 7.2
client charset = UTF-8
server charset = UTF-8
EOL
