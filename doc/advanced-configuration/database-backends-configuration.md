
MongooseIM can work with several databases, both SQL and NoSQL ones. Some of
them require extra work before they can be used. For example the SQL databases require
defining schema. MongooseIM is tested with TravisCI, so the travis scripts can be used
as a reference.

# MySQL

**Can be used for**:

* users (credentials)
* vcards
* roster
* private storage
* privacy lists
* last activity
* mam (message archive)

**Setup**

The schema files can be found in the `apps/ejabberd/priv` directory. The default
schema is defined in the `mysql.sql` file.

For example, you can use the following command to apply it on localhost:

```bash
mysql -h localhost -u user -p -e 'create database mongooseim'
mysql -h localhost -u user -p mongooseim < mysql.sql
```

You should also configure MySQL database in `ejabberd.cfg` file. 
Please refer to [Advanced configuration/Database setup](../Advanced-configuration.md) for more information.

# Postgres

**Can be used for:**

* users (credentials)
* vcards
* roster
* private storage
* privacy lists
* last activity
* mam (message archive)

**Setup**

The schema files can be found in the `apps/ejabberd/priv` directory. The default
schema is defined in the `pg.sql` file.

For example, you can use the following command to apply it on localhost:

```bash
psql -h localhost -U user -c "CREATE DATABASE mongooseim;"
psql -h localhost -U user -q -d mongooseim -f pg.sql
```
You should also configure Postgres database in `ejabberd.cfg` file. 
Please refer to [Advanced configuration/Database setup](../Advanced-configuration.md) for more information.

# MSSQL / Azure SQL

**Can be used for:**

* users (credentials)
* vcards
* roster
* private storage
* privacy lists
* last activity
* mam (message archive)

**Setup**

MSSQL can be used from Mongoose through the ODBC layer, so you need to
have it installed in your system. Moreover, an Erlang/OTP release must be
built with the support for ODBC as well as MongooseIM itself.

You can configure MongooseIM appropriately by using the following command: ``make configure with-odbc``.

You also need FreeTDS (an ODBC driver for MSSQL) installed in your
system.

Then you need to configure ODBC and FreeTDS drivers. You can find an
example configuration for CentOS, given that unixODBC and freetds packages have
been installed.

Add your database (``mongooseim`` here) to the ``/etc/odbc.ini`` file:
```ini
[mongoose-mssql]
Driver = FreeTDS
Servername = mssql-local
Database = mongooseim
```

Add path to the ``FreeTDS`` driver to the ``/etc/odbcinst.ini`` file:
```ini
[FreeTDS]
Description = TDS driver (Sybase/MS SQL)
Setup = /usr/lib64/libtdsS.so.2
Driver = /usr/lib64/libtdsodbc.so.0
UsageCount = 1
```
For more details please refer to the [odbc.ini and odbcinst.ini
documentation](http://www.unixodbc.org/odbcinst.html).

Add database host to the ``/etc/freetds.conf`` file:
```ini
[mssql-local]
    host = localhost
    port = 1433
    tds version = 8.0
    client charset = UTF-8
```
For more details please refer to the [freetds.conf
documentation](http://www.freetds.org/userguide/freetdsconf.htm).

Then you need to import the SQL schema from either ``mssql2012.sql`` or
``azuresql.sql`` file depending on which database you are using.
You can use a Microsoft's GUI tool (the provided .sql files should work
with it) or isql, but after a slight modification of the dump file:

```bash
cat azuresql.sql | tr -d '\r' | tr '\n' ' ' | sed 's/GO/\n/g' |
isql mongoose-mssql username password -b
```

The final step is to configure ``ejabberd.cfg`` appropriately.
Configure the database section as follows:
```erlang
{odbc_server, "DSN=mongoose-mssql;UID=username;PWD=password"}.
{odbc_server_type, mssql}.
```


# Riak (versions >=2.0)

**Can be used for:**

* users (credentials)
* private storage
* vCard and vCard search
* MAM (experimental feature for one-to-one archives)

**Setup**

We are using the riak data types, so the minimal supported version is 2.0.
To be able to store above persistent date one have to run the following command:

```bash
# user base
riak-admin bucket-type create users '{"props":{"datatype":"map"}}'
riak-admin bucket-type activate users

# private storage
riak-admin bucket-type create private '{"props":{"last_write_wins":true}}'
riak-admin bucket-type activate private

# vCard
RIAK_HOST="http://localhost:8098"

curl -XPUT $RIAK_HOST/search/schema/vcard \
    -H 'Content-Type:application/xml' \
    --data-binary @tools/vcard_search_schema.xml

curl -XPUT $RIAK_HOST/search/index/vcard \
    -H 'Content-Type: application/json' \
    -d '{"schema":"vcard"}'

riak-admin bucket-type create vcard '{"props":{"last_write_wins":true, "search_index":"vcard"}}'
riak-admin bucket-type activate vcard

#MAM
curl -XPUT $RIAK_HOST/search/schema/mam \
    -H 'Content-Type:application/xml' \
    --data-binary @tools/mam_search_schema.xml

curl -XPUT $RIAK_HOST/search/index/mam \
    -H 'Content-Type: application/json' \
    -d '{"schema":"mam"}'

riak-admin bucket-type create mam_yz '{"props":{"datatype":"map", "search_index":"mam"}}'
riak-admin bucket-type activate mam_yz

```

This will create backed types, search schemas and indexes required
for storing above persitent date and it will activate them.

You should also configure Riak in `ejabberd.cfg` file. 
Please refer to [Advanced configuration/Database setup](../Advanced-configuration.md) for more information.

# Cassandra

**Can be used for:**

* mam (Message archive)

**Setup**

The schema files can be found in the `apps/ejabberd/priv` directory. The default
schema is defined in the `cassandra.cql` file.

For example, you can use the following command to apply schema on localhost:

```
cqlsh localhost 9160 -f cassandra.cql
```

# Redis

**Can be used for:**

* users sessions

**Setup**

No additional steps required.

# LDAP

**Can be used for:**

* users (credentials)
* shared roster
* vcard

**Setup**

No additional steps required, the modules that are using LDAP are very customizable,
so they can be configured to support existsing schemas.

