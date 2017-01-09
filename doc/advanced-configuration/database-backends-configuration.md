# Database Backends

MongooseIM can work with several databases, both RDBMS (SQL) and NOSQL ones. Some of
them require extra work before they can be used. For example the SQL databases require
defining schema. MongooseIM is tested with TravisCI, so the travis scripts can be used
as a reference.

# A Brief Overview

Data in MongooseIM is either transient or persistent:

* **transient**: volatile data changing often, such as session data, stream management data, and other in-memory data. These don't need any backup, since after a potential failure, they will naturally rebuild as clients reconnect.
* **persistent**: long-lived data, such as roster items, credentials, and chat archives. These absolutely need regular and tested backups.

# Choosing a database for MongooseIM

Here is some general advice on the use of databases.
Subsequent sections go into more depth on each database: what they are suitable for and how to set them up.

Transient data:

* Mnesia - as a highly available and distributed database we highly recommend Mnesia (over Redis) for storing **transient** data.
Being an Erlang-based database, it's the default persistance option for most modules in MongooseIM.
**Warning**: we **strongly recommend** keeping **persistent** data in an external DB (RDBMS or Riak) for production.
Mnesia is not suitable for the volumes of **persistent** data which some modules may require.
Sooner or later a migration will be needed which may be painful.
It is possible to store all data in Mnesia, but only for playing purposes, not for any serious use.

* Redis - A fantastic choice for storing live data. It's highly scalable and it can be
easily shared by multiple MongooseIM nodes. Additionally, Redis' great performance make it an excellent choice for
storing `users session` data. Let's speed up. We recommend caution, since it has not yet been widely tested in production.


Persistent Data:

* RDBMS/ODBC - MongooseIM has a strong backend support for relational databases. Considering the [CAP theorem](https://en.wikipedia.org/wiki/CAP_theorem)
they usually guarantees both availability and consistency which is a great choice for regular MongooseIM use cases and features
 like `privacy lists`, `vcards`, `roster`, `private storage`, `last activity` and `message archive`. Never loose your data.

* Riak KV - If you're planning to deploy a massive cluster then consider Riak KV as a potential storage backend solution.
It offers high availability and fault tolerance which is excatly what you need for your distributed MongooseIM architecture.
Use Riak KV with `privacy lists`, `vcards`, `roster`, `private storage`, `last activity` and `message archive`.
Erlang Solutions commercially supports Riak KV.

# RDBMS/ODBC

## MySQL

**Can be used for**:

* users (credentials)
* vcards
* roster
* private storage
* privacy/block lists
* last activity
* mam (message archive management)

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

## PostgreSQL

**Can be used for:**

* users (credentials)
* vcards
* roster
* private storage
* privacy/block lists
* last activity
* mam (message archive management)

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

## Microsoft SQL Server

Microsoft SQL Server, sometimes called MSSQL, or Azure SQL Database.

**Can be used for:**

* users (credentials)
* vcards
* roster
* private storage
* privacy/block lists
* last activity
* mam (message archive management)

**Setup**

MSSQL can be used from MongooseIM through the ODBC layer,
so you need to have it installed in your system.
Moreover, your Erlang/OTP as well as MongooseIM
must be built with support for ODBC.

You can configure MongooseIM appropriately by using the following command
(assuming you're in the top-level directory of the checked out repository):

```sh
./tools/configure with-odbc
```

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

# NOSQL

## Riak KV

Riak KV, for Key-Value, is technically supported by MongooseIM for versions upper than Riak KV 2.0. Erlang Solutions commercially supports Riak KV.

**Can be used for:**

* users (credentials)
* vcards
* roster
* private storage
* privacy/block lists
* last activity
* mam (message archive management)

**Setup**

We are using the Riak data types, so the minimal supported version is 2.0.
To be able to store above persistent date one have to run the following command:

```bash
RIAK_HOST="http://localhost:8098"

curl -XPUT $RIAK_HOST/search/schema/vcard \
    -H 'Content-Type:application/xml' \
    --data-binary @tools/vcard_search_schema.xml

curl -XPUT $RIAK_HOST/search/index/vcard \
    -H 'Content-Type: application/json' \
    -d '{"schema":"vcard"}'

#MAM
curl -XPUT $RIAK_HOST/search/schema/mam \
    -H 'Content-Type:application/xml' \
    --data-binary @tools/mam_search_schema.xml

curl -XPUT $RIAK_HOST/search/index/mam \
    -H 'Content-Type: application/json' \
    -d '{"schema":"mam"}'

# user base
riak-admin bucket-type create users '{"props":{"datatype":"map"}}'
riak-admin bucket-type activate users

# rosters
riak-admin bucket-type create rosters '{"props":{"datatype":"map"}}'
riak-admin bucket-type activate rosters
riak-admin bucket-type create roster_versions '{"props":{"last_write_wins":true, "dvv_enabled":false}}'
riak-admin bucket-type activate roster_versions

# private storage
riak-admin bucket-type create private '{"props":{"last_write_wins":true, "dvv_enabled":false}}'
riak-admin bucket-type activate private

# vCard

riak-admin bucket-type create vcard '{"props":{"last_write_wins":true, "search_index":"vcard", "dvv_enabled":false}}'
riak-admin bucket-type activate vcard

riak-admin bucket-type create mam_yz '{"props":{"datatype":"map", "search_index":"mam"}}'
riak-admin bucket-type activate mam_yz

# Last activity
riak-admin bucket-type create last '{"props":{"last_write_wins":true, "dvv_enabled":false}}'
riak-admin bucket-type activate last

# Offline messages

riak-admin bucket-type create offline '{"props":{"last_write_wins":true, "dvv_enabled":false}}'
riak-admin bucket-type activate offline

# Privacy/blocking lists

riak-admin bucket-type create privacy_defaults '{"props":{"last_write_wins":true, "dvv_enabled":false}}'
riak-admin bucket-type activate privacy_defaults

riak-admin bucket-type create privacy_lists_names '{"props":{"datatype":"set"}}'
riak-admin bucket-type activate privacy_lists_names

riak-admin bucket-type create privacy_lists '{"props":{"last_write_wins":true,"dvv_enabled":false}}'
riak-admin bucket-type activate privacy_lists

```

This will create buckt types, search schemas and indexes required
for storing above persitent date and it will activate them.

You should also configure Riak in `ejabberd.cfg` file.
Please refer to [Advanced configuration/Database setup](../Advanced-configuration.md) for more information.

## Redis

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
