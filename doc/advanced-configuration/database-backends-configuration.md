# Database Backends

MongooseIM can work with several databases, both RDBMS (SQL) and NOSQL ones.
Some of them require extra work before they can be used.
For example the SQL databases require defining a schema.
MongooseIM is tested with TravisCI, so the travis scripts can be used as a reference.

# A Brief Overview

Data in MongooseIM is either transient or persistent:

* **transient**: volatile data changing often, such as session data, stream management data, and other in-memory data.
 These don't need any backup, since after a potential failure, they will naturally rebuild as clients reconnect.
* **persistent**: long-lived data, such as roster items, credentials, and chat archives.
 These absolutely need regular and tested backups.

# Choosing a database for MongooseIM

Here is some general advice on the use of databases.
Subsequent sections go into more depth on each database: what they are suitable for and how to set them up.

Transient data:

* Mnesia - we highly recommend Mnesia (a highly available and distributed database) over Redis for storing **transient** data.
 Being an Erlang-based database, it's the default persistance option for most modules in MongooseIM.
 **Warning**: we **strongly recommend** keeping **persistent** data in an external DB (RDBMS or Riak) for production.
 Mnesia is not suitable for the volumes of **persistent** data which some modules may require.
 Sooner or later a migration will be needed which may be painful.
 It is possible to store all data in Mnesia, but only for testing purposes, not for any serious deployments.

* Redis - A fantastic choice for storing live data.
 It's highly scalable and it can be easily shared by multiple MongooseIM nodes.
 Additionally, Redis' great performance makes it an excellent choice for storing `user session` data.
 We recommend caution, since it has not yet been widely tested in production.


Persistent Data:

* RDBMS - MongooseIM has a strong backend support for relational databases.
 Reliable and battle proven, they are a great choice for regular MongooseIM use cases and features like `privacy lists`, `vcards`, `roster`, `private storage`, `last activity` and `message archive`.
 Never loose your data.
 Use MySQL, MariaDB, PostgreSQL, or MS SQL Server.

* Riak KV - If you're planning to deploy a massive cluster, consider Riak KV as a potential storage backend solution.
 It offers high availability and fault tolerance which is excatly what you need for your distributed MongooseIM architecture.
 Use Riak KV with `privacy lists`, `vcards`, `roster`, `private storage`, `last activity` and `message archive`.
 Erlang Solutions commercially supports Riak KV.

* Cassandra - Only for MAM (Message Archive Management).

* ElasticSearch - Only for MAM (Message Archive Management).


User Data:

* LDAP -  Used for: users, shared rosters, vCards


# RDBMS

## MySQL

**Can be used for**:

* users (credentials)
* vcards
* roster
* private storage
* privacy/block lists
* last activity
* mam (message archive management)
* muc_light rooms

**Setup**

The schema files can be found in the `priv` directory.
The default schema is defined in the `mysql.sql` file.

You can use the following command to apply it on localhost:

```bash
mysql -h localhost -u user -p -e 'create database mongooseim'
mysql -h localhost -u user -p mongooseim < mysql.sql
```

You should also configure MySQL database in the `mongooseim.cfg` file.
Please refer to the [Advanced configuration/Database setup](../Advanced-configuration.md) for more information.

**Version notice**

The required minimum version of MySQL is `5.5.14`. For versions `5.7.8` and older, add the following options to your MySQL configuration file:

```bash
innodb_large_prefix=true
innodb_file_format=BARRACUDA
innodb_file_format_max=BARRACUDA
innodb_file_per_table=true
```

For versions `5.7.9` and newer, all of the above options are set correctly by default.

## PostgreSQL

**Can be used for:**

* users (credentials)
* vcards
* roster
* private storage
* privacy/block lists
* last activity
* mam (message archive management)
* muc_light rooms

**Setup**

The schema files can be found in the `priv` directory.
The default schema is defined in the `pg.sql` file.

You can use the following command to apply it on localhost:

```bash
psql -h localhost -U user -c "CREATE DATABASE mongooseim;"
psql -h localhost -U user -q -d mongooseim -f pg.sql
```
You should also configure Postgres database in `mongooseim.cfg` file.
Please refer to the [Advanced configuration/Database setup](../Advanced-configuration.md) for more information.

## Microsoft SQL Server

Microsoft SQL Server, sometimes called MSSQL, or Azure SQL Database.

**Warning: MongooseIM can only connect to MSSQL [on Ubuntu Xenial x64](../operation-and-maintenance/known-issues.md).**

**This can be used for:**

* users (credentials)
* vcards
* roster
* private storage
* privacy/block lists
* last activity
* mam (message archive management)
* muc_light rooms

**Setup**

MSSQL can be used from MongooseIM through the ODBC layer with FreeTDS driver, so you need them installed on your system.

```bash
# Ubuntu
$ sudo apt install freetds-dev tdsodbc

# CentOS
$ sudo yum install freetds

# macOS
$ brew install freetds
```

Then you need to configure the connection.
Add your database (`mongooseim` here) to the `/etc/odbc.ini` or `$HOME/.odbc.ini` file:

```ini
[mongoose-mssql]
; Ubuntu
Driver      = /usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so
Setup       = /usr/lib/x86_64-linux-gnu/odbc/libtdsS.so
; CentOS
; Driver      = /usr/lib64/libtdsodbc.so.0
; Setup       = /usr/lib64/libtdsS.so
; macOS
; Driver      = /usr/local/Cellar/freetds/[current version]/lib/libtdsodbc.so
Server      = 127.0.0.1
Port        = 1433
Database    = mongooseim
Charset     = UTF-8
TDS_Version = 7.2
client_charset = UTF-8
```

Please amend the paths above to match your current OS if necessary.

For more details, please refer to the [freetds.conf documentation](http://www.freetds.org/userguide/freetdsconf.htm) and
[unixodbc documentation](http://www.unixodbc.org/odbcinst.html).

MongooseIM is built with ODBC support by default.

**Deadlocks notice**

If muc_light's backend is set to ODBC and in your system there is many rooms created in parallel,
there may be some deadlocks due to the `READ_COMMITTED_SNAPSHOT` set to `OFF` by default.
In this case we recommend to set this database property to `ON`, this will enable row level locking which significantly reduces
deadlock chances around muc_light operations.

This property can be set by the following `ALTER DATABASE` query:

```sql
ALTER DATABASE $name_of_your_db SET READ_COMMITTED_SNAPSHOT ON
```

The command above may take some time.

Then you need to import the SQL schema from either ``mssql2012.sql`` or ``azuresql.sql`` file depending on which database you are using.
You can use a Microsoft's GUI tool (the provided .sql files should work with it) or isql, but after a slight modification of the dump file:

```bash
cat azuresql.sql | tr -d '\r' | tr '\n' ' ' | sed 's/GO/\n/g' |
isql mongoose-mssql username password -b
```

The final step is to configure ``mongooseim.cfg`` appropriately.
Configure the database section as follows:

```erlang
{rdbms_server_type, mssql}.
{outgoing_pools, [
 {rdbms, global, default, [{workers, 5}],
  [{server, "DSN=mongoose-mssql;UID=username;PWD=password"}]}
]}.
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

This will create bucket types, search schemas and indexes required for storing the above persitent data and it will activate them.

You should also configure Riak in the `mongooseim.cfg` file.
Please refer to [Advanced configuration/Database setup](../Advanced-configuration.md) for more information.

## Cassandra

**Setup**

This will prepare Cassandra for connection from MongooseIM. Make sure Cassandra is running, open a new terminal window and enter the following commands:
```
$ cqlsh
$ cqlsh> source '$REPO/priv/casssandra.cql';
```

## ElasticSearch

**Can be used for:**

* MAM (Message Archive Management)

**Setup**

Please note that MongooseIM has been tested to work properly with ElasticSearch version 5.6.9.

In order to use ElasticSearch as a MAM backend, you'll need to create required indexes and mappings.
From the root of MongooseIM's repository run:

```bash
curl -X PUT $ELASTICSEARCH_URL/messages -d '@priv/elasticsearch/pm.json'
curl -X PUT $ELASTICSEARCH_URL/muc_messages -d '@priv/elasticsearch/muc.json'
```

where `$ELASTICSEARCH_URL` is a URL pointing to your ElasticSearch node's HTTP API endpoint.

Please refer to [advanced configuration](../Advanced-configuration.md#elasticsearch-connection-setup) page to check how to configure MongooseIM to connect to ElasticSearch node.

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

No additional steps required, the modules that are using LDAP are very customizable, so they can be configured to support existsing schemas.d
