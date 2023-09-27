# Database Backends

MongooseIM can work with several databases, both RDBMS (SQL) and NoSQL ones.
Some of them require extra work before they can be used.
For example the SQL databases require defining a schema.
MongooseIM is tested with CI, so the CI scripts can be used as a reference.

## A Brief Overview

Data in MongooseIM is either transient or persistent:

* **transient**: volatile data changing often, such as session data, stream management data, and other in-memory data.
 These don't need any backup, since after a potential failure, they will naturally rebuild as clients reconnect.
* **persistent**: long-lived data, such as roster items, credentials, and chat archives.
 These absolutely need regular and tested backups.

## Choosing a database for MongooseIM

Here is some general advice on the use of databases.
Subsequent sections go into more depth on each database: what they are suitable for and how to set them up.

Transient data:

* CETS - a library to synchronise ETS tables between nodes.
  A new choice to share live data across the MongooseIM cluster.
  We recommend to use this backend for transient data.
  This backend requires an RDBMS database configured because we use an external database to discover nodes in the cluster.
  For a CETS config example, see [tutorials](../tutorials/CETS-configure.md).

* Mnesia - a built-in Erlang Database.
  Mnesia is fine for a cluster of fixed size with reliable networking between nodes and with nodes rarely restarted.
  There are some issues when nodes are restarting or new ones joining the cluster. For this case, we recommend to use CETS instead.
  Mnesia is still the default backend for some modules for compatibility reasons with older config files.

* Redis - A fantastic choice for storing live data.
 It's highly scalable and it can be easily shared by multiple MongooseIM nodes.
 Additionally, Redis' great performance makes it an excellent choice for storing `user session` data.
 We recommend caution, since it has not yet been widely tested in production.


Persistent Data:

* RDBMS - MongooseIM has a strong backend support for relational databases.
 Reliable and battle proven, they are a great choice for regular MongooseIM use cases and features like `privacy lists`, `vcards`, `roster`, `private storage`, `last activity` and `message archive`.
 Never loose your data.
 Use MySQL, MariaDB, PostgreSQL, or MS SQL Server.

* Cassandra - Only for MAM (Message Archive Management).

* ElasticSearch - Only for MAM (Message Archive Management).

* Mnesia - some backends support Mnesia to store data, but it is not recommended.
  It is still the default option, when not specifying a backend for many modules, so be careful.

    !!! Warning
        We **strongly recommend** keeping **persistent** data in an external DB (RDBMS) for production.
        Mnesia is not suitable for the volumes of **persistent** data which some modules may require.
        Sooner or later a migration will be needed which may be painful.
        It is possible to store all data in Mnesia, but only for testing purposes, not for any serious deployments.

User Data:

* LDAP -  Used for: users, shared rosters, vCards

## RDBMS

### MySQL

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

You should also configure the MySQL database in the `mongooseim.toml` file.
Please refer to the [RDBMS options](outgoing-connections.md#rdbms-options) for more information.

**Version notice**

The required minimum version of MySQL is `8.0` because MongooseIM uses the JSON data type and the `INSERT INTO ... AS ...` query syntax.

### PostgreSQL

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
You should also configure the Postgres database in the `mongooseim.toml` file.
Please refer to the [RDBMS options](outgoing-connections.md#rdbms-options)
and [general database options](general.md#database-settings)
for more information.

### Microsoft SQL Server

Microsoft SQL Server, sometimes called MSSQL, or Azure SQL Database.

!!! Warning
    MongooseIM can only connect to MSSQL [on Ubuntu Xenial x64](../operation-and-maintenance/known-issues.md).

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

For more details, please refer to the [freetds.conf documentation](http://www.freetds.org/userguide/freetdsconf.html) and
[unixodbc documentation](http://www.unixodbc.org/odbcinst.html).

MongooseIM is built with ODBC support by default.

**Deadlocks notice**

If muc_light's backend is set to ODBC and there are many rooms created in parallel in your system,
there may be some deadlocks due to the `READ_COMMITTED_SNAPSHOT` set to `OFF` by default.
In this case we recommend setting this database property to `ON`, this will enable row level locking which significantly reduces
deadlock chances around muc_light operations.

This property can be set by the following `ALTER DATABASE` query:

```sql
ALTER DATABASE $name_of_your_db SET READ_COMMITTED_SNAPSHOT ON
```

The command above may take some time.

Then you need to import the SQL schema from  ``mssql2012.sql``.
You can use a Microsoft's GUI tool (the provided .sql files should work with it) or isql, but after a slight modification of the dump file:

```bash
cat mssql2012.sql | tr -d '\r' | tr '\n' ' ' | sed 's/GO/\n/g' |
isql mongoose-mssql username password -b
```

The final step is to configure ``mongooseim.toml`` appropriately.
Set the following option in the `general` section:

```toml
[general]
  rdbms_server_type = "mssql"
```

Configure the `outgoing_pools.rdbms` section as follows:

```toml
[outgoing_pools.rdbms.default]
  workers = 5

  [outgoing_pools.rdbms.default.connection]
    driver = "odbc"
    settings = "DSN=mongoose-mssql;UID=username;PWD=password"
```

## NoSQL

### Cassandra

**Setup**

This will prepare Cassandra for connection from MongooseIM. Make sure Cassandra is running, open a new terminal window and enter the following commands:
```
$ cqlsh
$ cqlsh> source '$REPO/priv/casssandra.cql';
```

### ElasticSearch

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

Please refer to the [advanced configuration](outgoing-connections.md#elasticsearch-options) page to check how to configure MongooseIM to connect to ElasticSearch node.

### Redis

**Can be used for:**

* users sessions

**Setup**

Please refer to the [Redis options](outgoing-connections.md#redis-specific-options) for more information.

## LDAP

**Can be used for:**

* users (credentials)
* shared roster
* vcard

**Setup**

Please refer to the [LDAP options](outgoing-connections.md#ldap-options) for more information.
