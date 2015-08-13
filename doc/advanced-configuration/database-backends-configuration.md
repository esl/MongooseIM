
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

# MSSQL

**Can be used for:**

* users (credentials)
* vcards
* roster
* private storage
* privacy lists
* last activity
* mam (message archive)

**Setup**

# Riak (versions >=2.0)

**Can be used for:**

* users (credentials)

**Setup**

We are using the riak data types, so the minimal supported version is 2.0. To
be able to store **user credentials** one have to run the following command:

```bash
riak-admin bucket-type create users '{"props":{"datatype":"map"}}'
riak-admin bucket-type activate users
```

This will create a bucket type required for storing **users credentials** and it will
active it.

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

