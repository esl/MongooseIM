## Overview

This authentication method stores user accounts in a relational database, e.g. MySQL or PostgreSQL.

## Configuration options

The `rdbms` method uses an outgoing connection pool of type `rdbms` with the `default` tag - it has to be defined in the `outgoing_pools` section.

### `auth.rdbms.users_number_estimate`
* **Syntax:** boolean
* **Default:** false
* **Example:** `users_number_estimate = true`

By default querying MongooseIM for the number of registered users uses the `SELECT COUNT` query, which might be slow.
Enabling this option makes MongooseIM use an alternative query that might be not as accurate, but is always fast.

!!! Note
    This option is effective only for MySQL and PostgreSQL.

### Example

Authentication:

```toml
[auth.rdbms]
  users_number_estimate = true
```

Outgoing pools:

```toml
[outgoing_pools.rdbms.default.connection]
  driver = "pgsql"
  host = "localhost"
  database = "mongooseim"
  username = "mongooseim"
  password = "mongooseim_secret"
```
