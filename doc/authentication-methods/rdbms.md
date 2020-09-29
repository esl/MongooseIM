## Overview

This authentication method stores user accounts in a relational database, e.g. MySQL or PostgreSQL.

## Configuration

The `rdbms` method uses an outgoing connection pool of type `rdbms` with the `default` tag - it has to be defined in the `outgoing_pools` section.

### Example

Authentication:

```
[auth]
  methods = ["rdbms"]
```

Outgoing pools:

```
[outgoing_pools.rdbms.default.connection]
  driver = "pgsql"
  host = "localhost"
  database = "mongooseim"
  username = "mongooseim"
  password = "mongooseim_secret"
```
