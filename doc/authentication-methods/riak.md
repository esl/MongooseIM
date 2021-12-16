## Overview

This authentication method stores user accounts in Riak.

## Configuration options

The `riak` method uses an outgoing connection pool of type `riak` with the `default` tag - it has to be defined in the `outgoing_pools` section.

There is one additional option:

### `auth.riak.bucket_type`
* **Syntax:** string
* **Default:** `"users"`
* **Example:** `bucket_type = "user_bucket"`

Bucket type for storing users in Riak.

### Example

Authentication:

```toml
[auth.riak]
  bucket_type = "user"
```

Outgoing pools:

```toml
[outgoing_pools.riak.default]
  connection.address = "127.0.0.1"
  connection.port = 8087
```
