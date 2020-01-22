## Overview

A Riak authentication module `ejabberd_auth_riak`.
It stores users in riak.

## Configuration options

The following options can be set in the `auth_opts` tuple in `mongooseim.cfg`.

* **bucket_type:**
    * **Description:** Riak bucket type.
    * **Value:** Binary
    * **Default:** `<<"users">>`

Example:

```erlang
{auth_opts, [
 {bucket_type, <<"users">>}
]}.
```
