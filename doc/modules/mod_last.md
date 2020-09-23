### Module Description

Implements [XEP-0012: Last Activity](https://xmpp.org/extensions/xep-0012.html).

Use with caution, as it was observed that a user disconnect spike might result in overloading the database with "last activity" writes.

### Options

#### `modules.mod_last.iqdisc`
* **Syntax:** string
* **Default:** `no_queue`
* **Example:** `iqdisc = one_queue`

#### `modules.mod_last.backend`
* **Syntax:** string, one of `"mnesia"`, `"rdbms"`, `"riak"`
* **Default:** `mnesia`
* **Example:** `backend = rdbms`

Storage backend.

##### Riak-specific options

* `bucket_type` (default `<<"last">>`) - Riak bucket type.

### Example Configuration

```
[modules.mod_last]
  backend = "rdbms"
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Backend action | Description (when it gets incremented) |
| ---- | -------------------------------------- |
| `get_last` | A timestamp is fetched from DB. |
| `set_last_info` | A timestamp is stored in DB. |

