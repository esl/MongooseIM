### Module Description
This module implements [XEP-0049: Private XML Storage](http://xmpp.org/extensions/xep-0049.html).
It allows users to store custom XML data in the server's database. Used e.g. for storing roster groups separator.

### Options

#### `modules.mod_private.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"one_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../../advanced-configuration/Modules/#iq-processing-policies).

#### `modules.mod_private.backend`
* **Syntax:** string, one of `"mnesia"`, `"rdbms"`, `"riak"`.
* **Default:** "mnesia"
* **Example:** `backend = "mnesia"`

Database backend to use.

**CAUTION:**  Riak KV backend doesn't support transactions (rollbacks), so please avoid inserting
more than one value in a single set request, otherwise you may end up with partially saved data.
Backend returns the first error.

##### Riak-specific options

###### `modules.mod_privacy.riak.bucket_type`
* **Syntax:** string
* **Default:** `"private"`
* **Example:** `bucket_type = "private"`

Riak bucket type.

### Example Configuration
```toml
[modules.mod_private]
  backend = "mnesia"
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Backend operation | Description (when it gets incremented) |
| ---- | -------------------------------------- |
| `multi_get_data` | XML data is fetched from a DB. |
| `multi_set_data` | XML data is stored in a DB. |
