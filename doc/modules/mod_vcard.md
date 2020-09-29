### Module Description
This module provides support for vCards, as specified in [XEP-0054: vcard-temp](http://xmpp.org/extensions/xep-0054.html) and [XEP-0055: Jabber Search](http://xmpp.org/extensions/xep-0055.html).

### Options

#### `modules.mod_vcard.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"no_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../../advanced-configuration/Modules/#iq-processing-policies).

#### `modules.mod_vcard.host`
* **Syntax:** string
* **Default:** `"vjud.@HOST@"`
* **Example:** `host = "vjud.@HOST@"`

Domain of the vCard User Directory, used for searching.
`@HOST@` is replaced with the domain(s) supported by the cluster.

#### `modules.mod_vcard.search`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `search = false`

Enables/disables the domain set in the previous option. `false` makes searching for users impossible.

#### `modules.mod_vcard.backend`
* **Syntax:** string, one of `"ldap"`, `"rdbms"`, `"riak"`, `"mnesia"`
* **Default:** `"mnesia"`
* **Example:** `backend = "rdbms"`

vCard storage backend. **Warning:** LDAP backend is read-only.

#### `modules.mod_vcard.matches`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `30`
* **Example:** `matches = 10`

Maximum search results to be returned to the user.

##### LDAP-specific options

The following options are the same as for the [LDAP authentication module](../../authentication-methods/ldap#configuration-options):

* [`modules.mod_vcard.ldap_pool_tag`](../../authentication-methods/ldap#authldappool_tag)
* [`modules.mod_vcard.ldap_base`](../../authentication-methods/ldap#authldapbase)
* [`modules.mod_vcard.ldap_uids`](../../authentication-methods/ldap#authldapuids)
* [`modules.mod_vcard.ldap_filter`](../../authentication-methods/ldap#authldapfilter)
* [`modules.mod_vcard.ldap_deref`](../../authentication-methods/ldap#authldapderef)

###### `modules.mod_vcard.ldap_vcard_map`
* **Syntax:** Array of TOML tables with the following keys: `"vcard_field"`, `"ldap_pattern"`, `"ldap_field"` and string values.
* **Default:** see description
* **Example:** `ldap_vcard_map = [{vcard_field = "FN", ldap_pattern = "%s", ldap_field = "displayName"}]`

Mappings between VCard and LDAP fields. For the default settings, please see `[MongooseIM root]/src/mod_vcard_ldap.erl`, line 79.

###### `modules.mod_vcard.ldap_search_fields`
* **Syntax:** Array of TOML tables with the following keys: `"search_field"`, `"ldap_field"` and string values.
* **Default:** see description
* **Example:** `ldap_search_fields = [{search_field = "User", ldap_field = "%u"}]`

Mappings between the human-readable search fields and LDAP fields.
For the default settings, please see `[MongooseIM root]/src/mod_vcard_ldap.erl`, line 101.

###### `modules.mod_vcard.ldap_search_reported`
* **Syntax:** Array of TOML tables with the following keys: `"search_field"`, `"vcard_field"` and string values.
* **Default:** see description
* **Example:** `ldap_search_reported = [{search_field = "Full Name", vcard_field = "FN"}]`

Mappings between the human-readable search fields and VCard fields.
For the default settings, please see `[MongooseIM root]/src/mod_vcard_ldap.erl`, line 114.

###### `modules.mod_vcard.ldap_search_operator`
* **Syntax:** string, one of `"or"`, `"and"`
* **Default:** `"and"`
* **Example:** `ldap_search_operator = "or"`

A default operator used for search query items.

###### `modules.mod_vcard.ldap_binary_search_fields`
* **Syntax:** array of strings
* **Default:** `[]`
* **Example:** `ldap_binary_search_fields = ["User", "Full Name"]`

An array of search fields, which values should be Base64-encoded by MongooseIM before sending to LDAP.

##### Riak-specific options

###### `modules.mod_vcard.riak.bucket_type`
* **Syntax:** string
* **Default:** `"vcard"`
* **Example:** `bucket_type = "vcard"`

Riak bucket type.

###### `modules.mod_vcard.riak.search_index`
* **Syntax:** string
* **Default:** `"vcard"`
* **Example:** `search_index = "vcard"`

Riak index name.

### Example Configuration
```
[modules.mod_vcard]
  allow_return_all = true
  search_all_hosts = true
  matches = 1
  search = true
  host = "directory.example.com"

  [[modules.mod_vcard.ldap_vcard_map]]
    vcard_field = "FAMILY"
    ldap_pattern = "%s"
    ldap_field = "sn"

  [[modules.mod_vcard.ldap_vcard_map]]
    vcard_field = "FN"
    ldap_pattern = "%s"
    ldap_field = "displayName"

  [[modules.mod_vcard.ldap_search_fields]]
    search_field = "User"
    ldap_field = "%u"

  [[modules.mod_vcard.ldap_search_fields]]
    search_field = "Full Name"
    ldap_field = "displayName"

  [[modules.mod_vcard.ldap_search_reported]]
    search_field = "Full Name"
    vcard_field = "FN"

  [[modules.mod_vcard.ldap_search_reported]]
    search_field = "Given Name"
    vcard_field = "FIRST"
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Backend action | Description (when it gets incremented) |
| ---- | -------------------------------------- |
| `set_vcard` | A vCard is set in a DB. |
| `get_vcard` | A specific vCard is retrieved from a DB. |
| `search` | A vCard search is performed. |
