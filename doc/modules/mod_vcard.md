## Module Description

This module provides support for vCards, as specified in [XEP-0054: vcard-temp](http://xmpp.org/extensions/xep-0054.html) and [XEP-0055: Jabber Search](http://xmpp.org/extensions/xep-0055.html).

## Options

### `modules.mod_vcard.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"parallel"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_vcard.host`
* **Syntax:** string
* **Default:** `"vjud.@HOST@"`
* **Example:** `host = "vjud.@HOST@"`

Domain of the vCard User Directory, used for searching.
`@HOST@` is replaced with the domain(s) supported by the cluster.

### `modules.mod_vcard.search`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `search = false`

Enables/disables the domain set in the previous option. `false` makes searching for users impossible.

### `modules.mod_vcard.backend`
* **Syntax:** string, one of `"ldap"`, `"rdbms"`, `"mnesia"`
* **Default:** `"mnesia"`
* **Example:** `backend = "rdbms"`

vCard storage backend.

!!! Warning 
    LDAP backend is read-only.

### `modules.mod_vcard.matches`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `30`
* **Example:** `matches = 10`

Maximum search results to be returned to the user.

### LDAP-specific options

The following options are the same as for the [LDAP authentication module](../authentication-methods/ldap.md#configuration-options):

#### [`modules.mod_vcard.ldap.pool_tag`](../authentication-methods/ldap.md#authldappool_tag)
#### [`modules.mod_vcard.ldap.base`](../authentication-methods/ldap.md#authldapbase)
#### [`modules.mod_vcard.ldap.uids`](../authentication-methods/ldap.md#authldapuids)
#### [`modules.mod_vcard.ldap.filter`](../authentication-methods/ldap.md#authldapfilter)
#### [`modules.mod_vcard.ldap.deref`](../authentication-methods/ldap.md#authldapderef)

#### `modules.mod_vcard.ldap.vcard_map`
* **Syntax:** Array of TOML tables with the following keys: `"vcard_field"`, `"ldap_pattern"`, `"ldap_field"` and string values.
* **Default:** see description
* **Example:** `vcard_map = [{vcard_field = "FN", ldap_pattern = "%s", ldap_field = "displayName"}]`

Mappings between VCard and LDAP fields. For the default settings, please see `[MongooseIM root]/src/mod_vcard_ldap.erl`.

#### `modules.mod_vcard.ldap.search_fields`
* **Syntax:** Array of TOML tables with the following keys: `"search_field"`, `"ldap_field"` and string values.
* **Default:** see description
* **Example:** `search_fields = [{search_field = "User", ldap_field = "%u"}]`

Mappings between the human-readable search fields and LDAP fields.
For the default settings, please see `[MongooseIM root]/src/mod_vcard_ldap.erl`.

#### `modules.mod_vcard.ldap.search_reported`
* **Syntax:** Array of TOML tables with the following keys: `"search_field"`, `"vcard_field"` and string values.
* **Default:** see description
* **Example:** `search_reported = [{search_field = "Full Name", vcard_field = "FN"}]`

Mappings between the human-readable search fields and VCard fields.
For the default settings, please see `[MongooseIM root]/src/mod_vcard_ldap.erl`.

#### `modules.mod_vcard.ldap.search_operator`
* **Syntax:** string, one of `"or"`, `"and"`
* **Default:** `"and"`
* **Example:** `search_operator = "or"`

A default operator used for search query items.

#### `modules.mod_vcard.ldap.binary_search_fields`
* **Syntax:** array of strings
* **Default:** `[]`
* **Example:** `binary_search_fields = ["User", "Full Name"]`

An array of search fields, which values should be Base64-encoded by MongooseIM before sending to LDAP.

## Example Configuration

```toml
[modules.mod_vcard]
  matches = 1
  search = true
  host = "directory.example.com"

  [[modules.mod_vcard.ldap.vcard_map]]
    vcard_field = "FAMILY"
    ldap_pattern = "%s"
    ldap_field = "sn"

  [[modules.mod_vcard.ldap.vcard_map]]
    vcard_field = "FN"
    ldap_pattern = "%s"
    ldap_field = "displayName"

  [[modules.mod_vcard.ldap.search_fields]]
    search_field = "User"
    ldap_field = "%u"

  [[modules.mod_vcard.ldap.search_fields]]
    search_field = "Full Name"
    ldap_field = "displayName"

  [[modules.mod_vcard.ldap.search_reported]]
    search_field = "Full Name"
    vcard_field = "FN"

  [[modules.mod_vcard.ldap.search_reported]]
    search_field = "Given Name"
    vcard_field = "FIRST"
```

## Metrics

This module provides [backend metrics](../operation-and-maintenance/MongooseIM-metrics.md#backend-metrics).
If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

Prometheus metrics have a `host_type` and `function` labels associated with these metrics.
Since Exometer doesn't support labels, the function as well as the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

Backend in the action name can be either `rdbms`, `ldap` or `mnesia`.

=== "Prometheus"

    | Backend action | Type | Function | Description (when it gets incremented) |
    | -------------- | ---- | -------- | -------------------------------------- |
    | `mod_vcard_Backend_count` | counter | `set_vcard` | A vCard is set in a database. |
    | `mod_vcard_Backend_time`  | histogram | `set_vcard` | Time to set a vCard in a database. |
    | `mod_vcard_Backend_count` | counter | `get_vcard` | A specific vCard is retrieved from a database. |
    | `mod_vcard_Backend_time`  | histogram | `get_vcard` | Time to retrieve a specific vCard from a database. |
    | `mod_vcard_Backend_count` | counter | `search` | A vCard search is performed. |
    | `mod_vcard_Backend_time`  | histogram | `search` | Time to search a vCard. |

=== "Exometer"

    | Backend action | Type | Description (when it gets incremented) |
    | -------------- | ---- | -------------------------------------- |
    | `[HostType, mod_vcard_Backend, set_vcard, count]` | spiral | A vCard is set in a database. |
    | `[HostType, mod_vcard_Backend, set_vcard, time]`  | histogram | Time to set a vCard in a database. |
    | `[HostType, mod_vcard_Backend, get_vcard, count]` | spiral | A specific vCard is retrieved from a database. |
    | `[HostType, mod_vcard_Backend, get_vcard, time]`  | histogram | Time to retrieve a specific vCard from a database. |
    | `[HostType, mod_vcard_Backend, search, count]` | spiral | A vCard search is performed. |
    | `[HostType, mod_vcard_Backend, search, time]`  | histogram | Time to search a vCard. |
