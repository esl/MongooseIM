### Module Description
This module provides support for vCards, as specified in [XEP-0054: vcard-temp](http://xmpp.org/extensions/xep-0054.html) and [XEP-0055: Jabber Search](http://xmpp.org/extensions/xep-0055.html).

### Options

* `iqdisc` (default: `one_queue`)
* `host` (string, default: `"vjud.@HOST@"`): Domain of the vCard User Directory, used for searching.
 `@HOST@` is replaced with the domain(s) supported by the cluster.
* `search` (boolean, default: `true`): Enables/disables the domain set in the previous option.
 `false` makes searching for users impossible.
* `backend` (atom, default: `mnesia`): vCard storage backend.
 Valid values are `ldap`, `rdbms`, `riak` and `mnesia`.
 **Warning:** LDAP backend is read-only.
* `matches` (`inifnity` or positive integer, default: 30): Maxmimum search results to be returned to the user.

##### LDAP-specific options

* `ldap_vcard_map` (list of `{VCardField, LDAPPattern, LDAPField}`, default: see description): Mappings between VCard and LDAP fields. For the default setting, please see `[MongooseIM root]/src/mod_vcard_ldap.erl`, line 74.
* `ldap_search_fields` (list of `{SearchField, LDAPField}`, default: see description): Mappings between the human-readable search fields and LDAP fields.
 For the default setting, please see `[MongooseIM root]/src/mod_vcard_ldap.erl`, line 96.
* `ldap_search_reported` (list of `{SearchField, VCardField}`, default: see description): Mappings between the human-readable search fields and VCard fields.
 For the default setting, please see `[MongooseIM root]/src/mod_vcard_ldap.erl`, line 109.
* `ldap_search_operator` (`or` | `and`, default: `and`): A default operator used for search query items.
* `ldap_binary_search_fields` (list of binaries, default: `[]`): A list of search fields, which values should be Base64-encoded by MongooseIM before sending to LDAP.

### Example Configuration
```
{mod_vcard, [ {allow_return_all, true},
              {search_all_hosts, true},
              {matches, 1},
              {search, true},
              {host, "directory.example.com"}
             ]}
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, backends, mod_vcard, set_vcard]` | histogram | Time it takes to set a vCard in a DB. |
| `[global, backends, mod_vcard, get_vcard]` | histogram | Time it takes to fetch a specific vCard from a DB. |
| `[global, backends, mod_vcard, search]` | histogram | Time it takes to perform a vCard search in a DB. |
