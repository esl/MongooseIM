### Module Description

This module provides support for vCards, as specified in [XEP-0054: vcard-temp](http://xmpp.org/extensions/xep-0054.html) and [XEP-0055: Jabber Search](http://xmpp.org/extensions/xep-0055.html).

### Options

* `iqdisc` (default: `one_queue`).
* `host` (string, default: `"vjud.@HOST@"`): Domain of the vCard User Directory, used for searching.
 `@HOST@` is replaced with the domain(s) supported by the cluster.
* `search` (boolean, default: `true`): Enables/disables the domain set in previous option. 
 `false` makes searching for users impossible.
* `backend` (atom, default: `mnesia`): vCard storage backend. 
 Valid values are `ldap`, `odbc`, `riak` and `mnesia`. 
 **Warning:** LDAP backend is read-only.
* `matches` (`infinity` or positive integer, default: 30): Maximum search results to be returned to the user.
* `allow_return_all` (boolean):
* `search_all_hosts` (boolean): Search only local host or all hosts.

##### LDAP-specific options

* `ldap_vcard_map` (list of `{VCardField, LDAPPattern, LDAPField}`, default: see description): Mappings between VCard and LDAP fields. For the default setting, please see `[MongooseIM root]/apps/ejabberd/src/mod_vcard_ldap.erl`, line 74.
* `ldap_search_fields` (list of `{SearchField, LDAPField}`, default: see description): Mappings between the human-readable search fields and LDAP fields. 
 For the default setting, please see `[MongooseIM root]/apps/ejabberd/src/mod_vcard_ldap.erl`, line 96.
* `ldap_search_reported` (list of `{SearchField, VCardField}`, default: see description): Mappings between the human-readable search fields and VCard fields. 
 For the default setting, please see `[MongooseIM root]/apps/ejabberd/src/mod_vcard_ldap.erl`, line 109.
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
