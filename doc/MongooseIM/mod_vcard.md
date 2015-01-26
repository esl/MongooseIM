### Module Description
This module provides support for VCards, as specified in [XEP-0054 (vcard-temp)](http://xmpp.org/extensions/xep-0054.html) and [XEP-0055 (Jabber Search)](http://xmpp.org/extensions/xep-0055.html).

### Options

* **iqdisc**
* **host** (string, default: `"vjud.@HOST@"`) - Domain of VCard User Directory. Used for searching. `@HOST@` is replaced with domain(s) supported by the cluster.
* **search** (boolean, default: `true`) - Enables/disables the domain set in previous option. `false` makes searching for users impossible.
* **backend** (atom, default: `mnesia`) - VCard storage backend. Valid values are `ldap`, `odbc` and `mnesia`. **Warninig:** LDAP backend is read-only.
* **matches** (`inifnity` or positive integer, default: 30) - Maxmimum search results to be returned to the user.

##### Mnesia & ODBC-specific options

* **allow_return_all (boolean, default: `false`) - Enabling this option allows user to get all directory entries by submitting empty search query.

##### Mnesia-specific options

* **search_all_hosts** (boolean, default: `true`) - Searches will apply to all supported domains, not only to the current one.

##### LDAP-specific options

* **ldap_vcard_map** (list of `{VCardField, LDAPPattern, LDAPField}`, default: see description) - Mappings between VCard and LDAP fields. For the default setting, please see `[MongooseIM root]/apps/ejabberd/src/mod_vcard_ldap.erl`, line 74.
* **ldap_search_fields** (list of `{SearchField, LDAPField}`, default: see description) - Mappings between the human-readable search fields and LDAP fields. For the default setting, please see `[MongooseIM root]/apps/ejabberd/src/mod_vcard_ldap.erl`, line 96.
* **ldap_search_reported** (list of `{SearchField, VCardField}`, default: see description) - Mappings between the human-readable search fields and VCard fields. For the default setting, please see `[MongooseIM root]/apps/ejabberd/src/mod_vcard_ldap.erl`, line 109.

### Example Configuration
```
{mod_vcard, [ {allow_return_all, true},
              {search_all_hosts, true},
              {matches, 1},
              {search, true},
               {host, "directory.example.com"}
             ]}
```
