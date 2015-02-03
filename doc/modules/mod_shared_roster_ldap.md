### Module Description

This module, when enabled, will inject roster entries fetched from LDAP. It might get quite complicated to configure it properly, so fasten your seatbelts and prepare for a ride. :)

When default value for an option is defined with "top-level/XXX", it means that default value is equal to top-level parameter in `ejabberd.cfg` of the same name. If such is not defined, XXX becomes the default value.


### Options (attributes)

* **ldap_groupattr** (string, default: `"cn"`) - Attribute providing a group name.
* **ldap_groupdesc** (string, default: value of `ldap_groupattr`) - Attribute providing a group description.
* **ldap_userdesc** (string, default: `"cn"`) - Attribute providing a human-readable user name.
* **ldap_useruid** (string, default: `"cn"`) - Attribute providing a username.
* **ldap_memberattr** (string, default: `"memberUid"`) - Attribute holding group members' IDs.
* **ldap_memberattr_format** (string, default: `"%u"`) - Simple LDAP expression for extracting a user ID.
* **ldap_memberattr_format_re** (string, default: `""`) - Allows extracting the user ID with a regular expression.

### Options (parameters)

* **ldap_auth_check** (boolean, default: `true`) - Enables checking if a shared roster entry actually exists in the XMPP database.
* **ldap_user_cache_validity** (integer, default: top-level/300) - Specifies in seconds how long are the roster entries kept in the cache. 
* **ldap_group_cache_validity** (integer, default: top-level/300) - Specifies in seconds how long is the user's membership in a group kept in cache .
* **ldap_user_cache_size** (integer, default: top-level/1000) - Specifies how many shared roster items are kept in cache.
* **ldap_group_cache_size** (integer, default: top-level/1000) - Specifies how many roster group entries are kept in cache.

### Options (LDAP filters)

* **ldap_rfilter** (string, default: top-level/`""`) - Used to find names of all shared roster groups.
* **ldap_gfilter** (string, default: top-level/`""`) - Used for retrieving the human-readable name and the members of a group.
* **ldap_ufilter** (string, default: top-level/`""`) - Used for retrieving the human-readable name of the roster entries.
* **ldap_filter** (string, default: top-level/`""`) - Filter AND-ed with previous filters.

### Example Configuration
```
{mod_shared_roster_ldap, [
     {ldap_base, "ou=Users,dc=ejd,dc=com"},
     {ldap_groupattr, "ou"},
     {ldap_memberattr, "cn"},{ldap_userdesc, "cn"},
     {ldap_filter, "(objectClass=inetOrgPerson)"},
     {ldap_rfilter, "(objectClass=inetOrgPerson)"},
     {ldap_group_cache_validity, 1},
     {ldap_user_cache_validity, 1}]}
```
