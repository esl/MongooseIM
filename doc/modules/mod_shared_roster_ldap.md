## Module Description

This module, when enabled, will inject roster entries fetched from LDAP.
It might get quite complicated to configure it properly, so fasten your seatbelts and prepare for a ride.

When a default value for an option is defined with "top-level/XXX", it means that the default value is equal to a top-level parameter in `mongooseim.toml` of the same name.
If it is not defined, XXX becomes the default value.

## Options: general

### `modules.mod_shared_roster_ldap.ldap_pool_tag`
### `modules.mod_shared_roster_ldap.ldap_base`
### `modules.mod_shared_roster_ldap.ldap_deref`

These 3 options are the same as for the [LDAP authentication module](../../authentication-methods/ldap#configuration-options).

## Options: attributes

### `modules.mod_shared_roster_ldap.ldap_groupattr`
* **Syntax:** string
* **Default:** `"cn"`
* **Example:** `ldap_groupattr = "cn"`

Provides a group name.

### `modules.mod_shared_roster_ldap.ldap_groupdesc`
* **Syntax:** string
* **Default:** the value of `ldap_groupattr`
* **Example:** `ldap_groupdesc = "cn"`

Provides a group description.

### `modules.mod_shared_roster_ldap.ldap_userdesc`
* **Syntax:** string
* **Default:** `"cn"`
* **Example:** `ldap_userdesc = "cn"`

Provides a human-readable user name.

### `modules.mod_shared_roster_ldap.ldap_useruid`
* **Syntax:** string
* **Default:** `"cn"`
* **Example:** `ldap_useruid = "cn"`

Provides a username.

### `modules.mod_shared_roster_ldap.ldap_memberattr`
* **Syntax:** string
* **Default:** `"memberUid"`
* **Example:** `ldap_memberattr = "memberUid"`

Holds group members' IDs.

### `modules.mod_shared_roster_ldap.ldap_memberattr_format`
* **Syntax:** string
* **Default:** `"%u"`
* **Example:** `ldap_memberattr_format = "%u"`

Simple LDAP expression for extracting a user ID.

### `modules.mod_shared_roster_ldap.ldap_memberattr_format_re`
* **Syntax:** string
* **Default:** `""`
* **Example:** `ldap_memberattr_format_re = ""`

Allows extracting the user ID with a regular expression.

## Options: parameters

### `modules.mod_shared_roster_ldap.ldap_auth_check`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `ldap_auth_check = true`

Enables checking if a shared roster entry actually exists in the XMPP database.

### `modules.mod_shared_roster_ldap.ldap_user_cache_validity`
* **Syntax:** positive integer
* **Default:** top-level/`300`
* **Example:** `ldap_user_cache_validity = 300`

Specifies in seconds how long are the roster entries kept in the cache.

### `modules.mod_shared_roster_ldap.ldap_group_cache_validity`
* **Syntax:** positive integer
* **Default:** top-level/`300`
* **Example:** `ldap_group_cache_validity = 300`

Specifies in seconds how long is the user's membership in a group kept in the cache.

### `modules.mod_shared_roster_ldap.ldap_user_cache_size`
* **Syntax:** positive integer
* **Default:** top-level/`1000`
* **Example:** `ldap_user_cache_size = 1000`

Specifies how many shared roster items are kept in the cache.

### `modules.mod_shared_roster_ldap.ldap_group_cache_size`
* **Syntax:** positive integer
* **Default:** top-level/`1000`
* **Example:** `ldap_group_cache_size = 1000`

Specifies how many roster group entries are kept in cache.

## Options: LDAP filters

### `modules.mod_shared_roster_ldap.ldap_rfilter`
* **Syntax:** string
* **Default:** top-level/`""`
* **Example:** `ldap_rfilter = "(objectClass=inetOrgPerson)"`

Used to find names of all shared roster groups.

### `modules.mod_shared_roster_ldap.ldap_gfilter`
* **Syntax:** string
* **Default:** top-level/`""`
* **Example:** `ldap_gfilter = ""`

Used for retrieving the human-readable name and the members of a group.

### `modules.mod_shared_roster_ldap.ldap_ufilter`
* **Syntax:** string
* **Default:** top-level/`""`
* **Example:** `ldap_ufilter = ""`

Used for retrieving the human-readable name of the roster entries.

### `modules.mod_shared_roster_ldap.ldap_filter`
* **Syntax:** string
* **Default:** top-level/`""`
* **Example:** `ldap_filter = "(objectClass=inetOrgPerson)"`

Filter AND-ed with previous filters.

## Example Configuration

```toml
[modules.mod_shared_roster_ldap]
  ldap_base = "ou=Users,dc=ejd,dc=com"
  ldap_groupattr = "ou"
  ldap_memberattr = "cn"
  ldap_userdesc = "cn"
  ldap_filter = "(objectClass=inetOrgPerson)"
  ldap_rfilter = "(objectClass=inetOrgPerson)"
  ldap_group_cache_validity = 1
  ldap_user_cache_validity = 1
```
