## Module Description

This module injects roster entries fetched from LDAP.
It might get quite complicated to configure it properly, so proceed with caution.

!!! warning
    This module does not support [dynamic domains](../configuration/general.md#generalhost_types).

## Options: general

### `modules.mod_shared_roster_ldap.pool_tag`
### `modules.mod_shared_roster_ldap.base`
### `modules.mod_shared_roster_ldap.deref`

These 3 options are the same as for the [LDAP authentication module](../authentication-methods/ldap.md#configuration-options).

## Options: attributes

### `modules.mod_shared_roster_ldap.groupattr`
* **Syntax:** string
* **Default:** `"cn"`
* **Example:** `groupattr = "cn"`

Provides a group name.

### `modules.mod_shared_roster_ldap.groupdesc`
* **Syntax:** string
* **Default:** the value of `groupattr`
* **Example:** `groupdesc = "cn"`

Provides a group description.

### `modules.mod_shared_roster_ldap.userdesc`
* **Syntax:** string
* **Default:** `"cn"`
* **Example:** `userdesc = "cn"`

Provides a human-readable user name.

### `modules.mod_shared_roster_ldap.useruid`
* **Syntax:** string
* **Default:** `"cn"`
* **Example:** `useruid = "cn"`

Provides a username.

### `modules.mod_shared_roster_ldap.memberattr`
* **Syntax:** string
* **Default:** `"memberUid"`
* **Example:** `memberattr = "memberUid"`

Holds group members' IDs.

### `modules.mod_shared_roster_ldap.memberattr_format`
* **Syntax:** string
* **Default:** `"%u"`
* **Example:** `memberattr_format = "%u"`

Simple LDAP expression for extracting a user ID.

### `modules.mod_shared_roster_ldap.memberattr_format_re`
* **Syntax:** string
* **Default:** `""`
* **Example:** `memberattr_format_re = ""`

Allows extracting the user ID with a regular expression.

## Options: parameters

### `modules.mod_shared_roster_ldap.auth_check`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `auth_check = true`

Enables checking if a shared roster entry actually exists in the XMPP database.

### `modules.mod_shared_roster_ldap.user_cache_validity`
* **Syntax:** positive integer
* **Default:** `300`
* **Example:** `user_cache_validity = 300`

Specifies in seconds how long are the roster entries kept in the cache.

### `modules.mod_shared_roster_ldap.group_cache_validity`
* **Syntax:** positive integer
* **Default:** `300`
* **Example:** `group_cache_validity = 300`

Specifies in seconds how long is the user's membership in a group kept in the cache.

### `modules.mod_shared_roster_ldap.user_cache_size`
* **Syntax:** positive integer
* **Default:** `1000`
* **Example:** `user_cache_size = 1000`

Specifies how many shared roster items are kept in the cache.

### `modules.mod_shared_roster_ldap.group_cache_size`
* **Syntax:** positive integer
* **Default:** `1000`
* **Example:** `group_cache_size = 1000`

Specifies how many roster group entries are kept in cache.

## Options: LDAP filters

### `modules.mod_shared_roster_ldap.rfilter`
* **Syntax:** string
* **Default:** `""`
* **Example:** `rfilter = ""`

Used to find names of all shared roster groups.

### `modules.mod_shared_roster_ldap.gfilter`
* **Syntax:** string
* **Default:** `""`
* **Example:** `gfilter = ""`

Used for retrieving the human-readable name and the members of a group.

### `modules.mod_shared_roster_ldap.ufilter`
* **Syntax:** string
* **Default:** `""`
* **Example:** `ufilter = ""`

Used for retrieving the human-readable name of the roster entries.

### `modules.mod_shared_roster_ldap.filter`
* **Syntax:** string
* **Default:** `""`
* **Example:** `filter = "(objectClass=inetOrgPerson)"`

Filter AND-ed with previous filters.

## Example Configuration

```toml
[modules.mod_shared_roster_ldap]
  base = "ou=Users,dc=ejd,dc=com"
  groupattr = "ou"
  memberattr = "cn"
  userdesc = "cn"
  filter = "(objectClass=inetOrgPerson)"
  rfilter = "(objectClass=inetOrgPerson)"
  group_cache_validity = 1
  user_cache_validity = 1
```
