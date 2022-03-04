## Overview

This authentication method provides a read-only abstraction over an LDAP directory.

The following SASL mechanisms are supported:

### SASL EXTERNAL

User credentials are verified by performing an LDAP search with the user name provided by the client. This can be used to verify that the user is allowed to log in after the provided certificate has been verified.

This method requires one connection pool with the `default` tag (unless you change it with the `pool_tag` option). You need to provide the root DN and password unless your LDAP password allows anonymous searches.

Example:

```toml
[outgoing_pools.ldap.default]
  workers = 5
  connection.servers = ["ldap-server.example.com"]
  connection.rootdn = "cn=admin,dc=example,dc=com"
  connection.password = "ldap-admin-password"
```

For more details see [outgoing connections](../configuration/outgoing-connections.md).

### SASL PLAIN

User credentials are verified by performing an LDAP search followed by a bind with the user name and password provided by the client.

To use SASL PLAIN, you need to configure two connection pools:

* one with the `default` tag (unless you change it with the `pool_tag` option) for the search operations (like for SASL EXTERNAL),
* one with the `bind` tag (unless you change it with the `bind_pool_tag` option) for the bind operations - for this one it is not necessary to provide the root DN and password as the bind operations will be performed with users' credentials. This pool has to be used exclusively for the bind operations as the authentication state of the connection changes with each request.

Example:

```toml
[outgoing_pools.ldap.default]
  workers = 5
  connection.servers = ["ldap-server.example.com"]
  connection.rootdn = "cn=admin,dc=example,dc=com"
  connection.password = "ldap-admin-password"

[outgoing_pools.ldap.bind]
  connection.servers = ["ldap-server.example.com"]
```

For more details see [outgoing connections](../configuration/outgoing-connections.md).

## Configuration options

### `auth.ldap.pool_tag`
* **Syntax:** string
* **Default:** `"default"`
* **Example:** `pool_tag = "my_pool"`

Specifies the tag for the primary outgoing connection pool for LDAP authentication.

### `auth.ldap.bind_pool_tag`
* **Syntax:** string
* **Default:** `"bind"`
* **Example:** `bind_pool_tag = "my_bind_pool"`

Specifies the tag for the secondary outgoing connection pool for LDAP authentication, used for operations requiring the `bind` operations, such as checking passwords.

### `auth.ldap.base`
* **Syntax:** string
* **Default:** no default, this option is mandatory
* **Example:** `base = "ou=Users,dc=example,dc=com"`

LDAP base directory which stores user accounts.

### `auth.ldap.uids`
* **Syntax:** array of TOML tables with the following content:
    * `attr` - string, mandatory, name of the attribute
    * `format` - pattern, default: `"%u"`, requires `attr`
* **Default:** `[{attr = "uid"}]`
* **Example:** `uids = [{attr = "uid", format = "%u@example.org"}, {attr = "another_uid"}]`

List of LDAP attributes that contain the user name (user's part of the JID), used to search for user accounts.
They are used as alternatives - it is enough if one of them contains the name.
By default the whole value of the attribute is expected to be the user name.
If this is not the case, use the `format` option.
It must contain one and only one pattern variable `%u` which will be replaced by the user name.

### `auth.ldap.filter`
* **Syntax:** string
* **Default:** not set
* **Example:** `filter = "(&(objectClass=shadowAccount)(memberOf=Jabber Users))"`

An additional LDAP filter used to narrow down the search for user accounts.
Do not forget to close the brackets and do not use superfluous whitespaces as this expression is processed before sending to LDAP - the match for user name (see `ldap.uids`) is added automatically.

### `auth.ldap.dn_filter`
* **Syntax:** TOML table with the following content:
    * `filter` - string (LDAP filter), mandatory
    * `attributes` - array of strings (attribute names)
* **Default:** not set
* **Example:** `dn_filter = {filter = "(&(name=%s)(owner=%D)(user=%u@%d))", attributes = ["sn"]}`

This filter is applied to the results returned by the main filter.
It performs an additional LDAP lookup to provide the complete result.
This is useful when you are unable to define all filter rules in `ldap.filter`.
You can define `%u`, `%d`, `%s` and `%D` pattern variables in the filter:

* `%u` is replaced by the user’s part of a JID,
* `%d` is replaced by the corresponding domain (virtual host),
* `%s` variables are consecutively replaced by values of the attributes listen as `attributes`
* `%D` is replaced by the Distinguished Name.

Since this filter makes additional LDAP lookups, use it only as the last resort; try to define all filter rules in `ldap.filter` if possible.

### `auth.ldap.local_filter`
* **Syntax:** TOML table with the following content:
    * `operation` - string, mandatory, `"equal"` or `"notequal"`
    * `attribute` - string, mandatory, LDAP attribute
    * `values` - array of strings (attribute values)
* **Default:** not set
* **Example:** `local_filter = {operation = "equal", attribute = "accountStatus", values = ["enabled"]}`

If you can’t use the `ldap.filter` due to performance reasons (the LDAP server has many users registered), you can use this local filter.
The local filter checks an attribute in MongooseIM, not in LDAP, so this limits the load on the LDAP directory.

The example above shows a filter which matches accounts with the "enabled" status.
Another example is shown below - it matches any account that is neither "disabled" nor "blacklisted".
It also shows the usage of TOML dotted keys, which is recommended when the inline table grows too big.

```toml
   local_filter.operation = "notequal"
   local_filter.attribute = "accountStatus"
   local_filter.values = ["disabled", "blacklisted"]
```

### `auth.ldap.deref`
* **Syntax:** string, one of: `"never"`, `"always"`, `"finding"`, `"searching"`
* **Default:** `"never"`
* **Example:** `deref = "always"`

Specifies whether or not to dereference aliases: `finding` means to dereference only when finding the base and `searching` - only when performing the LDAP search. See the documentation on [LDAP search operation](https://ldap.com/the-ldap-search-operation/) for more information.

### Example

```toml
[auth.ldap]
  base = "ou=Users,dc=example,dc=com"
  filter = "(objectClass=inetOrgPerson)"
```
