## Overview

An LDAP authentication module.
It provides a read-only abstraction over an LDAP directory.

The following SASL methods are supported:

### SASL EXTERNAL

User credentials are verified by performing an LDAP search with the user name provided by the client. This can be used to verify that the user is allowed to log in after the provided certificate has been verified.

This method requires one connection pool with the `default` tag (unless you change it with the `ldap_pool_tag` option). You need to provide the root DN and password unless your LDAP password allows anonymous searches.

Example:

```erlang
{outgoing_pools, [
 {ldap, host, default, [{workers, 5}],
  [{servers, ["ldap-server.example.com"]},
   {rootdn, "cn=admin,dc=example,dc=com"},
   {password, "ldap-admin-password"}]
 }
]}.
```

For more details see [outgoing connections](../advanced-configuration/outgoing-connections.md).

### SASL PLAIN

User credentials are verified by performing an LDAP search followed by a bind with the user name and password provided by the client.

To use SASL PLAIN, you need to configure two connection pools:

* one with the `default` tag (unless you change it with the `ldap_pool_tag` option) for the search operations (like for SASL EXTERNAL),
* one with the `bind` tag (unless you change it with the `ldap_bind_pool_tag` option) for the bind operations - for this one it is not necessary to provide the root DN and password as the bind operations will be performed with users' credentials. This pool has to be used exclusively for the bind operations as the authentication state of the connection changes with each request.

Example:

```erlang
{outgoing_pools, [
 {ldap, host, default, [{workers, 5}],
  [{servers, ["ldap-server.example.com"]},
   {rootdn, "cn=admin,dc=example,dc=com"},
   {password, "ldap-admin-password"}]
 },
 {ldap, host, bind, [{workers, 5}],
  [{servers, ["ldap-server.example.com"]}]
 }
]}.
```

For more details see [outgoing connections](../advanced-configuration/outgoing-connections.md).

## Configuration options

The following options can be set in the `auth_opts` tuple in `mongooseim.cfg`.

* **ldap_pool_tag:**
    * **Description:** Worker pool tag for the search operations.
    * **Value:** Atom
    * **Default:** `default`

* **ldap_bind_pool_tag:**
    * **Description:** Worker pool tag for the search operations.
    * **Value:** Atom
    * **Default:** `bind`

* **ldap_base:**
    * **Description:** LDAP base directory which stores user accounts.
    * **Value:** String
    * **Default:** This option is required

* **ldap_uids:**
    * **Description:** An LDAP attribute holding a list of attributes to use as alternatives for getting the JID.
    The attributes take the following form: `[{ldap_uidattr}]` or `[{ldap_uidattr, ldap_uidattr_format}]`.
    You can use as many comma separated attributes as needed.
    * **Value:** `[ ldap_uidattr | {ldap_uidattr: ldap_uidattr_format} ]`
         * **ldap_uidattr:** An LDAP attribute holding the user’s part of a JID. The default value is `uid`.
         * **ldap_uidattr_format:** The format of the`ldap_uidattr` variable.
         It must contain one and only one pattern variable `%u` which will be replaced by the user’s part of a JID (example: `%u@example.org`).
         The default value is `%u`.
    * **Default** `[{uid, %u}]`

* **ldap_filter:**
    * **Description:** An LDAP filter.
    Please, do not forget to close the brackets and do not use superfluous whitespaces.
    Also do not use the `ldap_uidattr` attribute in the filter because it will be substituted in the LDAP filter automatically.
    * **Value:** String. For example: `"(&(objectClass=shadowAccount)(memberOf=Jabber Users))"`
    * **Default:** `undefined`

* **ldap_dn_filter:**
    * **Description:**  This filter is applied to the results returned by the main filter.
    It performs an additional LDAP lookup to provide the complete result.
    This is useful when you are unable to define all filter rules in the `ldap_filter`.
    You can define `%u`, `%d`, `%s` and `%D` pattern variables in the filter: `%u` is replaced by a user’s part of a JID, `%d` is replaced by the corresponding domain (virtual host), all `%s` variables are consecutively replaced by values of `FilterAttrs` attributes and `%D` is replaced by the Distinguished Name.
    Since this filter makes additional LDAP lookups, use it only as the last resort; try to define all filter rules in ldap_filter if possible.
    * **Value:** `{Filter, [FilterAttributes]}`. For example: `{"(&(name=%s)(owner=%D)(user=%u@%d))": ["sn"]}`
    * **Default:** `undefined`

* **ldap_local_filter:**
    * **Description:** If you can’t use the `ldap_filter` due to performance reasons (the LDAP server has many users registered), you can use this local filter.
    The local filter checks an attribute in MongooseIM, not in LDAP, so this limits the load on the LDAP directory.
    * **Value:** `Filter`. Example values:
    ```
    {ldap_local_filter, {notequal, {"accountStatus",["disabled"]}}}.
    {ldap_local_filter, {equal, {"accountStatus",["enabled"]}}}.
    {ldap_local_filter, undefined}.
    ```
    * **Default:** `undefined`

* **ldap_deref**
    * **Description:** Whether or not to dereference aliases
    * **Values:** `never`, `always`, `finding`, `searching`
    * **Default:** `never`

Example:

```erlang
{auth_opts, [
 {ldap_base, "ou=Users,dc=example,dc=com"},
 {ldap_filter, "(objectClass=inetOrgPerson)"}
]}.
```
