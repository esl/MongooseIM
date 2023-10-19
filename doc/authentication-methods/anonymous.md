## Overview

This authentication method allows the users to connect anonymously.

## Configuration options

### `auth.anonymous.allow_multiple_connections`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `allow_multiple_connections = true`

When set to true, allows multiple connections from the same JID using the `anonymous` authentication method.

### `auth.anonymous.protocol`
* **Syntax:** string, one of `"sasl_anon"`, `"login_anon"`, `"both"`
* **Default:** `sasl_anon`
* **Example:** `protocol = "both"`

Specifies the SASL mechanisms supported by the `anonymous` authentication method:

* `sasl_anon` - support only the the `ANONYMOUS` mechanism,
* `login_anon` - support the non-anonymous mechanisms (`PLAIN`, `DIGEST-MD5`, `SCRAM-*`),
* `both` - support both types of mechanisms.

### `auth.anonymous.backend`
* **Syntax:** string, one of `mnesia`, `cets`
* **Default:** `mnesia`
* **Example:** `backend = cets`

Sets the backend where anonymous sessions will be stored in-memory. See [internal databases](../configuration/internal-databases.md)

### Example

```toml
[auth.anonymous]
  allow_multiple_connections = true
  protocol = "both"
```
