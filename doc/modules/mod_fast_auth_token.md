## Module Description

This module implements [XEP-0484: Fast Authentication Streamlining Tokens](https://xmpp.org/extensions/xep-0484.html).
It provides services necessary to:

* issue auth tokens for authenticated users;
* reconnect to the server using the tokens instead of the original auth method.

Tokens are stored in RDBMS.

It is not related to another similar module `mod_auth_token`.

## Options

### `modules.mod_fast_auth_token.backend`
* **Syntax:** non-empty string
* **Default:** `"rdbms"`
* **Example:** `backend = "rdbms"`

Token storage backend. Currently only `"rdbms"` is supported.

### `modules.mod_fast_auth_token.validity_period`
* **Syntax:** TOML table. Each key is either `access` or `rotate_before_expire`.Each value is a nested TOML table with the following mandatory keys: `value` (non-negative integer) and `unit` (`"days"`, `"hours"`, `"minutes"` or `"seconds"`).
* **Default:** `{access = {value = 3, unit = "days"}, rotate_before_expire = {value = 6, unit = "hours"}}`
* **Example:** `validity_period.access = {value = 30, unit = "minutes"}`

The user can use each token for `access` period of time before it expired.

The server would [send](https://xmpp.org/extensions/xep-0484.html#token-rotation)
a new token at the login time `rotate_before_expire` time before it expires.
Set it to 0 to disable automatic rotation.

## Example configuration

```toml
[modules.mod_fast_auth_token]
  validity_period.access = {value = 1, unit = "days"}
  validity_period.rotate_before_expire = {value = 0, unit = "days"}
```

## 0-RTT Support

For support, set `session_tickets = "stateless"` and `early_data = true` for
the C2S listener on port 5223 (Direct TLS).

Implementation details:

> Servers MUST reject any authentication requests received via TLS 0-RTT payloads
> that do not include a 'count' attribute, or where the count is less than or equal
> to a count that has already been processed for this token.
> This protects against replay attacks that 0-RTT is susceptible to.

Checking this is impossible on the server side, so the client should ensure that
`count` is set.
