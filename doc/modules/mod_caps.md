## Module description

This module provides a presence-based mechanism for exchanging information about entity capabilities as defined in [XEP-0115: Entity Capabilities](https://xmpp.org/extensions/xep-0115.html).
By storing the clients' capabilities, it allows [filtered notifications](https://xmpp.org/extensions/xep-0163.html#approach-filter) in PEP.
It is not this module's responsibility to intercept and answer disco requests routed between clients.

## Options

### `modules.mod_caps.backend`
* **Syntax:** string; only `"cets"` is allowed.
* **Default:** `"cets"`
* **Example:** `backend = "cets"`

The backend used to store entity capabilities. Currently only CETS is supported.

!!! Warning
    The corresponding [internal database](../configuration/internal-databases.md) has to be enabled.

### `modules.mod_caps.iq_response_timeout`
* **Syntax:** positive integer (milliseconds)
* **Default:** `5000` (5 seconds)
* **Example:** `iq_response_timeout = 10_000`

Maximum waiting time for the client's response to a service discovery query.

## Example Configuration

```toml
[modules.mod_caps]
  iq_response_timeout = 10_000
```
