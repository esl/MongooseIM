## Module Description

This module implements [XEP-0077: In-Band Registration](http://xmpp.org/extensions/xep-0077.html), allowing users to register accounts on the server via XMPP. Use of this module on Internet-facing servers is **not recommended**.

## Options

### `modules.mod_register.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"one_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_register.access`
* **Syntax:** string, rule name or `"all"`
* **Default:** `"all"`
* **Example:** `access = "all"`

Defines which [access rule](../configuration/access.md#registration) should be used for checking if a chosen username is allowed for registration.

### `modules.mod_register.welcome_message`
* **Syntax:** TOML table with the following keys: `"body"`, `"subject"` and string values.
* **Default:** `{subject = "", body = ""}`
* **Example:** `welcome_message = {subject = "Hello from MIM!", body = "Message body."}`

Body and subject of a `<message>` stanza sent to new users. Only one of the fields (but non-empty) is mandatory for the message to be sent.

### `modules.mod_register.registration_watchers`
* **Syntax:** array of strings
* **Default:** `[]`
* **Example:** `registration_watchers = ["JID1", "JID2"]`

List of JIDs, which should receive a `<message>` notification about every successful registration.

### `modules.mod_register.password_strength`
* **Syntax:** non-negative integer
* **Default:** `0`
* **Example:** `password_strength = 32`

Specifies minimal entropy of allowed password.
Entropy is measured with `ejabberd_auth:entropy/1`.
When set to `0`, the password strength is not checked.
Recommended minimum is 32.
The entropy calculation algorithm is described in a section below.

### `modules.mod_register.ip_access`
* **Syntax:** Array of TOML tables with the following mandatory content:

    - `address` - string, IP address
    - `policy` - string, one of: `"allow"`, `"deny"`.

* **Default:** `[]`
* **Example:** `ip_access = [
  {address = "127.0.0.0/8", policy = "allow"},
{address = "0.0.0.0/0", policy = "deny"}
]`

Access list for specified IPs or networks.
Default value allows registration from every IP.

## Example configuration

Allow registrations from localhost:

```toml
[modules.mod_register]
  welcome_message = {subject = "Hello from MIM!", body = "Message body."}
  ip_access = [
    {address = "127.0.0.1", policy = "allow"}
  ]
  access = "register"
```

Deny registration from network 10.20.0.0 with mask 255.255.0.0.
```toml
[modules.mod_register]
  ip_access = [
    {address = "10.20.0.0/16", policy = "deny"}
  ]
```

## Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[Host, modRegisterCount]` | spiral | A user registers via `mod_register` module. |
| `[Host, modUnregisterCount]` | spiral | A user unregisters via `mod_register` module. |

## Entropy calculation algorithm

```
Entropy = length(Password) * log(X) / log(2)
```

Where `X` is initially set to 0 and certain values are added if at least one of these bytes are present:

* Lower case character: 26
* Upper case character: 26
* Digit: 9
* Printable ASCII (0x21 - 0x7e): 33
* Any other value: 128

!!! Note
    These values are added only once, no matter how many bytes of specific type are found.

### Example entropies

* `kotek`: ~23.5
* `abc123`: ~30.8
* `L33tSp34k`: ~53.4
* `CamelCase`: ~51.3
* `lowUP1#:`: ~45.9
* `lowUP1#❤`: ~78
