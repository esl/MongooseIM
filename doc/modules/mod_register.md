### Module Description
This module implements [XEP-0077: In-Band Registration](http://xmpp.org/extensions/xep-0077.html), allowing users to register accounts on the server via XMPP. Use of this module on Internet-facing servers is **not recommended**.

### Options

* `iqdisc` (default: `one_queue`)
* `access` (atom, default: `all`): Defines which ACL should be used for checking if a chosen username is allowed for registration.
* `welcome_message` (`{Subject :: string(), Body :: string()}`, default: `{"", ""}`): Body and subject of a `<message>` stanza sent to new users.
* `registration_watchers` (list of binaries, default: `[]`): List of JIDs, which should receive a `<message>` notification about every successful registration.
* `password_strength` (non-negative integer, default: 0): Specifies minimal entropy of allowed password. 
 Entropy is measured with `ejabberd_auth:entropy/1`.
 Recommended minimum is 32.
 The entropy calculation algorithm is described in a section below.
* `ip_access` (list of `{deny|allow, StringIP|StringSubnet, default: `[]`): Access list for specified IPs or networks. 
 Default value allows registration from every IP.

### Example configuration

Allow registrations from localhost:
``` 
{mod_register, [{allow, "127.0.0.1"}]} 
```

Deny registration from network 10.20.0.0 with mask 255.255.0.0.
```
{mod_register, [{deny, "10.20.0.0/16"}]}
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[Host, modRegisterCount]` | spiral | A user registers via `mod_register` module. |
| `[Host, modUnregisterCount]` | spiral | A user unregisters via `mod_register` module. |

### Entropy calculation algorithm

```
Entropy = length(Password) * log(X) / log(2)
```

Where `X` is initially set to 0 and certain values are added if at least one of these bytes are present:

* Lower case character: 26
* Upper case character: 26
* Digit: 9
* Printable ASCII (0x21 - 0x7e): 33
* Any other value: 128

*Note:* These values are added only once, no matter how many bytes of specific type are found.

#### Example entropies:

* `kotek`: ~23.5
* `abc123`: ~30.8
* `L33tSp34k`: ~53.4
* `CamelCase`: ~51.3
* `lowUP1#:`: ~45.9
* `lowUP1#❤`: ~78

