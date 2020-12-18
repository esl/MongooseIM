## Module Description

`mod_keystore` serves as storage for crypto keys - it doesn't implement
any XMPP-level protocol.
The module can store _transient RAM-only keys_ generated on module
startup, stored in memory only, distributed to all cluster members
and existing for only as long as the cluster is alive, as well as predefined
and pre-shared keys which can be read from a file.

RAM-only keys provide better security since they are never written to persistent
storage, at the cost of loss in case of a cluster-global failure or restart.

As of now [`mod_auth_token`](mod_auth_token.md) is the only module
dependent on `mod_keystore`.

It's crucial to understand the distinction between single-tenant and
multi-tenant hosting scenarios.
In a multi-tenant server **`mod_keystore` must be configured separately
for each virtual XMPP domain to avoid sharing keys between domains!**

## Options

### `modules.mod_keystore.ram_key_size`
* **Syntax:** non-negative integer
* **Default:** `2048`
* **Example:** `ram_key_size = 10000`

Size to use when generating RAM-only keys (designated by type `ram`).

### `modules.mod_keystore.keys`
* **Syntax:** Array of TOML tables with the following keys: `"name"`, `"type"`, `"path"`, and following values: {name = `string`, type = `values: "file", "ram"`, path = `string`}.
* **Default:** `[]`
* **Example:** `modules.mod_keystore.keys = [{name = "access_psk", type = "file", path = "priv/access_psk"}]`

Names, types, and optional filepaths of the keys.

## API

The module public API is hook-based:

```erlang
mongoose_hooks:get_key(Domain, [], KeyName).
```

An example of usage can be found in [mod_auth_token:get_key_for_user/2](https://github.com/esl/MongooseIM/blob/4.0.0/src/mod_auth_token.erl#L393).

## Example Configuration

Simple configuration - single tenant (i.e. server hosting just one XMPP domain):

```toml
[modules.mod_keystore]
  keys = [{name = "access_secret", type = "ram"},
          {name = "access_psk", type = "file", path = "priv/access_psk"},
          {name = "provision_psk", type = "file", path = "priv/provision_psk"}]
```

Multi-tenant setup (`mod_keystore` configured differently
for each virtual XMPP domain):

```toml
[[host_config]]
  host = "first.com"
  
  [host_config.modules.mod_keystore]
    keys = [{name = "access_secret", type = "ram"},
            {name = "access_psk", type = "file", path = "priv/first_access_psk"},
            {name = "provision_psk", type = "file", path = "priv/first_provision_psk"}]

[[host_config]]
  host = "second.com"
  
  [host_config.modules.mod_keystore]
    keys = [{name = "access_secret", type = "ram"},
            {name = "access_psk", type = "file", path = "priv/second_access_psk"},
            {name = "provision_psk", type = "file", path = "priv/second_provision_psk"}]
```
