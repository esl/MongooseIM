### Module Description

`mod_keystore` serves as storage for crypto keys - it doesn't implement
any XMPP-level protocol.
The module can store _transient RAM-only keys_ - generated on module
startup, stored in memory only, distributed to all cluster members
and existing for only as long as the cluster is alive - as well as predefined
and pre-shared keys which can be read from a file.

RAM-only keys provide better security since they are never written to persistent
storage - at the cost of loss in case of a cluster-global failure or restart.

As of now [`mod_auth_token`](mod_auth_token) is the only module
dependent on `mod_keystore`.

### Example Configuration

It's crucial to understand the distinction between single-tenant and
multi-tenant hosting scenarios.
In a multi-tenant server **`mod_keystore` must be configured separately
for each virtual XMPP domain to avoid sharing keys between domains!**

Simple configuration - single tenant (i.e. server hosting just one XMPP domain):

```erlang
{mod_keystore, [{keys, [{access_secret, ram},
                        {access_psk,    {file, "priv/access_psk"}},
                        {provision_psk, {file, "priv/provision_psk"}}]}]}

```

Multi-tenant setup (`mod_keystore` configured differently
for each virtual XMPP domain):

```
{host_config, "first.com",
 [
  {modules,
   [
    {mod_keystore, [ {keys, [{access_secret, ram},
                             {access_psk,    {file, "priv/first_access_psk"}},
                             {provision_psk, {file, "priv/first_provision_psk"}}]}
                   ]}
   ]}
 ]}.

{host_config, "second.com",
 [
  {modules,
   [
    {mod_keystore, [ {keys, [{access_secret, ram},
                             {access_psk,    {file, "priv/second_access_psk"}},
                             {provision_psk, {file, "priv/second_provision_psk"}}]}
                   ]}
   ]}
 ]}.
```

### Options

*   **key_size** - size to use when generating RAM-only keys (designated
    by type `ram`)

*   **keys** - list of _specifiers_ of keys which will be provided by the
    module at runtime

Each _key specifier_ is a pair of `{KeyName, KeyType}`, where:

*   `KeyName` is any Erlang term. For simplicity's sake atoms are advised.
    Names have to be unique in the context of one virtual domain.

*   `KeyType` - one of `ram` or `{file, "path/to/file"}`.
    The file is read and its contents are provided
    as the key (whitespace is trimmed).

### API

The module public API is hook-based:

```erlang
ejabberd_hooks:run_fold(get_key, Domain, [], [{KeyName, Domain}]).
```

However, a local function hiding the hook call might come in handy:

```erlang
-spec get_key(Domain, KeyName) -> Result when
      Domain :: ejabberd:server(),
      KeyName :: mod_keystore:key_name(),
      Result :: mod_keystore:key_list().
get_key(Domain, KeyName) ->
    ejabberd_hooks:run_fold(get_key, Domain, [], [{KeyName, Domain}]).
```
