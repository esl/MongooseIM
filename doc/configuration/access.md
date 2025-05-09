The `access` section is used to define **access rules** which return specific values for specific access classes.

* **Syntax:** each access rule is a key-value pair, where:
    * Key is the name of the rule,
    * Value is a TOML array of rule clauses - TOML tables, whose format is described below.
* **Default:** no default - each access rule needs to be specified explicitly.
* **Example:** see the [examples](#rule-examples) below.

## Access rule clauses

Whenever a rule is checked to obtain the resulting value for a user, the clauses are traversed one by one until a matching one is found or the list is exhausted (in which case the special value `deny` is returned).

Each clause has to contain the following keys:

### `access.*.acl`

* **Syntax:** string
* **Example:** `acl = "local"`

The access class defined in the `acl` section. The user is matched against it. The special name `all` is a catch-all value that matches any user. If the class does not exist, the clause does not match (there is no error).

### `access.*.value`

* **Syntax:** string or integer
* **Example:** `value = "allow"`

For rules determining access, the value will be `"allow"` or `"deny"`. For other rules it can be an integer or a string.

## Rule examples

The following access rules are already defined in the example configuration file.

### C2S Access

The `c2s` rule is used to allow/deny the users to establish C2S connections:

```toml
  c2s = [
    {acl = "blocked", value = "deny"},
    {acl = "all", value = "allow"}
  ]
```

It has the following logic:

* if the access class is `blocked`, the returned value is `"deny"`,
* otherwise, the returned value is `"allow"`.

The `blocked` access class can be defined in the [`acl` section](acl.md) and match blacklisted users.

For this rule to take effect, it needs to be referenced in the options of a [C2S listener](../listeners/listen-c2s.md#listenc2saccess).

### MUC

The following rules manage the permissions of MUC operations:

```toml
  muc_admin = [
    {acl = "admin", value = "allow"}
  ]

  muc_create = [
    {acl = "local", value = "allow"}
  ]

  muc = [
    {acl = "all", value = "allow"}
  ]
```

They are referenced in the options of the [`mod_muc`](../modules/mod_muc.md) module.

### Registration

This rule manages the permissions to create new users with `mod_register`.

```toml
  register = [
    {acl = "all", value = "allow"}
  ]
```

It needs to be referenced in the options of the [`mod_register`](../modules/mod_register.md) module.

### MAM permissions

These rules set the permissions for MAM operations triggered by IQ stanzas and handled by the [`mod_mam`](../modules/mod_mam.md) module.

```toml
  mam_set_prefs = [
    {acl = "all", value = "default"}
  ]

  mam_get_prefs = [
    {acl = "all", value = "default"}
  ]

  mam_lookup_messages = [
    {acl = "all", value = "default"}
  ]
```

They can return `"allow"`, `"deny"` or `"default"`.
The last value uses the default setting for the operation, which is to allow the operation when the sender and recipient JID's are the same.

MAM for MUC permissions has `muc_` prefix:

```toml
  muc_mam_set_prefs = [
    {acl = "all", value = "default"}
  ]

  muc_mam_get_prefs = [
    {acl = "all", value = "default"}
  ]

  muc_mam_lookup_messages = [
    {acl = "all", value = "default"}
  ]
```

### MAM shapers

These rules limit the rate of MAM operations triggered by IQ stanzas.

```toml
  mam_set_prefs_shaper = [
    {acl = "all", value = "mam_shaper"}
  ]

  mam_get_prefs_shaper = [
    {acl = "all", value = "mam_shaper"}
  ]

  mam_lookup_messages_shaper = [
    {acl = "all", value = "mam_shaper"}
  ]

  mam_set_prefs_global_shaper = [
    {acl = "all", value = "mam_global_shaper"}
  ]

  mam_get_prefs_global_shaper = [
    {acl = "all", value = "mam_global_shaper"}
  ]

  mam_lookup_messages_global_shaper = [
    {acl = "all", value = "mam_global_shaper"}
  ]
```

For each operation there are two rules:

- `*_shaper` - limits the number of operations per user connection per second,
- `*_global_shaper` - limits the number of operations per server node per second.

The values returned by the rules (`mam_shaper`, `mam_global_shaper`) are shaper names, which need to be defined in the [`shaper` section](shaper.md#mam-shapers).

MAM for MUC shapers has `muc_` prefix.

### Maximum number of sessions

The `max_user_sessions` rule is used to determine the maximum number of sessions a user can open.

```toml
  max_user_sessions = [
    {acl = "all", value = 10}
  ]
```

By default, all users can open at most 10 concurrent sessions.

### Maximum number of offline messages

The `max_user_offline_messages` rule is used to determine the maximum number of messages that is stored for a user by the [`mod_offline` module](../modules/mod_offline.md).

```toml
  max_user_offline_messages = [
    {acl = "admin", value = 5000},
    {acl = "all", value = 100}
  ]
```

It has the following logic:

* if the access class is `admin`, the returned value is `5000`,
* otherwise, the returned value is `100`.

This means that the admin users can have 5000 messages stored offline, while the others can have at most 100.
The `admin` access class can be defined in the [`acl` section](acl.md).

## For developers

To access the rule functionality, one has to use the `acl:match_rule/3` function.

Given the following rule:

```toml
  register = [
    {acl = "all", value = "deny"}
  ]
```

One can call:

`acl:match_rule(<<"localhost">>, register, jid:make(<<"p">>, <<"localhost">>, <<>>)).`

Which in our case will return `deny`.
If the rule is not host specific, one can use `global` instead of `<<"localhost">>`.
