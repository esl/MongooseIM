The `host_config` section is used to configure options for specific XMPP domains or for host types, which are used to group multiple domains.
For each domain or host type requiring such options, a `host_config` section needs to be created with the following format:

* **Syntax:** domain subsection starts with `[[host_config]]` and contains the options listed below.
* **Default:** none - all domain-level options need to be specified explicitly.
* **Example:** see the examples for each section below.

!!! Note
    Each hosted domain needs to be included in the list of [`hosts`](general.md#generalhosts) in the `general` section.
    Similarly, each host type needs to be included in [`general.host_types`](general.md#generalhost_types).

## General options

### `host_config.host`

* **Syntax:** string, domain name
* **Default:** no default, either this option or `host_config.host_type` is mandatory
* **Example:** `host = "my-xmpp-server.com"`

This option specifies the XMPP domain that this section refers to.

### `host_config.host_type`

* **Syntax:** string, host type name
* **Default:** no default, either this option or `host_config.host` is mandatory
* **Example:** `host_type = "first type"`

This option specifies the host type that this section refers to.

## Configuration sections

The following sections are accepted in `host_config`:

### `host_config.general`

The options defined here override the ones defined in the top-level [`general`](general.md) section.
The following options are allowed:

* [`route_subdomains`](general.md#generalroute_subdomains)
* [`replaced_wait_timeout`](general.md#generalreplaced_wait_timeout)

#### Example

The `replaced_wait_timeout` option is set to `2000` only for `domain2.com`.

```toml
[general]
  hosts = ["domain1.com", "domain2.com", "domain3.com"]
  loglevel = "info"
  replaced_wait_timeout = 1000

[[host_config]]
  host = "domain2.com"

  [host_config.general]
    replaced_wait_timeout = 2000
```

### `host_config.auth`

This section completely overrides the top-level [`auth`](auth.md) section, all options are allowed.

#### Example

In the example below the number of `scram_iterations` is increased for `domain2`.
It is necessary to put `methods` and `password.hash` and there as well, as otherwise they would not be set for `domain2`.

```toml
[general]
  hosts = ["domain1.com", "domain2.com", "domain3.com"]

[auth]
  methods = ["rdbms"]
  password.hash = ["sha256"]

[[host_config]]
  host = "domain2.com"

  [host_config.auth]
    methods = ["rdbms"]
    password.hash = ["sha256"]
    scram_iterations = 40_000
```

### `host_config.modules`

This section completely overrides the top-level [`modules`](Modules.md) section.
Remember that only the modules supporting dynamic domains are allowed if you are specifying options for a host type.
The ones that do not support it can be found in the [modules list](./Modules.md#modules-incompatible-with-dynamic-domains).

#### Example

The modules enabled for `domain2.com` will be `mod_disco` and `mod_stream_management`.
If we wanted to enable `mod_roster`, it would need to be repeated in `host_config`.

```toml
[general]
  hosts = ["domain1.com", "domain2.com", "domain3.com"]

[modules.mod_disco]
  users_can_see_hidden_services = false

[modules.mod_roster]
  backend = "rdbms"

[[host_config]]
  host = "domain2.com"

  [host_config.modules.mod_disco]
    users_can_see_hidden_services = false

  [host_config.modules.mod_stream_management]
```

### `host_config.outgoing_pools`

This section overrides any pool with the same type and tag that was defined in the top-level [`outgoing_pools`](outgoing-connections.md) section.
If we wanted to enable a `default` `rdbms` pool only for `"host-type-basic"` for example, we could do so as follows:

```toml
[general]
  host_type = ["host-type-basic", "host-type-advanced", "host-type-privacy"]

[[host_config]]
  host = "host-type-basic"

  [outgoing_pools.rdbms.default]
    workers = 5
    [outgoing_pools.rdbms.default.connection]
    ...
```

Configuration for such pools is all the same, except that the `scope` key is here disallowed.

### `host_config.acl`

The access classes defined here are merged with the ones defined in the top-level [`acl`](acl.md) section - when a class is defined in both places, the result is a union of both classes.

#### Example

The `blocked` access class is extended for `host_config` by adding `hacker2`.

```toml
[general]
  hosts = ["domain1.com", "domain2.com", "domain3.com"]

[acl]
  blocked = [
    {user = "spammer"},
    {user = "hacker1"}
  ]

[[host_config]]
  host = "domain2.com"

  [host_config.acl]
    blocked = [
      {user = "hacker2"}
    ]
```

### `host_config.access`

The access rules defined here are merged with the ones defined in the top-level [`access`](access.md) section:
When a rule is defined in both places:

* If the top-level rule ends with a catch-all clause `{acl = "all", value = "allow"}`, the resulting domain-specific rule has the clauses from **both** rules with the domain-specific clauses inserted after the top-level ones, but before the catch-all clause.
* If the top-level rule does not end with a catch-all clause, the resulting domain-specific rule has the clauses from **both** rules with the domain-specific clauses inserted after the top-level ones.

#### Example

The `c2s` access rule defined at the top level allows anyone to connect.
However, the rule for `domain2.com` is extended to prevent the `blocked` users from connecting:

```toml
[general]
  hosts = ["domain1.com", "domain2.com", "domain3.com"]

[access]
  c2s = [
    {acl = "admin", value = "allow"},
    {acl = "all", value = "allow"}
  ]

[[host_config]]
  host = "domain2.com"

  [host_config.access]
    c2s = [
      {acl = "blocked", value = "deny"}
    ]

    register = [
      {acl = "all", value = "deny"}
    ]
```

The resulting rule for `domain2.com` could be written as:

```toml
c2s = [
  {acl = "admin", value = "allow"},
  {acl = "blocked", value = "deny"},
  {acl = "all", value = "allow"}
]
```

The `register` rule is defined only for `domain2.com`.

!!! Note
    Some access rules are checked outside of the context of any domain, e.g. the [access rule for external components](listen.md#listenserviceaccess) - defining them in `host_config` would have no effect.

### `host_config.s2s`

This section completely overrides the top-level [`s2s`](s2s.md) section, all options are allowed.

#### Example

The `host_policy` option is changed for `domain2.com`:

```toml
[general]
  hosts = ["domain1.com", "domain2.com", "domain3.com"]

[s2s]
  default_policy = "deny"

  host_policy = [
    {host = "good-xmpp.org", policy = "allow"},
    {host = "bad-xmpp.org", policy = "deny"}
  ]

[[host_config]]
  host = "domain2.com"

  [host_config.s2s]
    host_policy = [
      {host = "bad-xmpp.org", policy = "allow"},
      {host = "evil-xmpp.org", policy = "deny"}
    ]
```

Note that `default_policy` for `domain2.com` has the default value `allow`, because `host_config.s2s` completely overrides the top-level `s2s` section, and all options are reset to the respective default values, unless they are explicitly changed.
