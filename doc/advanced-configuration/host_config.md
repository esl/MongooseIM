The `host_config` section is used to configure options for specific XMPP domains.
For each domain requiring such options, a `host_config` section needs to be created with the following format:

* **Scope:** for each option the scope is the same as for the corresponding top-level option.
* **Syntax:** domain subsection starts with `[[host_config]]` and contains the options listed below.
* **Default:** none - all domain-level options need to be specified explicitly.
* **Example:** see the examples for each section below.

**Note:** Each hosted domain needs to be included in the list of [`hosts`](general.md#generalhosts) in the `general` section.

# General options

### `host_config.host`

* **Syntax:** string, domain name
* **Default:** no default, this option is mandatory
* **Example:** `host = "my-xmpp-server.com"`

This option specifies the XMPP domain that this section refers to.

# Configuration sections

The following sections are accepted in `host_config`:

## `host_config.general`

The options defined here override the ones defined in the top-level [`general`](general.md) section.
The following options are allowed:

* [`pgsql_users_number_estimate`](general.md#generalpgsql_users_number_estimate)
* [`route_subdomains`](general.md#generalroute_subdomains)
* [`replaced_wait_timeout`](general.md#generalreplaced_wait_timeout)
* [`hide_service_name`](general.md#generalhide_service_name)

#### Example

The `hide_service_name` option is set to `false` only for `domain2.com`.

```toml
[general]
  hosts = ["domain1.com", "domain2.com", "domain3.com"]
  loglevel = "info"
  hide_service_name = true
  replaced_wait_timeout = 1000

[[host_config]]
  host = "domain2.com"

  [host_config.general]
    hide_service_name = false
```

## `host_config.auth`

This section overrides the top-level [`auth`](auth.md) section, all options are allowed.
It is recommended to repeat all top-level options in the domain-specific section as the rule is quite complicated:

- If you specify any of the following options, **all** of the following options will be overridden:
    - [`sasl_external`](auth.md#authsasl_external)
    - [`password.*`](auth.md#password-related-options)
    - [`scram_iterations`](auth.md#authscram_iterations)
    - [`external.program`](../../authentication-methods/external/#authexternalprogram)
    - [`ldap.*`](../../authentication-methods/ldap)
    - [`jwt.*`](../../authentication-methods/jwt)
    - [`riak.*`](../../authentication-methods/riak)
    - [`http.*`](../../authentication-methods/http)
- If you specify any of the following options, only these options will be overridden:
    - [`methods`](auth.md#authmethods)
    - [`sasl_mechanisms`](auth.md#authsasl_mechanisms)
    - [`external.instances`](../../authentication-methods/external/#authexternalinstances)
    - [`anonymous.*`](../../authentication-methods/anonymous)

#### Example

In the example below the number of `scram_iterations` is increased for `domain2`.
It is necessary to put the `password.hash` there as well, as otherwise it would be replaced with the default setting.
However, specifying `methods` is not necessary as this value will not be changed.

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

The last section would work the same without `methods`:

```toml
  [host_config.auth]
    password.hash = ["sha256"]
    scram_iterations = 40_000
```

## `host_config.modules`

This section completely overrides the top-level [`modules`](modules.md) section. All options are allowed.

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

## `host_config.acl`

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

## `host_config.access`

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

**Note:** some access rules are checked outside of the context of any domain, e.g. the [access rule for external components](listen.md#listenserviceaccess) - defining them in `host_config` would have no effect.

## `host_config.s2s`

The options defined here override the ones defined in the top-level [`s2s`](s2s.md) section.
The following options are allowed:

* [`default_policy`](s2s.md#s2sdefault_policy)
* [`host_policy`](s2s.md#s2shost_policy) - overrides the top-level setting host by host
* [`shared`](s2s.md#s2sshared)
* [`max_retry_delay`](s2s.md#s2smax_retry_delay)

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

The resulting `host_policy` for `domain2.com` is the following:

```toml
host_policy = [
  {host = "good-xmpp.org", policy = "allow"},
  {host = "bad-xmpp.org", policy = "allow"},
  {host = "evil-xmpp.org", policy = "deny"}
]
```

The `default_policy` is still `deny`.
