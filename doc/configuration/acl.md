The `acl` section is used to define **access classes** to which the connecting users are assigned. These classes are used in [access rules](access.md).

* **Syntax:** each access class is a key-value pair, where:
    * Key is the name of the access class,
    * Value is a TOML array of patterns - TOML tables, whose format is described below.
* **Default:** no default - each access class needs to be specified explicitly.
* **Example:** the `local` access class is used for the regular users connecting to the [C2S listener](listen.md#client-to-server-c2s-listenc2s). The pattern `{}` matches all users from the current server, because it is equivalent to `{match = "current_domain"}` (see below).

```toml
  local = [{}]
```

When there are multiple patterns listed, the resulting pattern will be the union of all of them.

## Patterns

Each pattern consists of one or more conditions, specified with the options listed below.
All defined conditions need to be satisfied for the pattern to be matched successfully.

### `acl.*.match`

* **Syntax:** string, one of: `"all"`, `"current_domain"`, `"any_hosted_domain"`, `"none"`
* **Default:** `"current_domain"`
* **Example:** `match = "all"`

By default only users from the *current domain* (the one of the server) are matched.
Setting it to `"any_hosted_domain"` results in matching users from all domains hosted by this server.
You can also set this option to `"all"`, extending the pattern to users from external domains.
This option makes a difference for some [access rules](access.md), e.g. MAM, MUC and registration ones.
Setting the option to `"none"` makes the pattern never match.

```toml
  everyone = [
    {match = "all"}
  ]
```

### `acl.*.user`

* **Syntax:** string
* **Example:** `user = "admin"`

Matches all JIDs with the specified user name.
The following class includes `alice@localhost`, but not `bob@localhost`:

```toml
  admin = [
    {user = "alice"},
    {user = "charlie"}
  ]
```

### `acl.*.server`

* **Syntax:** string
* **Example:** `server = "localhost"`

Matches all JIDs with the specified domain name.
The following class includes `alice@localhost`, but not `alice@xmpp.org`:

```toml
  localhost_users = [
    {server = "localhost"}
  ]
```

This option can be combined with `user` - only `alice@localhost` belongs to the following class:

```toml
  admin = [
    {user = "alice", server = "localhost"}
  ]
```

### `acl.*.resource`

* **Syntax:** string
* **Example:** `resource = "mobile"`

Matches all JIDs with the specified resource name.
The following class includes `alice@localhost/mobile`, but not `alice@localhost/home`:

```toml
  mobile_users = [
    {resource = "mobile"}
  ]
```
This option can be combined with `user` and `server` - only `alice@localhost/mobile` belongs to the following class:

```toml
  admin = [
    {user = "alice", server = "localhost", resource = "mobile"}
  ]
```

### `acl.*.user_regexp`

* **Syntax:** string, [regular expression](http://erlang.org/doc/man/re.html#regexp_syntax)
* **Example:** `user_regexp = "^user.*"`

Matches all JIDs with the user name matching the regular expression.
The following class includes `alice@localhost` and `albert@jabber.org`, but not `bob@localhost`:

```toml
  ae = [
    {user_regexp = "^a.*e"}
  ]
```

This option can be combined with `server` - here `albert@jabber.org` is excluded:

```toml
  localhost_ae = [
    {user_regexp = "^a.*e", server = "localhost"}
  ]
```

### `acl.*.server_regexp`

* **Syntax:** string, [regular expression](http://erlang.org/doc/man/re.html#regexp_syntax)
* **Example:** `server = "localhost"`

Matches all JIDs with the domain name matching the regular expression.
The following class includes `alice@host1`, but not `alice@xmpp.org`:

```toml
  host_users = [
    {server_regexp = "host"}
  ]
```

This option can be combined with `user_regexp`, e.g. we can require the user name to contain 'a' and the domain name to start with 'a':

```toml
  a = [
    {user_regexp = "a", server_regexp = "^a"}
  ]
```

### `acl.*.resource_regexp`

* **Syntax:** string, [regular expression](http://erlang.org/doc/man/re.html#regexp_syntax)
* **Example:** `resource_regexp = "^res"`

Matches all JIDs with the resource name matching the regular expression. This class includes `bob@xmpp.org/res123`, but not `bob@xmpp.org/home`:

```toml
  digital_resources = [
    {resource_regexp = '^res\d+$'}
  ]
```

Note the use of a literal string (single quotes) to prevent `\d` from being escaped.

### `acl.*.user_glob`

* **Syntax:** string, [glob pattern](https://en.wikipedia.org/wiki/Glob_(programming))
* **Example:** `user_glob = "^user.*"`

Matches all JIDs with the user name matching the pattern:
The following class includes `alice@localhost` and `albert@jabber.org`, but not `bob@localhost`:

```toml
  ae_users = [
    {user_glob = "a*e*"}
  ]
```

This option can be combined with `server` - here `albert@jabber.org` is excluded:

```toml
  localhost_ae_users = [
    {user_glob = "a*e*", server = "localhost"}
  ]
```

### `acl.*.server_glob`

* **Syntax:** string, [glob pattern](https://en.wikipedia.org/wiki/Glob_(programming))
* **Example:** `server = "localhost"`

Matches all JIDs with the domain name matching the pattern.
The following class includes `alice@host1`, but not `alice@xmpp.org`:

```toml
  localhost_users = [
    {server_glob = "host*"}
  ]
```

This option can be combined with `user_glob`, e.g. we can require the user name to contain 'a' and the domain name to start with 'a':

```toml
  a = [
    {user_glob = "*a*", server_glob = "a*"}
  ]
```

### `acl.*.resource_glob`

* **Syntax:** string, [glob pattern](https://en.wikipedia.org/wiki/Glob_(programming))
* **Example:** `resource_glob = "^res"`

Matches all JIDs with the resource name matching the pattern. This class includes `bob@xmpp.org/res123`, but not `bob@xmpp.org/home`:

```toml
  limited_resources = [
    {resource_glob = "res???"}
  ]
```
