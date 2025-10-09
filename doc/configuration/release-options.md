When building a MongooseIM release from source code, the initial configuration files are generated with options taken from the `vars-toml.config` file found in the `[MongooseIM root]/rel/` directory.
You can change the values in this file to affect the resulting `vm.args` and `mongooseim.toml` files.

The file contains erlang tuples terminated with period ('.'). For users not familiar with Erlang syntax, here is a quick cheat sheet:

* Each config option (key and value) is a tuple. Tuples are (Erlangers, forgive us the simplification) other Erlang terms separated with commas and enclosed in curly brackets ({}).
* Tuples (at least the top-level ones) in `vars.config` are always 2-element.
* The first element of each tuple is the name (Erlang atom).
* The second element is a quoted string. Any quotes (`"`) inside the string should be escaped with a backslash (`\`).

There are two types of options: parameters and blocks:

* a **parameter** is inserted into the value of an already defined option.
Parameters are mandatory - a valid value has to be provided.
* a **block** can be an empty string, one line or multiple lines, defining zero, one or more options.
Blocks are optional - the default is an empty string.

## vm.args options

These options are inserted into the `rel/files/vm.args` template.

### node_name

* **Type:** parameter
* **Option:** value of `-sname` in [vm.args](configuration-files.md#vmargs)
* **Syntax:** Erlang node name: `name@host`
* **Example:** `{node_name, "mongooseim@localhost"}.`

### highload_vm_args

* **Type:** block
* **Option:** arguments in [vm.args](configuration-files.md#vmargs): `+K`, `+A`, `+P`, `-env ERL_MAX_PORTS`
* **Syntax:** command-line arguments
* **Example:** `{highload_vm_args, "+P 10000000 -env ERL_MAX_PORTS 250000"}.`

### epmd_module

Allows to set EPMD module to `mongoose_epmd` in case CETS is used with RDBMS backend
to enable getting IP addresses of the remote nodes using RDBMS instead of the default
resolver.

* **Type:** parameter
* **Option:** value of `-epmd_module` in [vm.args](configuration-files.md#vmargs)
* **Syntax:** Erlang module name: `mongoose_epmd`
* **Example:** `{epmd_module, "mongoose_epmd"}.`

## TOML Options

These options are inserted into the `rel/files/mongooseim.toml` template.

### hosts

* **Type:** parameter
* **Option:** [`general.hosts`](general.md#generalhosts)
* **Syntax:** comma-separated list of strings
* **Example:** `{hosts, "\"localhost\", \"domain2\""}.`

### host_config

* **Type:** block
* **Option:** [`host_config`](host_config.md)
* **Syntax:** TOML block, one or more `[[host_config]]` sections.
* **Example:**

```
{host_config, "
[[host_config]]
  host = \"anonymous.localhost\"

  [host_config.auth]
    methods = [\"anonymous\"]
"}.
```

### auth_ldap

* **Type:** block
* **Option:** [`auth.ldap`](../authentication-methods/ldap.md)
* **Syntax:** TOML block, the `[auth.ldap]` subsection
* **Example:**

```
{auth_ldap, "
  [auth.ldap]
    base = \"ou=Users,dc=esl,dc=com\"
    filter = \"(objectClass=inetOrgPerson)\"
"}.
```

### s2s_addr

* **Type:** block
* **Option:** [`auth.s2s.address`](s2s.md#s2saddress)
* **Syntax:** TOML key-value pair with the `address` option
* **Example:**

```
{s2s_addr, "
  address = [
    {host = \"my.xmpp.org\", ip_address = \"192.0.100.1\"},
    {host = \"your.xmpp.org\", ip_address = \"192.0.1.100\", port = 5271}
  ]
"}.
```

### s2s_default_policy

* **Type:** parameter
* **Option:** [`s2s.default_policy`](s2s.md#s2sdefault_policy)
* **Syntax:** string
* **Example:** `{s2s_default_policy, "\"deny\""}.`

### outgoing_s2s_port

* **Type:** parameter
* **Option:** [`s2s.outgoing.port`](s2s.md#s2soutgoingport)
* **Syntax:** integer
* **Example:** `{outgoing_s2s_port, "5269"}.`

### c2s_port

* **Type:** parameter
* **Option:** [`listen.c2s.port`](listen.md#listenport)
* **Syntax:** integer
* **Example:** `{c2s_port, "5222"}.`

### s2s_port

* **Type:** parameter
* **Option:** [`listen.s2s.port`](listen.md#listenport)
* **Syntax:** integer
* **Example:** `{s2s_port, "5269"}.`

### cowboy_port

* **Type:** parameter
* **Option:** [`listen.http.port`](listen.md#listenport)
* **Syntax:** integer
* **Example:** `{http_port, "5280"}.`

### mod_last

* **Type:** block
* **Option:** [`modules.mod_last`](../modules/mod_last.md)
* **Syntax:** TOML section: `[modules.mod_last]`
* **Example:** `{mod_last, "[modules.mod_last]"}.`

### mod_offline

* **Type:** block
* **Option:** [`modules.mod_offline`](../modules/mod_offline.md)
* **Syntax:** TOML section: `[modules.mod_offline]`
* **Example:**

```
{mod_offline, "
[modules.mod_offline]
  access_max_user_messages = \"max_user_offline_messages\"
"}.
```

### mod_privacy

* **Type:** block
* **Option:** [`modules.mod_privacy`](../modules/mod_privacy.md)
* **Syntax:** TOML section: `[modules.mod_privacy]`
* **Example:** `{mod_privacy, "[modules.mod_privacy]"}.`

### mod_private

* **Type:** block
* **Option:** [`modules.mod_private`](../modules/mod_private.md)
* **Syntax:** TOML section: `[modules.mod_private]`
* **Example:** `{mod_private, "[modules.mod_private]"}.`

### mod_roster

* **Type:** block
* **Option:** [`modules.mod_roster`](../modules/mod_roster.md)
* **Syntax:** TOML section: `[modules.mod_roster]`
* **Example:** `{mod_roster, "[modules.mod_roster]"}.`

### mod_vcard

* **Type:** block
* **Option:** [`modules.mod_vcard`](../modules/mod_vcard.md)
* **Syntax:** TOML section: `[modules.mod_vcard]`
* **Example:**

```
{mod_vcard, "
[modules.mod_vcard]
  host = \"vjud.@HOST@\"
"}.
```

### sm_backend

* **Type:** parameter
* **Option:** [`general.sm_backend`](general.md#generalsm_backend)
* **Syntax:** string
* **Example:** `{sm_backend, "\"redis\""}.`

### s2s_backend

* **Type:** parameter
* **Option:** [`general.s2s_backend`](general.md#generals2s_backend)
* **Syntax:** string
* **Example:** `{s2s_backend, "\"mnesia\""}.`

### tls_config

* **Type:** block
* **Option:** [`listen.c2s.tls.*`](../listeners/listen-c2s.md#tls-options-for-c2s)
* **Syntax:** TOML key-value pairs
* **Example:**

```
{tls_config, "
  tls.certfile = \"priv/ssl/fake_cert.pem\"
  tls.keyfile = \"priv/ssl/fake_key.pem\"
  tls.mode = \"starttls\"
"}.
```

### auth_method

* **Type:** parameter
* **Option:** [`auth.methods`](auth.md#authmethods)
* **Syntax:** comma-separated list of strings
* **Example:** `{auth_method, "\"internal\""}.`
