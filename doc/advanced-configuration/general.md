The `general` section contains basic settings as well as some miscellaneous options.
You can start with providing only the basic options, configuring the loglevel, a single host (XMPP domain) and setting the default server language:

```
[general]
  loglevel = "warning"
  hosts = ["my-xmpp-domain.com"]
  language = "en"
```

All options are described below.

# General options

These are the basic settings that you should configure before running your MongooseIM server.

## `loglevel`
* **Scope:** local
* **Syntax:** string, one of `"none"`, `"emergency"`, `"alert"`, `"critical"`, `"error"`, `"warning"`, `"notice"`, `"info"`, `"debug"`, `"all"`.
* **Default:** `"warning"`
* **Example:** `loglevel = "error"`

Verbosity level of the logger. Values recommended for production systems are `"error"` and `"warning"`. The `"debug"` level is good for development.

## `hosts`
* **Scope:** global
* **Syntax:** array of strings representing the domain names.
* **Default:** there is no default, you have to provide at least one host name.
* **Example:** `hosts = ["localhost", "domain2"]`

Mandatory option, specifying the XMPP domains served by this cluster.

**Warning:** Extension modules and database backends will be started separately for every domain. When increasing the number of domains, please make sure you have enough resources available (e.g. connection limit set in the DBMS).

## `language`
* **Scope:** global
* **Syntax:** string representing the two-letter language code.
* **Default:** `"en"`
* **Example:** `language = "pl"`

Default language for messages sent by the server to users. You can get a full list of supported codes by executing `cd [MongooseIM root] ; ls priv/*.msg | awk '{split($0,a,"/"); split(a[4],b,"."); print b[1]}'` (`en` is not listed there)

# Database settings

RDBMS connection pools are set using [outgoing connections configuration](./outgoing-connections.md).
There are some additional options that influence all database connections in the server:

## `rdbms_server_type`
* **Scope:** local
* **Syntax:** string, `"mssql"` or `"pgsql"`
* **Default:** not set
* **Example:** `rdbms_server_type = "mssql"`

When using MSSQL or PostgreSQL databases, this option allows MongooseIM to optimize some queries for these DBs (e.g. `mod_mam_rdbms_user` uses different queries for `mssql`).

## `pgsql_users_number_estimate`
* **Scope:** local
* **Syntax:** boolean
* **Default:** false
* **Example:** `pgsql_users_number_estimate = true`

PostgreSQL's internal structure can make row counting slow.
Enabling this option uses an alternative query instead of `SELECT COUNT`, that might be not as accurate, but is always fast.

# Access management

User access rules and groups are configured mainly in the `acl`, `shaper` and `access` sections. Here you can find some additional options.

## `mongooseimctl_access_commands`
* **Scope:** local
* **Syntax:** TOML table, whose **keys** are access groups defined in the `acl` config section and **values** specify allowed administration commands. Each value is a table with the following nested options:
    * `commands`: mandatory, a list of strings representing the allowed commands, or the string `"all"`
    * `argument_restrictions`: optional, a table whose keys are the argument names and the values are strings representing the allowed values
* **Default:** not set

By default all admin operations are permitted with the `mongooseimctl` command without authentication. You can change that by setting this option for specific user access groups.

**Example 1.** Allow users from the `admin` access group to execute all commands without any restrictions:

```
  [general.mongooseimctl_access_commands.admin]
    commands = "all"
```

Alternative syntax: ```mongooseimctl_access_commands.admin.commands = "all"```

**Example 2.** Allow users from the `local` access group to execute the `join_cluster` command, but only if the `node` argument is equal to `mongooseim@prime`:

```
  [general.mongooseimctl_access_commands.local]
    commands = ["join_cluster"]
    argument_restrictions.node = "mongooseim@prime"
```

Here the more compact syntax is discouraged as the key names would get too long.

# Security

Here you can find some additional options related to system security.

## `registration_timeout`
* **Scope:** local
* **Syntax:** the string `"infinity"` or a number of seconds (positive integer)
* **Default:** `600`
* **Example:** `registration_timeout = "infinity"`

Limits the registration frequency from a single IP address. The special value `infinity` means no limit.

## `hide_service_name`
* **Scope:** local
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `hide_service_name = true`

According to RFC 6210, even when a client sends invalid data after opening a connection, the server must open an XML stream and return a stream error anyway. For extra security, this option may be enabled. It changes MIM behaviour to simply close the connection without any errors returned (effectively hiding the server's identity).

# User session management

These options can be used to configure the way MongooseIM manages user sessions.

## `sm_backend`
* **Scope:** global
* **Syntax:** string, `"mnesia"` or `"redis"`
* **Default:** `"mnesia"`
* **Example:** `sm_backend = "redis"`

Backend for storing user session data. All nodes in a cluster must have access to a complete session database.
Mnesia is sufficient in most cases, use Redis only in large deployments when you notice issues with the mnesia backend. Requires a redis pool with the `default` tag defined in the `outgoing_pools` section.
See the section about [redis connection setup](./outgoing-connections.md#redis-connection-setup) for more information.

## `replaced_wait_timeout`
* **Scope:** local
* **Syntax:** positive integer, representing time in milliseconds
* **Default:** `2000`
* **Example:** `replaced_wait_timeout = 5000`

When a user's session is replaced (due to a full JID conflict) by a new one, this parameter specifies the time MongooseIM waits for the old sessions to close. The default value is sufficient in most cases. If you observe `replaced_wait_timeout` warning in logs, then most probably the old sessions are frozen for some reason and it should be investigated.

# Message routing

The following options influence the way MongooseIM routes incoming messages to their recipients.

## `route_subdomains`
* **Scope:** local
* **Syntax:** string, the only accepted value is `"s2s"`
* **Default:** not set
* **Example:** `route_subdomains = "s2s"`

If a stanza is addressed to a subdomain of the served domain and this option is set to `s2s`, such a stanza will be transmitted over a server-to-server connection. Without it, MongooseIM will try to route the stanza to one of its internal services.

## `routing_modules`
* **Scope:** local
* **Syntax:** a list of strings representing the routing module names.
* **Default:** `["mongoose_router_global", "mongoose_router_localdomain", "mongoose_router_external_localnode", "mongoose_router_external", "ejabberd_s2s"]`
* **Example:** `routing_modules = ["mongoose_router_global", "mongoose_router_localdomain"]`

Provides an ordered list of modules used for routing messages. If one of the modules accepts packet for processing, the remaining ones are not called.

Allowed module names:

* `mongoose_router_global` - calls the `filter_packet` hook.
* `mongoose_router_localdomain` - routes packets addressed to a domain supported by the local cluster.
* `mongoose_router_external_localnode` - delivers packets to an XMPP component connected to the node, which processes the request.
* `mongoose_router_external` - delivers packets to an XMPP component connected to the local cluster.
* `ejabberd_s2s` - forwards packets to another XMPP cluster over XMPP Federation.

# Miscellaneous

The options listed below are used to configure more specific settings, that do not need to be changed in usual use cases.

## `all_metrics_are_global`
* **Scope:** local
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `all_metrics_are_global = true`

When enabled, all per-host metrics are merged into global equivalents. It means it is no longer possible to view individual host1, host2, host3, ... metrics, only sums are available. This option significantly reduces CPU and (especially) memory footprint in setups with exceptionally many domains (thousands, tens of thousands).

## `http_server_name`
* **Scope:** local
* **Syntax:** string
* **Default:** `"Cowboy"`
* **Example:** `http_server_name = "Apache"`

Replaces [Cowboy](https://github.com/ninenines/cowboy)'s default name returned in the `server` HTTP response header. It may be used for extra security, as it makes it harder for the malicious user to learn what HTTP software is running under a specific port. This option applies to **all** configured HTTP listeners.

## `override`
* **Scope:** local
* **Syntax:** array of strings: `"global"`, `"local"`, `"acls"`
* **Default:** not set
* **Example:** `override = ["global", "local"]`

Will cause MongooseIM to erase all global/local/acl configuration options in database respectively. This ensures that ALL settings of a specific type will be reloaded on startup.

## `max_fsm_queue`
* **Scope:** local
* **Syntax:** positive integer
* **Default:** not set
* **Example:** `max_fsm_queue = 5000`

When specified, will terminate certain processes (e.g. client handlers) that have more messages accumulated in the queue than the specified limit, to prevent resource exhaustion.
This option is set for C2S, outgoing S2S and component connections and can be overridden for particular `s2s` or `service` listeners in their configurations. **Use with caution!**
