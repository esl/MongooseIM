The `general` section contains basic settings as well as some miscellaneous options.
You can start with providing only the basic options, for example configuring the loglevel, a single host (XMPP domain) as the default, and setting the server language:

```toml
[general]
  loglevel = "warning"
  hosts = ["my-xmpp-domain.com"]
  default_server_domain = "my-xmpp-domain.com"
  language = "en"
```

All options are described below.

## General options

These are the basic settings that you should configure before running your MongooseIM server.

### `general.loglevel`
* **Syntax:** string, one of `"none"`, `"emergency"`, `"alert"`, `"critical"`, `"error"`, `"warning"`, `"notice"`, `"info"`, `"debug"`, `"all"`.
* **Default:** `"warning"`
* **Example:** `loglevel = "error"`

Verbosity level of the logger. Values recommended for production systems are `"error"` and `"warning"`. The `"debug"` level is good for development.

### `general.hosts`
* **Syntax:** array of strings representing the domain names.
* **Default:** none. If omitted, at least one host type has to be defined in `general.host_types`.
* **Example:** `hosts = ["localhost", "domain2"]`

This option specifies the statically defined XMPP domains served by this cluster.
In order to configure these hosts independently, use the [`host_config` section](./host_config.md).

!!! Note
    At least one of `general.hosts` or `general.host_types` have to be provided.

!!! Warning
    Extension modules and database backends will be started separately for every domain from this list.
    When increasing the number of domains, please make sure you have enough resources available (e.g. connection limit set in the DBMS).

### `general.host_types`
* **Syntax:** array of strings the names for host types.
* **Default:** none. If omitted, at least one hast has to be defined in `general.hosts`.
* **Example:** `host_types = ["first type", "second type"]`

This is the list of names for the types of hosts that will serve dynamic XMPP domains.
Each host type can be seen as a label for a group of independent domains that use the same server configuration.
In order to configure these host types independently, use the [`host_config` section](./host_config.md). The domains can be added or removed dynamically with the [command line interface](../developers-guide/domain_management.md#command-line-interface) or using the [API](../developers-guide/domain_management.md#api).

If you use the host type mechanism, make sure you only configure modules which support dynamic domains in the [`modules`](Modules.md) or [`host_config.modules`](./host_config.md#host_configmodules) sections.
MongooseIM will **not** start otherwise.
Most of the modules are compatible with host types, but please read the particular extension module's page, or the [incompatible modules list](./Modules.md#modules-incompatible-with-dynamic-domains) to see which do not.
Moreover, server-to-server connections do not support dynamic domains. See the corresponding [listener](../listeners/listen-s2s.md) and [s2s](s2s.md) configuration for more information about s2s connections.

!!! Note
    At least one of `general.hosts` or `general.host_types` have to be provided.

!!! Warning
    Extension modules and database backends will be started separately for every host type from this list.
    When increasing the number of host types, please make sure you have enough resources available (e.g. connection limit set in the DBMS).

### `general.default_server_domain`
* **Syntax:** a string
* **Default:** none, this option is mandatory.
* **Example:** `default_server_domain = "my-xmpp-domain.com"`

This domain is used as a default when one cannot be determined, for example when sending XMPP stream errors to unauthenticated clients.

### `general.language`
* **Syntax:** string representing the two-letter language code.
* **Default:** `"en"`
* **Example:** `language = "pl"`

Default language for messages sent by the server to users. You can get a full list of supported codes by executing `cd [MongooseIM root] ; ls priv/*.msg | awk '{split($0,a,"/"); split(a[4],b,"."); print b[1]}'` (`en` is not listed there)

## Access management

User access rules are configured mainly in the [`acl`](acl.md) and [`access`](access.md) sections.

## Security

Here you can find some additional options related to system security.

### `general.registration_timeout`
* **Syntax:** the string `"infinity"` or a number of seconds (positive integer)
* **Default:** `600`
* **Example:** `registration_timeout = "infinity"`

Limits the registration frequency from a single IP address. The special value `infinity` means no limit.

### `general.hide_service_name`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `hide_service_name = true`

According to RFC 6210, even when a client sends invalid data after opening a connection, the server must open an XML stream and return a stream error anyway. For extra security, this option may be enabled. It changes MIM behaviour to simply close the connection without any errors returned (effectively hiding the server's identity).

## User session management

These options can be used to configure the way MongooseIM manages user sessions.

### `general.sm_backend`
* **Syntax:** string: `"mnesia"`, `"cets"` or `"redis"`
* **Default:** `"mnesia"`
* **Example:** `sm_backend = "redis"`

Backend for storing user session data. All nodes in a cluster must have access to a complete session database.
CETS is a new backend, requires RDBMS configured to work properly.
Mnesia is a legacy backend, sufficient in most cases, use Redis only in large deployments when you notice issues with the mnesia backend. Requires a redis pool with the `default` tag defined in the `outgoing_pools` section.
See the section about [redis connection setup](./outgoing-connections.md#redis-specific-options) for more information.

!!! Warning
    When set to `mnesia` or `cets`, the corresponding [internal database](internal-databases.md) has to be enabled.

### `general.replaced_wait_timeout`
* **Syntax:** positive integer, representing time in milliseconds
* **Default:** `2000`
* **Example:** `replaced_wait_timeout = 5000`

When a user's session is replaced (due to a full JID conflict) by a new one, this parameter specifies the time MongooseIM waits for the old sessions to close. The default value is sufficient in most cases. If you observe `replaced_wait_timeout` warning in logs, then most probably the old sessions are frozen for some reason and it should be investigated.

## XMPP federation (S2S)

### `general.s2s_backend`
* **Syntax:** string: `"mnesia"` or `"cets"`
* **Default:** `"mnesia"`
* **Example:** `s2s_backend = "cets"`

Backend for replicating the list of outgoing Server to Server (S2S) connections across the nodes of the local MongooseIM cluster.

!!! Warning
    The corresponding [internal database](internal-databases.md) has to be enabled.

## External XMPP components

### `general.component_backend`
* **Syntax:** string: `"mnesia"` or `"cets"`
* **Default:** `"mnesia"`
* **Example:** `component_backend = "cets"`

Backend for replicating the list of connected external components across the nodes of the local MongooseIM cluster.

!!! Warning
    The corresponding [internal database](internal-databases.md) has to be enabled.

## Message routing

The following options influence the way MongooseIM routes incoming messages to their recipients.

### `general.route_subdomains`
* **Syntax:** string, the only accepted value is `"s2s"`
* **Default:** not set
* **Example:** `route_subdomains = "s2s"`

If a stanza is addressed to a subdomain of the served domain and this option is set to `s2s`, such a stanza will be transmitted over a server-to-server connection. Without it, MongooseIM will try to route the stanza to one of its internal services.

### `general.routing_modules`
* **Syntax:** a list of strings representing the routing module names.
* **Default:** `["mongoose_router_global", "mongoose_router_localdomain", "mongoose_router_external_localnode", "mongoose_router_external", "mongoose_router_dynamic_domains", "ejabberd_s2s"]`
* **Example:** `routing_modules = ["mongoose_router_global", "mongoose_router_localdomain"]`

Provides an ordered list of modules used for routing messages. All available modules are enabled by default, and you can change their order or disable some of them by providing your own list. See the [Message routing](../developers-guide/Stanza-routing.md#3-message-routing) section of the developer's guide for more information.

## Miscellaneous

The options listed below are used to configure more specific settings, that do not need to be changed in usual use cases.

### `general.http_server_name`
* **Syntax:** string
* **Default:** `"Cowboy"`
* **Example:** `http_server_name = "Apache"`

Replaces [Cowboy](https://github.com/ninenines/cowboy)'s default name returned in the `server` HTTP response header. It may be used for extra security, as it makes it harder for the malicious user to learn what HTTP software is running under a specific port. This option applies to **all** configured HTTP listeners.
