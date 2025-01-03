# XMPP components: `[[listen.component]]`

Interface for external services acting as XMPP components ([XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html)), enabling communication between MongooseIM and external services over the XMPP network. The recommended port number for a component listener is 8888.

According to [XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html) the component's hostname should be given in the <stream:stream> element.

!!! Note
    The component might register _any_ domain, which might not necessarily be a static nor a dynamic domain, nor subdomain, recognised by the MongooseIM router. For routing to work, the modules `mongoose_router_external_localnode` and `mongoose_router_external` must be enabled in the [`general.routing_modules`](../configuration/general.md#generalrouting_modules) section.

    Note the order of the routing modules: if a component is meant to supplant a domain served regularly by the MongooseIM server, the external routers should be ordered with higher priority.


## Configuration options

The following options are supported for each component listener under `listen.component` subsection:

### `listen.component.max_connections`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `max_connections = 10000`

Maximum number of open connections. This is a *soft limit* according to the [Ranch](https://ninenines.eu/docs/en/ranch/2.1/manual/ranch) documentation.

### `listen.component.reuse_port`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `reuse_port = true`

Enables linux support for `SO_REUSEPORT`, see [Stack Overflow](https://stackoverflow.com/questions/14388706/how-do-so-reuseaddr-and-so-reuseport-differ) for more details.

### `listen.component.state_timeout`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `5000`
* **Example:** `state_timeout = 10_000`

Timeout value (in milliseconds) used by the component state machine when waiting for the connecting client to respond during stream negotiation. After the timeout the server responds with the `connection-timeout` stream error and closes the connection.

### `listen.component.access`
* **Syntax:** string, rule name or `"all"`
* **Default:** `"all"`
* **Example:** `access = "component"`

Determines who is allowed to send data to external components. By default, the rule is `all`, which means that anyone can communicate with the components.

### `listen.component.password`
* **Syntax:** string
* **Default:** no default, this option is mandatory
* **Example:** `password = "secret"`

The external component needs to authenticate with this password to connect.

### `listen.component.shaper`
* **Syntax:** string, name of the shaper
* **Default:** `"none"`
* **Example:** `shaper = "component_shaper"`

The traffic shaper used to limit the XMPP traffic to prevent the server from being flooded with incoming data.

### `listen.component.check_from`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `check_from = false`

Specifies whether the server should verify the "from" field in stanzas from the component.

### `listen.component.hidden_components`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `hidden_components = true`

All components connected to an endpoint with this option enabled will be considered "hidden".

Hidden components have a special flag enabled in the internal component table.
Alone, it doesn't change the server behaviour in any way, but it may be used by other modules and extensions to execute special logic.
An example would be [`mod_disco`](../modules/mod_disco.md), which may be configured to filter out hidden components from disco results, so they won't be discoverable by clients.
A reason to do so could be reduced traffic - systems with many components could return very long disco responses.
Also, some deployments would like to avoid revealing some services; not because it is a security threat (this method does not prevent clients from communicating with hidden components), but rather because they are not meant to interact with clients directly (e.g. helper components for other components).

### `listen.component.conflict_behaviour`
* **Syntax:** string, one of: `"disconnect"`, `"kick_old"`
* **Default:** `"disconnect"`
* **Example:** `conflict_behaviour = "kick_old"`

By default, when a component tries to connect and a registration conflict occurs, the connection is dropped with the following error:

```xml
<stream:error>
  <conflict xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>
</stream:error>
</stream:stream>
```

It makes implementing the reconnection logic difficult, because the old connection would not allow any other connections.
By setting this option to `kick_old`, we drop any old connections registered at the same host before accepting new ones.

## Custom extension to the protocol

In order to register a component for all virtual hosts served by the server (see [`hosts`](../configuration/general.md#generalhosts) in the [`general`](../configuration/general.md) section), the component must add the attribute `is_subdomain="true"` to the opening stream element.
This maybe helpful if someone wants to have a single instance of a component serving multiple virtual hosts.
The `is_subdomain` attribute is optional and the default behaviour is as described in [XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html).

## TLS options for components

You can specify additional options of the TLS encryption in the `tls` subsection of the listener configuration. They have the same semantics as the corresponding [c2s options](listen-c2s.md#tls-options-for-c2s) for `just_tls`.

## Service listener configuration example

The following section configures a component listener, accepting connections from external components.
The IP address is limited to loopback to prevent connections from different hosts.
All components are allowed to connect, but they need to provide the password.
The shaper named `fast` needs to be defined in the [`shaper`](../configuration/shaper.md) section.

```toml
[[listen.component]]
  port = 8888
  access = "all"
  shaper = "fast"
  ip_address = "127.0.0.1"
  password = "secret"
```
