# XMPP components: `[[listen.service]]`

Interface for external services acting as XMPP components ([XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html)), enabling communication between MongooseIM and external services over the XMPP network. The recommended port number for a component listener is 8888.

According to [XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html) the component's hostname should be given in the <stream:stream> element.

!!! warning
    This interface does not support [dynamic domains](../configuration/general.md#generalhost_types).
    Do not use them both at the same time.

## Configuration options

The following options are supported for each component listener under `listen.service` subsection:

### `listen.service.access`
* **Syntax:** string, rule name or `"all"`
* **Default:** `"all"`
* **Example:** `access = "component"`

Determines who is allowed to send data to external components. By default, the rule is `all`, which means that anyone can communicate with the components.

### `listen.service.password`
* **Syntax:** string
* **Default:** no default, this option is mandatory
* **Example:** `password = "secret"`

The external component needs to authenticate with this password to connect.

### `listen.service.shaper_rule`
* **Syntax:** string, name of the shaper
* **Default:** `"none"`
* **Example:** `shaper = "component_shaper"`

The traffic shaper used to limit the XMPP traffic to prevent the server from being flooded with incoming data.
Contrary to the C2S and S2S shapers, here the shaper name directly references the shaper that needs to be defined in the [`shaper`](../configuration/shaper.md) section.

### `listen.service.check_from`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `check_from = false`

Specifies whether the server should verify the "from" field in stanzas from the component.

### `listen.service.hidden_components`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `hidden_components = true`

All components connected to an endpoint with this option enabled will be considered "hidden".

Hidden components have a special flag enabled in the internal component table.
Alone, it doesn't change the server behaviour in any way, but it may be used by other modules and extensions to execute special logic.
An example would be [`mod_disco`](../modules/mod_disco.md), which may be configured to filter out hidden components from disco results, so they won't be discoverable by clients.
A reason to do so could be reduced traffic - systems with many components could return very long disco responses.
Also, some deployments would like to avoid revealing some services; not because it is a security threat (this method does not prevent clients from communicating with hidden components), but rather because they are not meant to interact with clients directly (e.g. helper components for other components).

### `listen.service.conflict_behaviour`
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

### `listen.service.max_fsm_queue`
* **Syntax:** positive integer
* **Default:** not set - no limit
* **Example:** `max_fsm_queue = 1000`

Message queue limit to prevent resource exhaustion; overrides the value set in the [`general`](../configuration/general.md#generalmax_fsm_queue) section.

## Custom extension to the protocol

In order to register a component for all virtual hosts served by the server (see [`hosts`](../configuration/general.md#generalhosts) in the [`general`](../configuration/general.md) section), the component must add the attribute `is_subdomain="true"` to the opening stream element.
This maybe helpful if someone wants to have a single instance of a component serving multiple virtual hosts.
The `is_subdomain` attribute is optional and the default behaviour is as described in [XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html).

## Service listener configuration example

The following section configures a service listener, accepting connections from external components.
The IP address is limited to loopback to prevent connections from different hosts.
All components are allowed to connect, but they need to provide the password.
The shaper named `fast` needs to be defined in the [`shaper`](../configuration/shaper.md) section.

```toml
[[listen.service]]
  port = 8888
  access = "all"
  shaper_rule = "fast"
  ip_address = "127.0.0.1"
  password = "secret"
```
