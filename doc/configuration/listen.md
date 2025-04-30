The `listen` section specifies how MongooseIM handles incoming connections.

* **Syntax:** Each listener is specified in a subsection starting with `[[listen.type]]` where `type` is one of the allowed listener types, handling different types of incoming connections:

    * [`c2s`](../listeners/listen-c2s.md) - client-to-server XMPP connections,
    * [`s2s`](../listeners/listen-s2s.md) - server-to-server XMPP connections,
    * [`component`](../listeners/listen-components.md) - XMPP connections from external components,
    * [`http`](../listeners/listen-http.md) - HTTP connections from clients or other services.

The double-bracket syntax is used because there can be multiple listeners of a given type, so for each listener type there is a TOML array of one or more tables (subsections).

* **Default:** None - each listener needs to be enabled explicitly. Typical listeners are already specified in the example configuration file.
* **Example:** The simplest XMPP listener configuration, handling only incoming XMPP client connections:

```toml
[[listen.c2s]]
  port = 5222
```

## General listener options

The options listed below are the same for all listener types. They set the basic listening socket options. Only `port` is required, the rest can be used to change the default settings.

### `listen.*.port`
* **Syntax:** integer, port number
* **Default:** no default, this option is mandatory.
* **Example:** `port = 5222`

The port number to which the listening socket is bound.

### `listen.*.ip_address`
* **Syntax:** string with the IP address
* **Default:** all-zeros address (e.g. `"0.0.0.0"` for IPv4)
* **Example:** `ip_address = "127.0.0.1"`

The IP address to which the listening socket is bound.

### `listen.*.proto`
* **Syntax:** string, only `"tcp"` is accepted
* **Default:** `"tcp"`
* **Example:** `proto = "tcp"`

The protocol, which is TCP by default. Currently this is the only valid option.

### `listen.*.ip_version`
* **Syntax:** integer, `4` or `6`
* **Default:** if `ip_address` is specified, the IP version is determined from that address, otherwise it is `4`
* **Example:** `ip_version = 6`

Allows to set the IP version to IPv6. Does not need to be set if `ip_address` is defined.

### `listen.*.hibernate_after`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `0`
* **Example:** `hibernate_after = 10`

Time in milliseconds after which a client process spawned by this listener will hibernate.
Hibernation greatly reduces memory consumption of client processes, but *may* result in increased CPU consumption if a client is used *very* frequently.
The default, recommended value of 0 means that the client processes will hibernate at every opportunity.

## XMPP listener options

The options listed below can be set for the `c2s`, `s2s` and `component` listeners to adjust their parameters.

### `listen.*.backlog`
* **Syntax:** positive integer
* **Default:** `1024`
* **Example:** `backlog = 1000`

Overrides the default TCP backlog value.

### `listen.*.proxy_protocol`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `proxy_protocol = true`

When set to `true`, [Proxy Protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol/) is enabled and each connecting client has to provide a proxy header. Use only with a proxy (or a load balancer) to allow it to provide the connection details (including the source IP address) of the original client. Versions 1 and 2 of the protocol are supported.

### `listen.*.max_stanza_size`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `max_stanza_size = 10_000`

Maximum allowed incoming stanza size in bytes.
!!! Warning
    This limit is checked **after** the input data parsing, so it does not apply to the input data size itself.

### `listen.*.num_acceptors`
* **Syntax:** positive integer
* **Default:** `100`
* **Example:** `num_acceptors = 200`

The number of processes accepting new connections on the listening socket.

### `listen.*.shaper`
* **Syntax:** string, shaper name
* **Default:** `"none"` (no shaper)
* **Example:** `shaper = "normal"`

The shaper name that determines what traffic shaper is used to limit the incoming XMPP traffic to prevent the server from being flooded with incoming data.
The shaper referenced here needs to be defined in the [`shaper`](../configuration/shaper.md) configuration section.
The value of the shaper name needs to be either the shaper name or the string `"none"`, which means no shaper.

### `listen.*.max_connections`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `max_connections = 10000`

Maximum number of open connections. This is a *soft limit* according to the [Ranch](https://ninenines.eu/docs/en/ranch/2.1/manual/ranch) documentation.

### `listen.*.reuse_port`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `reuse_port = true`

Enables linux support for `SO_REUSEPORT`, see [Stack Overflow](https://stackoverflow.com/questions/14388706/how-do-so-reuseaddr-and-so-reuseport-differ) for more details.

### `listen.*.state_timeout`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `5000`
* **Example:** `state_timeout = 10_000`

Timeout value (in milliseconds) used by the state machine when waiting for the connecting party to respond during stream negotiation and SASL authentication. After the timeout, the server responds with the `connection-timeout` stream error and closes the connection.
