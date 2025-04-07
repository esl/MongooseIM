The `s2s` section contains options configuring the server-to-server connections used to communicate with other federated XMPP servers.

!!! warning
    Server-to-server connections do not support [dynamic domains](../configuration/general.md#generalhost_types).
    Do not use dynamic domains when using `s2s`.

## General options

These options affect both incoming and outgoing S2S connections.

### `s2s.default_policy`
* **Syntax:** string, `"allow"` or `"deny"`
* **Default:** `"allow"`
* **Example:** `default_policy = "deny"`

Default policy for opening new S2S connections to/from remote servers.

### `s2s.host_policy`
* **Syntax:** array of TOML tables with the following mandatory content:
    * `host` - string, host name
    * `policy` - string, `"allow"` or `"deny"`
* **Default:** not set, `default_policy` is used
* **Example:**

```toml
  host_policy = [
    {host = "good.xmpp.org", policy = "allow"},
    {host = "bad.xmpp.org", policy = "deny"}
  ]
```

Policy for opening new connections to/from specific remote servers.

### `s2s.shared`
* **Syntax:** string
* **Default:** 10 strong random bytes, hex-encoded
* **Example:** `shared = "82gc8b23ct7824"`

S2S shared secret used in the [Server Dialback](https://xmpp.org/extensions/xep-0220.html) extension.

## Outgoing connections

The options listed below affect only the outgoing S2S connections.

### `s2s.outgoing.address`
* **Syntax:** array of TOML tables with the following content:
    * `host` - string, mandatory, host name
    * `ip_address` - string, mandatory, IP address
    * `port` - integer, optional, port number
* **Default:** not set
* **Example:**

```toml
  outgoing.address = [
    {host = "my.xmpp.org", ip_address = "192.0.100.1"},
    {host = "your.xmpp.org", ip_address = "192.0.1.100", port = 5271}
  ]
```

This option defines IP addresses and port numbers for specific non-local XMPP domains, allowing to override the DNS lookup for outgoing S2S connections.

### `s2s.outgoing.connection_timeout`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `10_000`
* **Example:** `outgoing.connection_timeout = 5000`

Timeout (in milliseconds) for establishing an outgoing S2S connection.

### `s2s.outgoing.dns.retries`
* **Syntax:** positive integer
* **Default:** `2`
* **Example:** `outgoing.dns.retries = 1`

Number of DNS lookup attempts when opening an outgoing S2S connection.

### `s2s.outgoing.dns.timeout`
* **Syntax:** positive integer
* **Default:** `10`
* **Example:** `outgoing.dns.timeout = 30`

Timeout (in seconds) for DNS lookups when opening an outgoing S2S connection.

### `s2s.outgoing.ip_versions`
* **Syntax:** array of integers (IP versions): `4` or `6`
* **Default:** `[4, 6]`
* **Example:** `outgoing.ip_versions = [6]`

Specifies the order of IP address families to try when establishing an outgoing S2S connection.

### `s2s.outgoing.max_retry_delay`
* **Syntax:** positive integer
* **Default:** `300`
* **Example:** `outgoing.max_retry_delay = 300`

Specifies the maximum time in seconds that MongooseIM will wait until the next attempt to connect to a remote XMPP server. The delays between consecutive attempts will be doubled until this limit is reached.

### `s2s.outgoing.max_stanza_size`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `outgoing.max_stanza_size = 10_000`

Maximum allowed incoming stanza size in bytes.
!!! Warning
    This limit is checked **after** the input data parsing, so it does not apply to the input data size itself.

### `s2s.outgoing.port`
* **Syntax:** integer, port number
* **Default:** `5269`
* **Example:** `outgoing.port = 5270`

Defines the port to be used for outgoing S2S connections.

### `s2s.outgoing.shaper`
* **Syntax:** string, shaper name
* **Default:** `"none"` (no shaper)
* **Example:** `outgoing.shaper = "fast"`

The shaper name that determines what traffic shaper is used to limit the incoming XMPP traffic to prevent the server from being flooded with incoming data.
The shaper referenced here needs to be defined in the [`shaper`](shaper.md) configuration section.
The value of the shaper name needs to be either the shaper name or the string `"none"`, which means no shaper.

### `s2s.outgoing.state_timeout`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `5000` (5 seconds)
* **Example:** `outgoing.state_timeout = 10_000`

Timeout value (in milliseconds) used by the state machine when waiting for the remote server to respond during stream negotiation and SASL authentication. After the timeout, the local server responds with the `connection-timeout` stream error and closes the connection.

### `s2s.outgoing.stream_timeout`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `600_000` (10 minutes)
* **Example:** `outgoing.stream_timeout = 60_000`

Timeout value (in milliseconds) used by the state machine for an established connection.
When it passes without any sent or received data, the outgoing connection is closed due to inactivity.

### TLS options for outgoing connections

In order to enable TLS encryption, you need to ensure that the `s2s.outgoing.tls` subsection is present.
It contains options with the same semantics as the corresponding options for [outgoing connection pools](outgoing-connections.md#tls-options).
Additionally, the following options are supported:

### `s2s.outgoing.tls.mode`
* **Syntax:** string, one of `"tls"`, `"starttls"`, `"starttls_required"`
* **Default:** `"starttls"`
* **Example:** `outgoing.tls.mode = "starttls"`

This option determines how the TLS encryption is set up.

* `tls` - the local server initiates a TLS session immediately after connecting, before beginning the normal XML stream.
* `starttls` - enables StartTLS, which upgrades the connection to TLS if supported by the remote server.
* `starttls_required` - enables and enforces StartTLS usage. The connection is closed if StartTLS cannot be enabled.
