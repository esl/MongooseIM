The `s2s` section contains options configuring the server-to-server connections used to communicate with other federated XMPP servers.

# General options

These options affect both incoming and outgoing S2S connections.

## `s2s.default_policy`
* **Scope:** local
* **Syntax:** string, `"allow"` or `"deny"`
* **Default:** `"allow"`
* **Example:** `default_policy = "deny"`

Default policy for opening new S2S connections to/from remote servers.

## `s2s.host_policy`
* **Scope:** local
* **Syntax:** array of TOML tables with the following mandatory content:
    * `host` - string, host name
    * `policy` - string, `"allow"` or `"deny"`
* **Default:** `"allow"`
* **Example:**

```toml
  host_policy = [
    {host = "good.xmpp.org", policy = "allow"},
    {host = "bad.xmpp.org", policy = "deny"}
  ]
```

Policy for opening new connections to/from specific remote servers.

## `s2s.use_starttls`
* **Scope:** local
* **Syntax:** string, one of `"false"`, `"optional"`, `"required"`, `"required_trusted"`
* **Default:** `"false"`
* **Example:** `use_starttls = "required"`

Allows to configure StartTLS for incoming and outgoing S2S connections:

- `false` - StartTLS is disabled,
- `optional` - StartTLS is supported,
- `required` - StartTLS is supported and enforced,
- `required_trusted` - StartTLS is supported and enforced with certificate verification.

## `s2s.certfile`
* **Scope:** local
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `certfile = "cert.pem"`

Path to the X509 PEM file with a certificate and a private key inside (not protected by any password). Required if `use_starttls` is not `false`.

## `s2s.domain_certfile`
* **Scope:** local
* **Syntax:** array of TOML tables with the following mandatory content:
    * `domain` - string, XMPP domain name
    * `certfile` - string, path in the file system
* **Default:** not set
* **Example:**

```toml
  domain_certfile = [
    {domain = "localhost1.com", certfile = "cert1.pem"},
    {domain = "localhost2.com", certfile = "cert2.pem"}
  ]
```

This option overrides the configured certificate file for specific local XMPP domains.

**Notes:**

* This option applies to **S2S and C2S** connections.
* Each domain needs to be included in the list of [`hosts`](general.md#generalhosts) configured in the `general` section.

## `s2s.shared`
* **Scope:** local
* **Syntax:** string
* **Default:** 10 strong random bytes, hex-encoded
* **Example:** `shared = "82gc8b23ct7824"`

S2S shared secret used in the [Server Dialback](https://xmpp.org/extensions/xep-0220.html) extension.

# Outgoing connections

The options listed below affect only the outgoing S2S connections.

## `s2s.address`
* **Scope:** local
* **Syntax:** array of TOML tables with the following content:
    * `host` - string, mandatory, host name
    * `ip_address` - string, mandatory, IP address
    * `port` - integer, optional, port number
* **Default:** `"allow"`
* **Example:**

```toml
  address = [
    {host = "my.xmpp.org", ip_address = "192.0.100.1"},
    {host = "your.xmpp.org", ip_address = "192.0.1.100", port = 5271}
  ]
```

This option defines IP addresses and port numbers for specific non-local XMPP domains, allowing to override the DNS lookup for outgoing S2S connections.

## `s2s.ciphers`
* **Scope:** local
* **Syntax:** string
* **Default:** `"TLSv1.2:TLSv1.3"`
* **Example:** `ciphers = "TLSv1.2"`

Defines a list of accepted SSL ciphers for outgoing S2S connections.
Please refer to the [OpenSSL documentation](http://www.openssl.org/docs/apps/ciphers.html) for the cipher string format.

## `s2s.max_retry_delay`
* **Scope:** local
* **Syntax:** positive integer
* **Default:** `300`
* **Example:** `max_retry_delay = 300`

Specifies the maximum time in seconds that MongooseIM will wait until the next attempt to connect to a remote XMPP server. The delays between consecutive attempts will be doubled until this limit is reached.

## `s2s.outgoing.port`
* **Scope:** local
* **Syntax:** integer, port number
* **Default:** `5269`
* **Example:** `outgoing.port = 5270`

Defines the port to be used for outgoing S2S connections.

## `s2s.outgoing.ip_versions`
* **Scope:** local
* **Syntax:** array of integers (IP versions): `4` or `6`
* **Default:** `[4, 6]`
* **Example:** `outgoing.ip_versions = [6]`

Specifies the order of IP address families to try when establishing an outgoing S2S connection.

## `s2s.outgoing.connection_timeout`
* **Scope:** local
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `10_000`
* **Example:** `outgoing.connection_timeout = 5000`

Timeout (in seconds) for establishing an outgoing S2S connection.

## `s2s.dns.timeout`
* **Scope:** local
* **Syntax:** positive integer
* **Default:** `10`
* **Example:** `dns.timeout = 30`

Timeout (in seconds) for DNS lookups when opening an outgoing S2S connection.

## `s2s.dns.retries`
* **Scope:** local
* **Syntax:** positive integer
* **Default:** `2`
* **Example:** `dns.retries = 1`

Number of DNS lookup attempts when opening an outgoing S2S connection.
