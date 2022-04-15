The `listen` section specifies how MongooseIM handles incoming connections.

* **Scope:** local
* **Syntax:** Each listener is specified in a subsection starting with `[[listen.type]]` where `type` is one of the allowed listener types, handling different types of incoming connections:

    * `c2s` - client-to-server XMPP connections,
    * `s2s` - server-to-server XMPP connections,
    * `service` - XMPP connections from external components,
    * `http` - HTTP connections from clients or other services.

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
* **Example:** `proto = "udp"`

The protocol, which is TCP by default. Currently this is the only valid option.

### `listen.*.ip_version`
* **Syntax:** integer, `4` or `6`
* **Default:** if `ip_address` is specified, the IP version is determined from that address, otherwise it is `4`
* **Example:** `ip_version = 6`

Allows to set the IP version to IPv6. Does not need to be set if `ip_address` is defined.

## XMPP listener options

The options listed below can be set for the `c2s`, `s2s` and `service` listeners to adjust their parameters.

### `listen.*.backlog`
* **Syntax:** positive integer
* **Default:** `100`
* **Example:** `backlog = 1000`

Overrides the default TCP backlog value.

### `listen.*.proxy_protocol`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `proxy_protocol = true`

When set to `true`, [Proxy Protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol/) is enabled and each connecting client has to provide a proxy header. Use only with a proxy (or a load balancer) to allow it to provide the connection details (including the source IP address) of the original client. Versions 1 and 2 of the protocol are supported.

### `listen.*.hibernate_after`
* **Syntax:** non-negative integer
* **Default:** `0`
* **Example:** `hibernate_after = 10`

Time in milliseconds after which a client process spawned by this listener will hibernate.
Hibernation greatly reduces memory consumption of client processes, but *may* result in increased CPU consumption if a client is used *very* frequently.
The default, recommended value of 0 means that the client processes will hibernate at every opportunity.

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

### `listen.*.max_fsm_queue`
* **Syntax:** positive integer
* **Default:** not set - no limit
* **Example:** `max_fsm_queue = 1000`

Message queue limit to prevent resource exhaustion; overrides the value set in the `general` section.
This option does **not** work for `s2s` listeners - the `general` value is used for them.

## Client-to-server (C2S): `[[listen.c2s]]`

Handles XMPP client-to-server (C2S) connections.
The recommended port number for a C2S listener is 5222 [as registered in the XMPP protocol](https://tools.ietf.org/html/rfc6120#section-14.7).
The following options are supported for each C2S listener:

### `listen.c2s.access`
* **Syntax:** string, rule name or `"all"`
* **Default:** `"all"`
* **Example:** `access = "c2s"`

The rule that determines who is allowed to connect. By default the rule is `"all"`, which means that anyone can connect. The rule referenced here needs to be defined in the `access` configuration section.

### `listen.c2s.shaper`
* **Syntax:** string, rule name
* **Default:** `"none"` (no shaper)
* **Example:** `shaper = "c2s_shaper"`

The rule that determines what traffic shaper is used to limit the incoming XMPP traffic to prevent the server from being flooded with incoming data.
The rule referenced here needs to be defined in the `access` configuration section.
The value of the access rule needs to be either the shaper name or the string `"none"`, which means no shaper.

### `listen.c2s.zlib`
* **Syntax:** positive integer
* **Default:** not set, disabled
* **Example:** `zlib = 1024`

Enables ZLIB support, the integer value is a limit for a decompressed output size in bytes (to prevent a successful [ZLIB bomb attack](https://xmpp.org/community/security-notices/uncontrolled-resource-consumption-with-highly-compressed-xmpp-stanzas.html)).

### `listen.c2s.allowed_auth_methods`

* **Syntax:** array of strings. Allowed values: `"internal"`, `"rdbms"`, `"external"`, `"anonymous"`, `"ldap"`, `"jwt"`, `"riak"`, `"http"`, `"pki"`, `"dummy"`
* **Default:** not set
* **Example:** `allowed_auth_methods = ["internal"]`

A subset of enabled methods to login with for this listener.
This option allows to enable only some backends.
It is useful, if you want to have several listeners for different type of users (for example, some users use PKI while other users use LDAP auth).
Same syntax as for `auth.methods` option.

## TLS options for C2S

The following options allow enabling and configuring TLS which makes the client-to-server connections secure.
They all have the `tls.` prefix.

### `listen.c2s.tls.mode`
* **Syntax:** string, one of `"tls"`, `"starttls"`, `"starttls_required"`
* **Default:** not set
* **Example:** `tls.mode = "starttls"`

By default there is no encryption for the incoming connections. You can change this by setting the `tls.mode` option to one of the following modes:

* `tls` - clients must initiate a TLS session immediately after connecting, before beginning the normal XML stream,
* `starttls` - enables StartTLS support; requires `certfile`,
* `starttls_required` - enables and enforces StartTLS usage.

### `listen.c2s.tls.verify_peer`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `verify_peer = true`

Enforces verification of a client certificate. Requires a valid `cacertfile`.

### `listen.c2s.tls.module`
* **Syntax:** string, one of `"just_tls"`, `"fast_tls"`
* **Default:** `"fast_tls"`
* **Example:** `tls.module = "just_tls"`

By default the TLS library used for C2S connections is `fast_tls`, which uses OpenSSL-based NIFs. It is possible to change it to `just_tls` - Erlang TLS implementation provided by OTP. Some TLS-related options described here have different formats for these two libraries.

Requires setting `tls.verify_mode`.  When set to `false`, it allows the client to connect even though the certificate verification failed. It is then up to the authentication layer to accept or reject the client connection. This behaviour mimics the FastTLS one.

### `listen.c2s.tls.certfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.certfile = "server.pem"`

Path to the X509 PEM file with a certificate and a private key (not protected by a password). If the certificate is signed by an intermediate CA, you should specify here the whole CA chain by concatenating all public keys together and appending the private key after that.

### `listen.c2s.tls.cacertfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.cacertfile = "ca.pem"`

Path to the X509 PEM file with a CA chain that will be used to verify clients. It won't have any effect if `verify_peer` is not enabled.

### `listen.c2s.tls.dhfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.dhfile = "dh.pem"`

Path to the Diffie-Hellman parameter file.

### `listen.c2s.tls.ciphers`
* **Syntax:** string with the OpenSSL cipher suite specification
* **Default:** for `fast_tls` the default is`"TLSv1.2:TLSv1.3"`. For `just_tls` this option is not set by default - all supported suites are accepted.
* **Example:** `tls.ciphers = "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384"`

Cipher suites to use with StartTLS or TLS. Please refer to the [OpenSSL documentation](http://www.openssl.org/docs/man1.0.2/apps/ciphers.html) for the cipher string format. For `fast_tls`, this string can be used to specify versions as well. For `just_tls`, see the [Erlang/OTP SSL documentation](https://erlang.org/doc/man/ssl.html#type-ciphers) for allowed values.

### `listen.c2s.tls.protocol_options` - only for `fast_tls`
* **Syntax:** array of strings
* **Default:** `["no_sslv2", "no_sslv3", "no_tlsv1", "no_tlsv1_1"]`
* **Example:** `tls.protocol_options = ["no_tlsv1", "no_tlsv1_1"]`

A list of OpenSSL options for FastTLS. You can find the mappings between supported options and actual OpenSSL flags in the `fast_tls` [source code](https://github.com/processone/fast_tls/blob/master/c_src/options.h).

### `listen.c2s.tls.verify_mode` - only for `just_tls`
* **Syntax:** string, one of `"peer"`, `"selfsigned_peer"`, `"none"`
* **Default:** not set (equivalent to `"peer"` in the current version of Erlang/OTP)
* **Example:** `tls.verify_mode = "selfsigned_peer"`

Specifies the way certificate verification works:

* `peer` - makes sure the peer's certificate is valid and signed by a trusted CA,
* `selfsigned_peer` - makes sure the peer's certificate is valid, but allows self-signed certificates,
* `none` - any certificate is accepted.

### `listen.c2s.tls.disconnect_on_failure` - only for `just_tls`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `tls.disconnect_on_failure = false`

### `listen.c2s.tls.versions` - only for `just_tls`
* **Syntax:** array of strings
* **Default:** not set, all supported versions are accepted
* **Example:** `tls.versions = ["tlsv1.2", "tlsv1.3"]`

TLS versions to use with StartTLS or TLS. For allowed values, see the [Erlang/OTP SSL documentation](https://erlang.org/doc/man/ssl.html#type-protocol_version)

### `listen.c2s.tls.crl_files` - only for `just_tls`
* **Syntax:** array of strings, paths in the file system
* **Default:** not set
* **Example:** `tls.crl_files = ["certs.crl"]`

Specifies the paths to Certificate Revocation Lists.

### C2S Example

The following section configures two C2S listeners.

```toml
[[listen.c2s]]
  port = 5222
  zlib = 10000
  access = "c2s"
  shaper = "c2s_shaper"
  max_stanza_size = 65536
  tls.mode = "starttls"
  tls.certfile = "server.pem"
  tls.dhfile = "dh_server.pem"

[[listen.c2s]]
  port = 5223
  zlib = 4096
  access = "c2s"
  shaper = "c2s_shaper"
  max_stanza_size = 65536
```

* One at port 5222, which accepts a plain TCP connection and allows to use StartTLS for upgrading it to an encrypted one. The files containing the certificate and the DH parameter are also provided.
* One at port 5223, which accepts only encrypted TLS connections - this is the legacy method as StartTLS is preferred.

Both listeners use ZLIB and the `c2s` and `c2s_shaper` rules for access management and traffic shaping, respectively.

## Server-to-server (S2S): `[[listen.s2s]]`

Handles incoming server-to-server (S2S) connections (federation).
The recommended port number for an S2S listener is 5269 [as registered in the XMPP protocol](https://tools.ietf.org/html/rfc6120#section-14.7).

!!! Note
    Many S2S options are configured in the `s2s` section of the configuration file, and they apply to both incoming and outgoing connections.

### `listen.s2s.shaper`
* **Syntax:** string, name of the shaper rule or `"none"`
* **Default:** `"none"` - no shaper
* **Example:** `shaper = "s2s_shaper"`

Name of the rule that determines what traffic shaper is used to limit the incoming XMPP traffic to prevent the server from being flooded with incoming data. The rule referenced here needs to be defined in the `access` config section and it should return the shaper name or the value `"none"`.

### TLS options for S2S

S2S connections do not use TLS encryption unless enabled with the `use_starttls` option in the `s2s` section.
Here you can specify some additional options of the TLS encryption.

#### `listen.s2s.tls.cacertfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.cacertfile = "ca.pem"`

Path to the X509 PEM file with a CA chain that will be used to verify the connecting XMPP servers (acting as clients here). It won't have any effect if `verify_peer` is not enabled.

#### `listen.s2s.tls.dhfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.dhfile = "dh.pem"`

Path to the Diffie-Hellman parameter file.

#### `listen.s2s.tls.ciphers`
* **Syntax:** string with the OpenSSL cipher suite specification
* **Default:** `"TLSv1.2:TLSv1.3"`
* **Example:** `tls.ciphers = "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384"`

Cipher suites to use with StartTLS. Please refer to the [OpenSSL documentation](http://www.openssl.org/docs/man1.0.2/apps/ciphers.html) for the cipher string format.

### S2S Example

The following section configures an S2S listener with some basic settings set up.
The `s2s_shaper` access rule is used, which requires a definition in the `access` section.

```toml
[[listen.s2s]]
  port = 5269
  shaper = "s2s_shaper"
  max_stanza_size = 131072
  tls.dhfile = "dh_server.pem"
```

##  XMPP Components: `[[listen.service]]`

Interface for external services acting as XMPP components ([XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html)), enabling communication between MongooseIM and external services over the XMPP network. The recommended port number for a component listener is 8888.

According to [XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html) the component's hostname should be given in the <stream:stream> element.

!!! warning
    This interface does not support [dynamic domains](../configuration/general.md#generalhost_types).
    Do not use them both at the same time.

### `listen.service.access`
* **Syntax:** string, rule name or `"all"`
* **Default:** `"all"`
* **Example:** `access = "component"`

Determines who is allowed to send data to external components. By default the rule is `all`, which means that anyone can communicate with the components.

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
Contrary to the C2S and S2S shapers, here the shaper name directly references the shaper that needs to be defined in the [`shaper`](shaper.md) section.

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

### Custom extension to the protocol

In order to register a component for all virtual hosts served by the server (see `hosts` in the `general` section), the component must add the attribute `is_subdomain="true"` to the opening stream element.
This maybe helpful if someone wants to have a single instance of a component serving multiple virtual hosts.
The `is_subdomain` attribute is optional and the default behaviour is as described in [XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html).

### Service listener example

The following section configures a service listener, accepting connections from external components.
The IP address is limited to loopback to prevent connections from different hosts.
All components are allowed to connect, but they need to provide the password.
The shaper named `fast` needs to be defined in the `shaper` section.

```toml
[[listen.service]]
  port = 8888
  access = "all"
  shaper_rule = "fast"
  ip_address = "127.0.0.1"
  password = "secret"
```

## HTTP-based services: `[[listen.http]]`

Manages all HTTP-based services, such as BOSH (HTTP long-polling), WebSocket and REST.
It uses the [Cowboy](https://ninenines.eu/docs/en/cowboy/2.6/manual) web server.
Recommended port number: 5280 for BOSH/WS.

There are the following options for each of the HTTP listeners:

### `listen.http.handlers`
* **Syntax:** each handler is specified in a subsection starting with `[[listen.http.handlers.type]]` where `type` is one of the allowed handler types, handling different connection types, e.g.

    * `mod_bosh` - for [BOSH](https://xmpp.org/extensions/xep-0124.html) connections,
    * `mod_websockets` - for [WebSocket](https://tools.ietf.org/html/rfc6455) connections,
    * `mongoose_api_*`, `mongoose_client_api_*`, ... - for REST API.

    These types are described below in more detail.
    The double-bracket syntax is used because there can be multiple handlers of a given type, so for each type there is a TOML array of one or more tables (subsections).

* **Default:** there is no default, all handlers need to be specified explicitly.
* **Example:** two handlers, one for BOSH and one for WebSockets
```toml
  [[listen.http.handlers.mod_bosh]]
    host = "_"
    path = "/http-bind"

  [[listen.http.handlers.mod_websockets]]
    host = "_"
    path = "/ws-xmpp"
```

### Common handler options

#### `listen.http.handlers.*.host`
* **Syntax:** string
* **Default:** no default, mandatory option
* **Example:** `host = "localhost"`

Host name for this handler or `"_"` for any host.

#### `listen.http.handlers.*.path`
* **Syntax:** string
* **Default:** no default, mandatory option
* **Example:** `path = "/ws-xmpp"`

Path for this handler.

### Handler types: BOSH - `mod_bosh`

To handle incoming BOSH traffic you need to configure the `mod_bosh` module in the `modules` section as well.

### Handler types: WebSockets - `mod_websockets`

Websocket connections as defined in [RFC 7395](https://tools.ietf.org/html/rfc7395).
You can pass the following optional parameters:

#### `listen.http.handlers.mod_websockets.timeout`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `timeout = 60_000`

The time (in milliseconds) after which an inactive user is disconnected.

#### `listen.http.handlers.mod_websockets.ping_rate`
* **Syntax:** positive integer
* **Default:** not set - pings disabled
* **Example:** `ping_rate = 10_000`

The time between pings sent by server. By setting this option you enable server-side pinging.

#### `listen.http.handlers.mod_websockets.max_stanza_size`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `max_stanza_size = 10_000`

Maximum allowed incoming stanza size.
!!! Warning
    This limit is checked **after** the input data parsing, so it does not apply to the input data size itself.

#### `listen.http.handlers.mod_websockets.service`
* **Syntax:** an array of `listen.service.*` options
* **Default:** not set
* **Example:**

```toml
[listen.http.handlers.mod_websockets.service]
  access = "all"
  shaper_rule = "fast"
  password = "secret"
```

This subsection enables external component connections over WebSockets.
See the [service](#xmpp-components-listenservice) listener section for details.

### Handler types: REST API - Admin - `mongoose_api_admin`

For more information about the API, see the [REST interface](../rest-api/Administration-backend.md) documentation.
The following options are supported for this handler:

#### `listen.http.handlers.mongoose_api_admin.username`
* **Syntax:** string
* **Default:** not set
* **Example:** `username = "admin"`

When set, enables authentication for the admin API, otherwise it is disabled. Requires setting `password`.

#### `listen.http.handlers.mongoose_api_admin.password`
* **Syntax:** string
* **Default:** not set
* **Example:** `password = "secret"`

Required to enable authentication for the admin API.

### Handler types: REST API - Client

To enable the REST API for clients, several handlers need to be added:

* `mongoose_client_api_*` - handles individual API endpoints. You can add and remove these to enable particular functionality.
* `lasse_handler` - provides the [SSE handler](https://github.com/inaka/lasse) which is required for the client HTTP API, should not be changed.
* `cowboy_*` - hosts the Swagger web-based documentation, should not be changed, but can be removed to disable the API docs.

The recommended configuration is shown in [Example 3](#example-3-client-api) below.
Please refer to [REST interface](../rest-api/Client-frontend.md) documentation for more information.

### Handler types: REST API - Domain management - `mongoose_domain_handler`

This handler enables dynamic domain management for different host types.
For more information about the API, see the [REST interface](../rest-api/Dynamic-domains.md) documentation.
The following options are supported for this handler:

#### `listen.http.handlers.mongoose_domain_handler.username`
* **Syntax:** string
* **Default:** not set
* **Example:** `username = "admin"`

When set, enables authentication to access this endpoint. Requires setting password.

#### `listen.http.handlers.mongoose_domain_handler.password`
* **Syntax:** string
* **Default:** not set
* **Example:** `password = "secret"`

Required to enable authentication for this endpoint.

### Handler types: Metrics API (obsolete) - `mongoose_api`

REST API for accessing the internal MongooseIM metrics.
Please refer to the [REST interface to metrics](../rest-api/Metrics-backend.md) page for more information.
The following option is required:

#### `listen.http.handlers.mongoose_api.handlers`
* **Syntax:** array of strings - Erlang modules
* **Default:** not set, this is a mandatory option for this handler
* **Example:** `handlers = ["mongoose_api_metrics"]`

### Transport options

The options listed below are used to modify the HTTP transport settings.

#### `listen.http.transport.num_acceptors`
* **Syntax:** positive integer
* **Default:** `100`
* **Example:** `transport.num_acceptors = 10`

Number of HTTP connection acceptors.

#### `listen.http.transport.max_connections`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `1024`
* **Example:** `transport.max_connections = "infinity"`

Maximum number of open connections. The default value of 1024 is set by the [Ranch](https://ninenines.eu/docs/en/ranch/1.7/guide/) library.

### TLS (HTTPS) options

By default the HTTP listener does not use TLS.
To use TLS (HTTPS), you need to add a TOML table (subsection) called `tls` to the config file with the `certfile` and `keyfile` options that specify the location of the certificate and private key files, respectively.
If the keyfile is password-protected, `password` is required as well.
If the certificate is signed by an intermediate CA, one will probably want to specify the CA chain with the `cacertfile` option.
The library used for HTTP is the Erlang TLS implementation provided by OTP - see [ranch_ssl](https://github.com/ninenines/ranch/blob/master/doc/src/manual/ranch_ssl.asciidoc) for details.

#### `listen.http.tls.verify_peer`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `tls.verify_peer = true`

Enforces verification of a client certificate. Requires a valid `cacertfile`.

#### `listen.http.tls.verify_mode`
* **Syntax:** string, one of `"peer"`, `"selfsigned_peer"`, `"none"`
* **Default:** not set (equivalent to `"peer"` in the current version of Erlang/OTP)
* **Example:** `tls.verify_mode = "selfsigned_peer"`

Specifies the way certificate verification works:

* `peer` - makes sure the peer's certificate is valid and signed by a trusted CA,
* `selfsigned_peer` - makes sure the peer's certificate is valid, but allows self-signed certificates,
* `none` - any certificate is accepted.

#### `listen.http.tls.certfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.certfile = "server.pem"`

Path to the X509 PEM file with a certificate and a private key (not protected by a password). If the certificate is signed by an intermediate CA, you should specify here the whole CA chain by concatenating all public keys together and appending the private key after that.

#### `listen.http.tls.cacertfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.cacertfile = "ca.pem"`

Path to the X509 PEM file with a CA chain that will be used to verify clients. It won't have any effect if `verify_peer` is not enabled.

#### `listen.http.tls.dhfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.dhfile = "dh.pem"`

Path to the Diffie-Hellman parameter file.

#### `listen.http.tls.ciphers`
* **Syntax:** string with the OpenSSL cipher suite specification
* **Default:** not set, all supported cipher suites are accepted
* **Example:** `tls.ciphers = "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384"`

Cipher suites to use. Please refer to the [OpenSSL documentation](http://www.openssl.org/docs/man1.0.2/apps/ciphers.html) for the cipher string format. For allowed values, see the [Erlang/OTP OpenSSL documentation](http://www.openssl.org/docs/man1.0.2/apps/ciphers.html).

#### `listen.http.tls.versions`
* **Syntax:** array of strings
* **Default:** not set, all supported versions are accepted
* **Example:** `tls.versions = ["tlsv1.2", "tlsv1.3"]`

TLS versions to use. For allowed values, see the [Erlang/OTP SSL documentation](https://erlang.org/doc/man/ssl.html#type-protocol_version)

#### `listen.http.tls.keyfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.keyfile = "key.pem"`

Path to the X509 PEM file with the private key.

#### `listen.http.tls.password`
* **Syntax:** string
* **Default:** not set
* **Example:** `tls.password = "secret"`

Password to the X509 PEM file with the private key.

### Protocol options

These are some additional options of the HTTP protocol.

#### `listen.http.protocol.compress`
* **Syntax:** boolean
* **Default:** false
* **Example:** `protocol.compress = "true"`

Compresses response bodies automatically when the client supports it.

### HTTP listener examples

The examples shown below are included in the provided default configuration file.

#### Example 1. BOSH and WS

The following listener accepts BOSH and WebSocket connections and has TLS configured.

```toml
[[listen.http]]
  port = 5285
  tls.certfile = "mycert.pem"
  tls.keyfile = "mykey.pem"
  tls.password =  "secret"

  [[listen.http.handlers.mod_bosh]]
    host = "_"
    path = "/http-bind"

  [[listen.http.handlers.mod_websockets]]
    host = "_"
    path = "/ws-xmpp"
```

#### Example 2. Admin API

REST API for administration, the listener is bound to `127.0.0.1` for increased security.
The number of acceptors and connections is specified (reduced).

```toml
[[listen.http]]
  ip_address = "127.0.0.1"
  port = 8088
  transport.num_acceptors = 5
  transport.max_connections = 10

  [[listen.http.handlers.mongoose_api_admin]]
    host = "localhost"
    path = "/api"
```

#### Example 3. Client API

REST API for clients.

```toml
[[listen.http]]
  port = 8089
  transport.max_connections = 1024
  protocol.compress = true

  [[listen.http.handlers.lasse_handler]]
    host = "_"
    path = "/api/sse"
    module = "mongoose_client_api_sse"

  [[listen.http.handlers.mongoose_client_api_messages]]
    host = "_"
    path = "/api/messages/[:with]"

  [[listen.http.handlers.mongoose_client_api_contacts]]
    host = "_"
    path = "/api/contacts/[:jid]"

  [[listen.http.handlers.mongoose_client_api_rooms]]
    host = "_"
    path = "/api/rooms/[:id]"

  [[listen.http.handlers.mongoose_client_api_rooms_config]]
    host = "_"
    path = "/api/rooms/[:id]/config"

  [[listen.http.handlers.mongoose_client_api_rooms_users]]
    host = "_"
    path = "/api/rooms/:id/users/[:user]"

  [[listen.http.handlers.mongoose_client_api_rooms_messages]]
    host = "_"
    path = "/api/rooms/[:id]/messages"

  [[listen.http.handlers.cowboy_swagger_redirect_handler]]
    host = "_"
    path = "/api-docs"

  [[listen.http.handlers.cowboy_swagger_json_handler]]
    host = "_"
    path = "/api-docs/swagger.json"

  [[listen.http.handlers.cowboy_static]]
    host = "_"
    path = "/api-docs/[...]"
    type = "priv_dir"
    app = "cowboy_swagger"
    content_path = "swagger"
```

#### Example 4. Domain API

REST API for domain management.

```toml
[[listen.http]]
  ip_address = "127.0.0.1"
  port = 8088
  transport.num_acceptors = 10
  transport.max_connections = 1024

  [[listen.http.handlers.mongoose_domain_handler]]
    host = "localhost"
    path = "/api"
    username = "admin"
    password = "secret"
```
