# HTTP-based services: `[[listen.http]]`

Manages all HTTP-based services, such as BOSH (HTTP long-polling), WebSocket, GraphQL and REST.
It uses the [Cowboy](https://ninenines.eu/docs/en/cowboy/2.6/manual) web server.
Recommended port number: 5280 for BOSH/WS.

## Configuration options

For each HTTP listener, all the [general](../configuration/listen.md#general-listener-options) options are accepted. Additionally, the following configuration option is used to set up HTTP handlers:

### `listen.http.handlers`
* **Syntax:** each handler is specified in a subsection starting with `[[listen.http.handlers.type]]` where `type` is one of the allowed handler types, handling different connection types:

    * `mod_bosh` - for [BOSH](https://xmpp.org/extensions/xep-0124.html) connections,
    * `mod_websockets` - for [WebSocket](https://tools.ietf.org/html/rfc6455) connections,
    * `mongoose_prometheus_handler` - for [Prometheus](https://prometheus.io/) metrics,
    * `mongoose_graphql_handler` - for GraphQL API,
    * `mongoose_admin_api`, `mongoose_client_api` - for REST API.

    These types are described below in more detail.
    The double-bracket syntax is used because there can be multiple handlers of a given type, so for each type there is a TOML array of one or more tables (subsections).

* **Default:** `[]` - no handlers enabled, all of them need to be specified explicitly.
* **Example:** two handlers, one for BOSH and one for WebSockets
```toml
  [[listen.http.handlers.mod_bosh]]
    host = "_"
    path = "/http-bind"

  [[listen.http.handlers.mod_websockets]]
    host = "_"
    path = "/ws-xmpp"
```

## Common handler options

### `listen.http.handlers.*.host`
* **Syntax:** string
* **Default:** no default, mandatory option
* **Example:** `host = "localhost"`

Host name for this handler or `"_"` for any host.

### `listen.http.handlers.*.path`
* **Syntax:** string
* **Default:** no default, mandatory option
* **Example:** `path = "/ws-xmpp"`

Path for this handler.

## Handler types: BOSH - `mod_bosh`

The recommended configuration is shown in [Example 1](#example-1-bosh-and-ws) below.
To handle incoming BOSH traffic you need to configure the `mod_bosh` module in the `modules` section as well.

## Handler types: WebSockets - `mod_websockets`

The recommended configuration is shown in [Example 1](#example-1-bosh-and-ws) below.
Websocket connections as defined in [RFC 7395](https://tools.ietf.org/html/rfc7395).
You can pass the following optional parameters:

### `listen.http.handlers.mod_websockets.timeout`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `timeout = 60_000`

The time (in milliseconds) after which an inactive user is disconnected.

### `listen.http.handlers.mod_websockets.ping_rate`
* **Syntax:** positive integer
* **Default:** not set - pings disabled
* **Example:** `ping_rate = 10_000`

The time (in milliseconds) between pings sent by server. By setting this option you enable server-side pinging.

### `listen.http.handlers.mod_websockets.max_stanza_size`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `max_stanza_size = 10_000`

Maximum allowed incoming stanza size in bytes.
!!! Warning
    This limit is checked **after** the input data parsing, so it does not apply to the input data size itself.

### `listen.http.handlers.mod_websockets.state_timeout`

Same as the [XMPP option](../configuration/listen.md#listenstate_timeout).

### `listen.http.handlers.mod_websockets.backwards_compatible_session`

Same as the [C2S option](listen-c2s.md#listenc2sbackwards_compatible_session).

## Handler types: GraphQL API - `mongoose_graphql_handler`

For more information about the API, see the [Admin interface](../graphql-api/Admin-GraphQL.md) and [User interface](../graphql-api/User-GraphQL.md) documentation.
The following options are supported for this handler:

### `listen.http.handlers.mongoose_graphql_handler.schema_endpoint`
* **Syntax:** string, one of `"admin"`, `"domain_admin"`, `"user"`
* **Default:** no default, this option is mandatory
* **Example:** `schema_endpoint = "admin"`

Specifies the schema endpoint:

* `admin` - Endpoint with the admin commands. A global admin has permission to execute all commands. See the recommended configuration -  [Example 2](#example-2-admin-graphql-api).
* `domain_admin` - Endpoint with the admin commands. A domain admin has permission to execute only commands with the owned domain. See the recommended configuration - [Example 3](#example-3-domain-admin-graphql-api).
* `user` - Endpoint with the user commands. Used to manage the authorized user. See the recommended configuration - [Example 4](#example-4-user-graphql-api).

### `listen.http.handlers.mongoose_graphql_handler.username` - only for `admin`
* **Syntax:** string
* **Default:** not set
* **Example:** `username = "admin"`

When set, enables authentication for the admin API, otherwise it is disabled. Requires setting `password`.

### `listen.http.handlers.mongoose_graphql_handler.password` - only for `admin`
* **Syntax:** string
* **Default:** not set
* **Example:** `password = "secret"`

### `listen.http.handlers.mongoose_graphql_handler.allowed_categories`
* **Syntax:** non-empty array of strings. Allowed values: `"checkAuth", "account", "domain", "last", "muc", "muc_light", "session", "stanza", "roster", "vcard", "private", "metric", "stat", "gdpr", "mnesia", "server", "inbox", "http_upload", "offline", "token"`
* **Default:** all GraphQL categories enabled
* **Example:** `allowed_categories = ["domain", "last"]`

By default, when the option is not included, all GraphQL categories are enabled, so you don't need to add this option.
When this option is added, only listed GraphQL categories will be processed. For others, the error "category disabled" will be returned.

### `listen.http.handlers.mongoose_graphql_handler.sse_idle_timeout`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** 3600000
* **Example:** `schema_endpoint = "admin"`

This option specifies the time in milliseconds after which the SSE connection is closed when idle.
The default value is 1 hour.

## Handler types: REST API - Admin - `mongoose_admin_api`

The recommended configuration is shown in [Example 5](#example-5-admin-rest-api) below.
For more information about the API, see the [REST interface](../rest-api/Administration-backend.md) documentation.
The following options are supported for this handler:

### `listen.http.handlers.mongoose_admin_api.username`
* **Syntax:** string
* **Default:** not set
* **Example:** `username = "admin"`

When set, enables authentication for the admin API, otherwise it is disabled. Requires setting `password`.

### `listen.http.handlers.mongoose_admin_api.password`
* **Syntax:** string
* **Default:** not set
* **Example:** `password = "secret"`

Required to enable authentication for the admin API.

### `listen.http.handlers.mongoose_admin_api.handlers`
* **Syntax:** array of strings. Allowed values: `"contacts"`, `"users"`, `"sessions"`, `"messages"`, `"stanzas"`, `"muc_light"`, `"muc"`, `"inbox"`, `"domain"`, `"metrics"`.
* **Default:** all API handler modules enabled
* **Example:** `handlers = ["domain"]`

The admin API consists of several handler modules, each of them implementing a subset of the functionality.
By default, all modules are enabled, so you don't need to change this option.

## Handler types: REST API - Client - `mongoose_client_api`

The recommended configuration is shown in [Example 6](#example-6-client-rest-api) below.
Please refer to [REST interface](../rest-api/Client-frontend.md) documentation for more information.
The following options are supported for this handler:

### `listen.http.handlers.mongoose_client_api.handlers`
* **Syntax:** array of strings. Allowed values: `"sse"`, `"messages"`, `"contacts"`, `"rooms"`, `"rooms_config"`, `"rooms_users"`, `"rooms_messages"`.
* **Default:** all API handler modules enabled
* **Example:** `handlers = ["messages", "sse"]`

The client API consists of several handler modules, each of them implementing a subset of the functionality.
By default, all modules are enabled, so you don't need to change this option.

### `listen.http.handlers.mongoose_client_api.docs`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `docs = "false"`

The Swagger documentation of the client API is hosted at the `/api-docs` path.
You can disable the hosted documentation by setting this option to `false`.

## Handler types: Prometheus - `mongoose_prometheus_handler`

Requires no additional options other than the [common handler options](#common-handler-options) in the listener section.
In order to collect useful metrics, a `[prometheus]` section has to be added in [the instrumentation section](../configuration/instrumentation.md#).
The default configuration available with MongooseIM is shown in [Example 7](#example-7-prometheus) below.

## Transport options

The options listed below are used to modify the HTTP transport settings.

### `listen.http.transport.num_acceptors`
* **Syntax:** positive integer
* **Default:** `100`
* **Example:** `transport.num_acceptors = 10`

Number of HTTP connection acceptors.

### `listen.http.transport.max_connections`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `1024`
* **Example:** `transport.max_connections = "infinity"`

Maximum number of open connections. The default value of 1024 is set by the [Ranch](https://ninenines.eu/docs/en/ranch/2.1/guide/) library.

## TLS (HTTPS) options

By default, the HTTP listener does not use TLS.
To use TLS (HTTPS), you need to add a TOML table (subsection) called `tls` to the config file with the `certfile` and `keyfile` options that specify the location of the certificate and private key files, respectively.
If the keyfile is password-protected, `password` is required as well.
If the certificate is signed by an intermediate CA, one will probably want to specify the CA chain with the `cacertfile` option.
The library used for HTTP is the Erlang TLS implementation provided by OTP - see [ranch_ssl](https://github.com/ninenines/ranch/blob/master/doc/src/manual/ranch_ssl.asciidoc) for details.

The options accepted here are: `verify_mode`, `certfile`, `cacertfile`, `ciphers`, `keyfile`, `password`, `versions`, `dhfile`. They have the same semantics as the corresponding [c2s options](listen-c2s.md#tls-options-for-c2s).

## Protocol options

These are some additional options of the HTTP protocol.

### `listen.http.protocol.compress`
* **Syntax:** boolean
* **Default:** false
* **Example:** `protocol.compress = "true"`

Compresses response bodies automatically when the client supports it.

## HTTP listener configuration examples

The examples shown below are included in the provided default configuration file.

### Example 1. BOSH and WS

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

### Example 2. Admin GraphQL API

GraphQL API for administration, the listener is bound to 127.0.0.1 for increased security. The number of acceptors and connections is specified (reduced).

```toml
[[listen.http]]
  ip_address = "127.0.0.1"
  port = 5551
  transport.num_acceptors = 5
  transport.max_connections = 10

  [[listen.http.handlers.mongoose_graphql_handler]]
    host = "localhost"
    path = "/api/graphql"
    schema_endpoint = "admin"
    username = "admin"
    password = "secret"
    allowed_categories = ["server", "last", "vcard"]
```

### Example 3. Domain Admin GraphQL API

GraphQL API for the domain admin.

```toml
[[listen.http]]
  ip_address = "0.0.0.0"
  port = 5541
  transport.num_acceptors = 10
  transport.max_connections = 1024

  [[listen.http.handlers.mongoose_graphql_handler]]
    host = "_"
    path = "/api/graphql"
    schema_endpoint = "domain_admin"
```

### Example 4. User GraphQL API

GraphQL API for the user.

```toml
[[listen.http]]
  ip_address = "0.0.0.0"
  port = 5561
  transport.num_acceptors = 10
  transport.max_connections = 1024

  [[listen.http.handlers.mongoose_graphql_handler]]
    host = "_"
    path = "/api/graphql"
    schema_endpoint = "user"
```

### Example 5. Admin REST API

REST API for administration, the listener is bound to `127.0.0.1` for increased security.
The number of acceptors and connections is specified (reduced).
Basic HTTP authentication is used as well.

```toml
[[listen.http]]
  ip_address = "127.0.0.1"
  port = 8088
  transport.num_acceptors = 5
  transport.max_connections = 10

  [[listen.http.handlers.mongoose_admin_api]]
    host = "localhost"
    path = "/api"
    username = "admin"
    password = "secret"
```

### Example 6. Client REST API

REST API for clients.

```toml
[[listen.http]]
  port = 8089
  transport.max_connections = 1024
  protocol.compress = true

  [[listen.http.handlers.mongoose_client_api]]
    host = "_"
    path = "/api"
```

### Example 7. Prometheus

Prometheus metrics endpoint.

```toml
[[listen.http]]
  port = 9091

  transport.num_acceptors = 10

  [[listen.http.handlers.mongoose_prometheus_handler]]
    host = "_"
    path = "/metrics"
```
