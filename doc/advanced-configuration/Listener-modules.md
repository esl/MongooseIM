Some of the MongooseIM modules are specialised in handling user connections.
They can be used in the `listen` clause in the `mongooseim.cfg` file.
See this section for their description and configuration options.

Options described with a value type (e.g. string, integer) are key-value tuples.
Other options are enabled by being added as atoms.
E.g. a tuple option might be: `{access, c2s}` while other options are added as: `starttls`.

## Client-to-server (C2S): `ejabberd_c2s`

Handles pure XMPP client-to-server (C2S) connections, relies on `ejabberd_listener` for listening.
It processes the incoming data from the user client, while the data reception and parsing is executed with `ejabberd_receiver`'s help.
You only need to declare running `ejabberd_c2s`, to have the other 2 modules started and used.

**Default port:** 5222

### Configuration

#### General

* `access` (atom, default: `c2s`) - Access Rule to use for C2S connections.
* `c2s_shaper` (atom, default: `none`) - Connection shaper to use for incoming C2S stanzas.
* `max_stanza_size` (positive integer, default: infinity) - Maximum allowed incoming stanza size.
 **Warning:** this limit is checked **after** the input data parsing, so it does not apply to the input data size itself.
* `backlog` (positive integer, default 100) - overrides the default TCP backlog value
* `max_fsm_queue` (positive integer, the value of this option set global) - message queue limit to prevent resource exhaustion; overrides the global value of this option
* `hibernate_after` (integer, default: 0) - Time in milliseconds after which a client process spawned by this listener will hibernate.
  Hibernation greatly reduces memory consumption of client processes, but *may* result in increased CPU consumption if a client is used *very* frequently.
  The default, recommended value of 0 means that the client processes will hibernate at every opportunity.
* `acceptors_num` (integer, default: 100) - For TCP-based listeners: the number of processes accepting new connections on the listening socket.
* `zlib` (atom or a positive integer, default: disabled) - Enables ZLIB support, the integer value is a limit for a decompressed output size (to prevent successful [ZLIB bomb attack](https://xmpp.org/community/security-notices/uncontrolled-resource-consumption-with-highly-compressed-xmpp-stanzas.html)); the limit can be disabled with an atom 'unlimited'.

#### Common TLS options

* `starttls` (default: disabled) - Enables StartTLS support; requires `certfile`.
* `starttls_required` (default: disabled) - enforces StartTLS usage.
* `tls` (default: disabled) - When this option is set, clients must initiate a TLS session immediately after connecting, before beginning the normal XML stream.
* `tls_module` (atom, default: `fast_tls`) - Provides a TLS library to use. `fast_tls` uses OpenSSL-based NIFs, while `just_tls` uses Erlang TLS implementation provided by OTP. They are fully interchangeable, with some exceptions (`ejabberd_c2s` options supported by only one of them are explicitly described, e.g. `crlfiles`).
* `certfile` (string, default: no certfile will be used) - Path to the X509 PEM file with a certificate and a private key (not protected by a password).
  If the certificate is signed by an intermediate CA, you should specify here the whole CA chain by concatenating all public keys together and appending the private key after that.
* `cafile` (string, default: no CA file will be used) - Path to the X509 PEM file with a CA chain that will be used to verify clients. Won't have any effect if `verify_peer` is not enabled.
* `verify_peer` (default: disabled) - Enforces verification of a client certificate. Requires a valid `cafile`.
* `dhfile` (string, default: no DH file will be used) - Path to the Diffie Hellman parameter file

#### `fast_tls` - specific options

* `ciphers` (string, default: `"TLSv1.2:TLSv1.3"`) - <a name="c2s-ciphers"></a>Cipher suites to use with StartTLS or TLS. Please refer to the [OpenSSL documentation](http://www.openssl.org/docs/man1.0.2/apps/ciphers.html) for the cipher string format.
* `protocol_options` List of OpenSSL options, the default value is `["no_sslv2", "no_sslv3", "no_tlsv1, "no_tlsv1_1"]`. You can find the mappings between supported options and actual OpenSSL flags in the `fast_tls` [source code](https://github.com/processone/fast_tls/blob/master/c_src/options.h).

#### `just_tls` - specific options

* `crlfiles` (list of strings, default: []) - A list of paths to Certificate Revocation Lists.

## HTTP-based services (BOSH, WebSocket, REST): `ejabberd_cowboy`

Manages all HTTP-based services, such as BOSH (HTTP long-polling) and WebSocket.
Unlike `ejabberd_c2s`, it doesn't use `ejabberd_receiver` or `ejabberd_listener`.

**Default port:** 5280

### Configuration

* `ip` (IP tuple, default: `{0,0,0,0}`) - IP address to bind to.
* `num_acceptors` (positive integer, default: 100) - Number of acceptors.
* `transport_options` (proplist, default: []) - Ranch-specific transport options.
 See [ranch:opt()](https://ninenines.eu/docs/en/ranch/1.7/manual/ranch/#_opt).
* `protocol_options` (proplist, default: []) - Protocol configuration options for Cowboy.
 See [Cowboy HTTP module docs](https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy_http/).
* `ssl` (list of ssl options, required for https, no default value) - If specified, https will be used.
 Accepts all ranch_ssl options that don't take fun() parameters.
 Only `certfile` and `keyfile` are mandatory.
 See [ranch_ssl documentation](https://github.com/ninenines/ranch/blob/master/doc/src/manual/ranch_ssl.asciidoc) for details. A minimal usage would be as follows:

        {ssl, [
            {certfile, "priv/ssl/fake_cert.pem"},
            {keyfile, "priv/ssl/fake_key.pem"},
        ]},

    Here, `certfile` and `keyfile` specify the certificate and private key files respectively.
    If the keyfile is password-protected, one will need to specify the password with `{password, "secret"}`.
    If the certificate is signed by an intermediate CA, one will probably want to specify the CA chain with `cacertfile` option.

    Note that `port`, `ip` and `max_connections` are taken from the listener config above and will be ignored if specified under `ssl`.

* `modules` (list of tuples: `{Host, Path, Modules}`) - List of enabled HTTP-based modules. `"_"` equals any host.
    * `mod_bosh` - BOSH connections handler.
     Default declaration:

            `{"_", "/http-bind", mod_bosh}`

    * `mod_websockets` - Websocket connections as defined in [RFC 7395](https://tools.ietf.org/html/rfc7395).
    You can pass optional parameters:
        * `{timeout, Val}` (positive integer, default: infinity) - the time after which an inactive user is disconnected.
        * `{ping_rate, Val}` (positive integer, default: none) - the Ping rate points to the time between pings sent by server.
	 By declaring this field you enable server-side pinging.
        * `{max_stanza_size, Val}` (positive integer, default: infinity) - Maximum allowed incoming stanza size.
          **Warning:** this limit is checked **after** the input data parsing, so it does not apply to the input data size itself.
        * `{ejabberd_service, Params}` (default: []) - this enables external component connections over WebSockets.
	 See the [ejabberd_service](#ejabberd_service) section for more details how to configure it.

        Default declaration:

            `{"_", "/ws-xmpp", mod_websockets, []}`

    * <i>(OBSOLETE)</i> `mongoose_api` - REST API for accessing internal MongooseIM metrics.
        Please refer to the [REST interface to metrics](../rest-api/Metrics-backend.md) page for more information.
	Default declaration:

            `{"localhost", "/api", mongoose_api, [{handlers, [mongoose_api_metrics]}]}`

  * `mongoose_api_admin` -  REST API for admin commands. Exposes all mongoose_commands.
    			    It expects one optional argument:
      * Credentials: `{auth, {Username, Password}}`.
        If they're not provided, authorization is disabled.

     Example:

            `{"localhost", "/api", mongoose_api_admin, [{auth, {<<"ala">>, <<"makotaipsa">>}}]}`

  * `mongoose_api_client` - REST API for client side commands.
     Exposes all mongoose_commands marked as "user".

     Example:

            `{"localhost", "/api/contacts/{:jid}", mongoose_api_client_contacts, []}`

### HTTP module: `mod_cowboy`

This module provides an additional routing layer on top of HTTP(s) or WS(S) protocols.
It allows other HTTP/WS modules to coexist under the same URL on the single port.
Packets are forwarded to them based on the protocol.
This mechanism is transparent to actual handlers so the path sharing does not require any additional code.

Example: If you wish, you can use BOSH and WS XMPP handlers (mod_bosh, mod_websockets) on a single port and a URL without any code modifications.


Here's an example of its configuration (added to ejabberd_cowboy modules list described above):

```Erlang
{"_", "/[...]", mod_cowboy, [{http, mod_revproxy,
                               [{timeout, 5000},
                                % time limit for upstream to respond
                                {body_length, 8000000},
                                % maximum body size (may be infinity)
                                {custom_headers, [{<<"header">>,<<"value">>}]}
                                % list of extra headers that are send to upstream
                               ]},
                               {ws, xmpp, mod_websockets}
                              ]},
```

According to this configuration, all HTTP requests will go through the `mod_revproxy` module (see [mod_revproxy](../modules/mod_revproxy.md) for more details).
As for now, all WebSocket connections with the `Sec-WebSocket-Protocol: xmpp` header, will go through the mod_websockets connection.
This is the MongooseIM's regular websocket connection handler.

## Server-to-server (S2S): `ejabberd_s2s_in`

Handles incoming server-to-server (S2S) connections (federation).
Relies on `ejabberd_listener` and `ejabberd_receiver` just like `ejabberd_c2s`.

**Note:** Many S2S options are configured as top-level config options and they apply to both incoming and outgoing connections.
Please refer to the [Advanced configuration](../Advanced-configuration.md) for more information.

**Default port:** 5269

### Configuration

* `shaper` (atom, default: `none`) - Connection shaper to use for incoming S2S data.
* `max_stanza_size` (positive integer, default: infinity) - Maximum allowed incoming stanza size.
 **Warning:** this limit is checked **after** input data parsing, so it does not apply to the input data size itself.
* `protocol_options` List of supported SSL protocols, default "no_sslv3".
 It also accepts "no_tlsv1" and "no_tlsv1_1"
* `dhfile` (string, default: no DH file will be used) - Path to the Diffie Hellman parameter file
* `ciphers` (string, default: `"TLSv1.2:TLSv1.3"`) - cipher suites to use with StartTLS. <a name="s2s-ciphers"></a>
* `cafile` (string, default: no CA file will be used) - Path to the X509 PEM file with a CA chain that will be used to verify clients (here server initiating S2S connection).

## XMPP components: `ejabberd_service`

Interface for XMPP components ([XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html)), enabling communication between servers and "external" components over the XMPP network.

**Default port:** 8888

### Configuration

* `access` (atom, default: `all`) - Access Rule to use for incoming component connections.
* `password` (string) - the service is protected with this password
* `shaper_rule` (atom, default: `none`) - Connection shaper to use for incoming component traffic.
* `service_check_from` (boolean, default: `true`) - Checks whether the server should verify the "from" field in stanzas from the component
* `max_fsm_queue` (positive integer, the value of this option set global) - message queue limit to prevent resource exhaustion; overrides the global value of this option
* `hidden_components` (boolean, default: `false`) - All components connected to an endpoint with this option enabled will be considered "hidden" (see explanation below).
* `conflict_behaviour` (`disconnect`, `kick_old`, default: `disconnect`) - If set to `kick_old`, in case of a routing conflict it stops the previous connection (see the explanation below). 

According to ([XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html)) component's hostname should be given in the <stream:stream> element.

### Custom extension to the protocol

In order to register a component for all virtual hosts served by the server (listed in global variable hosts), the component must add the attribute `is_subdomain="true"` to the opening stream element.
This maybe helpful if someone wants to have a single instance of a component serving multiple virtual hosts.
The `is_subdomain` attribute is optional and the default behaviour is as described in the XEP.

### Hidden components

Hidden components have a special flag enabled in the internal component table.
Alone, it doesn't change the server behaviour in any way, but it may be used by other modules and extensions to execute special logic.
An example would be [`mod_disco`](../modules/mod_disco.md), which may be configured to filter out hidden components from disco results, so they won't be discoverable by clients.
A reason to do so could be reduced traffic - systems with many components could return very long disco responses.
Also, some deployments would like to avoid revealing some services; not because it is a security threat (this method does not prevent clients from communicating with hidden components), but rather because they are not meant to interact with clients directly (e.g. helper components for other components).

## Conflict behaviour

By default, when a component tries to connect and a registration conflict occures, we drop such connection by sending:

```xml
<stream:error>
  <conflict xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>
</stream:error>
</stream:stream>
```

It makes implementing the reconnection logic difficult, because the old connection would not allow any other connections.

By setting `{conflict_behaviour, kick_old}`, we drop any old connections registered at the same host, before accepting new ones.

