Some of MongooseIM modules are specialised in handling user connections. They can be used in the `listen` clause in `ejabberd.cfg` file. See this section for their description and configuration options.

Options described with a value type (e.g. string, integer) are key-value tuples.
Other options are enabled by being added as atoms. E.g. a tuple option might be: `{access, c2s}` while other options are added as: `starttls`.

## Client-to-server (C2S): `ejabberd_c2s`

Handles pure XMPP client-to-server (C2S) connections, relies on `ejabberd_listener` for listening. It processes the incoming data from the user client, while the data reception and parsing is executed with `ejabberd_receiver`'s help. You only need to declare running `ejabberd_c2s’, to have the other 2 modules started and used.

**Default port:** 5222

### Configuration

* `certfile` (string, optional, no default value) - Path to X509 PEM file with certificate and private key (not protected by password).
* `starttls` (optional, default: disabled) - Enables StartTLS support; requires `certfile`.
* `starttls_required` (optional, default: disabled) - enforces StartTLS usage.
* `zlib` (atom or positive integer, default: disabled) - Enables ZLIB support, integer value is a limit for decompressed output size (to prevent successful [ZLIB bomb attack](http://xmpp.org/resources/security-notices/uncontrolled-resource-consumption-with-highly-compressed-xmpp-stanzas/)); limit can be disabled with atom 'unlimited'.
* `ciphers` (string, optional, default: as of OpenSSL 1.0.0 it's `ALL:!aNULL:!eNULL` [(source)](https://www.openssl.org/docs/apps/ciphers.html#CIPHER_STRINGS)) - cipher suites to use with StartTLS. Please refer to [OpenSSL documentation](http://www.openssl.org/docs/apps/ciphers.html) for cipher string format.
* `access` (atom, default: `c2s`) - Access Rule to use for C2S connections.
* `c2s_shaper` (atom, default: `c2s_shaper`) - Connection shaper to use for incoming C2S stanzas.
* `max_stanza_size` (positive integer, default: 65536) - Maximum allowed incoming stanza size. **Warning:** this limit is checked **after** input data parsing, so it does not limit the input data size itself.
* `backlog` (positive integer, default 100) - overrides default TCP backlog value
* `max_fsm_queue` (positive integer, the value of this option set global) - message queue limit to prevent resource exhaustion; overrides the global value of this option
* `protocol_options` List of supported SSL protocols, default "no_sslv3". It also accepts "no_tlsv1" and "no_tlsv1_1"
* `dhfile` (string, optional, no default value) - Path to Diffie Hellman parameters file

## HTTP-based services (BOSH, WebSocket, REST): `ejabberd_cowboy`

Manages all HTTP-based services, such as BOSH (HTTP long-polling) and WebSocket. Unlike `ejabberd_c2s`, it doesn't use `ejabberd_receiver` or `ejabberd_listener`.

**Default port:** 5280

### Configuration

* `ip` (IP tuple, optional, default: `{0,0,0,0}`) - IP address to bind to.
* `num_acceptors` (positive integer, optional, default: 100) - Number of acceptors.
* `max_connections` (positive integer, optional, default: 1024) - Maximum total number of HTTP(S) connections.
* `ssl` (list of ssl options, required for https, no default value) - If specified, https will be used. Accepts all ranch_ssl options that don't take fun() parameters. Only `certfile` and `keyfile` are mandatory. See [ranch_ssl documentation](https://github.com/ninenines/ranch/blob/master/doc/src/manual/ranch_ssl.asciidoc) for details. A minimal usage would be as follows:

        {ssl, [
            {certfile, "priv/ssl/fake_cert.pem"},
            {keyfile, "priv/ssl/fake_key.pem"},
        ]},

    Here, `certfile` and `keyfile` specify the certificate and private key files respectively. If the keyfile is password-protected, one will need to specify the password with `{password, "secret"}`. If the certificate is signed by an intermediate CA, one will probably want to specify the CA chain with `cacertfile` option.

    Note that `port`, `ip` and `max_connections` are taken from the listener config above and will be ignored if specified under `ssl`.

* `modules` (list of tuples: `{Host, Path, Modules}`) - List of enabled HTTP-based modules. `"_"` equals any host.
    * `mod_bosh` - BOSH connections handler. Default declaration:

            `{"_", "/http-bind", mod_bosh}`

    * `mod_websockets` - Websocket connections as defined in  [RFC 7395](https://tools.ietf.org/html/rfc7395).
    You can pass optional parameters:
        * `{timeout, Val}` - the time after which an inactive user is disconnected.
        * `{ping_rate, Val}` - the Ping rate points to the time between pings sent by server.
	By declaring this field you enable server-side pinging.
        * `{ejabberd_service, Params}` - this enables external component
            connections over WebSockets. See [ejabberd_service](#ejabberd_service)
            section for more details how to configure it.

        Default declaration:

            `{"_", "/ws-xmpp", mod_websockets, []}`

    * <i>(OBSOLETE)</i> `mongoose_api` - REST API for accessing internal MongooseIM metrics.
        Please refer to [REST interface to metrics](../developers-guide/REST-interface-to-metrics.md)
        for more information. Default declaration:

            `{"localhost", "/api", mongoose_api, [{handlers, [mongoose_api_metrics]}]}`

  * `mongoose_api_admin` -  REST API for admin commands. Exposes all mongoose_commands
    `mongoose_api_client` - REST API for client side commands. Exposes all mongoose_commands marked as "user"
        Example declaration:

            `{"localhost", "/api", mongoose_api_admin, []}`

### HTTP module: `mod_cowboy`

This module provides additional routing layer on top of HTTP(s) or
WS(S) protocols. Example configuration looks like the following (add
this to ejabberd_cowboy modules list described above):

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

According to this configuration, all HTTP requests will go through the
`mod_revproxy` module (see [mod_revproxy](../modules/mod_revproxy.md)
for more details).
As for now, all WebSockets connections with the `Sec-WebSocket-Protocol: xmpp`
header, will go through the mod_websockets connection.
This is the MongooseIM's regular websocket connection handler.

## Server-to-server (S2S): `ejabberd_s2s_in`

Handles incoming server-to-server (S2S) connections (federation). Relies on `ejabberd_listener` and `ejabberd_receiver` just like `ejabberd_c2s`.

**Note:** Many S2S options are configured as top-level config options and they apply to both incoming and outgoing connections. Please refer to [Advanced configuration](../Advanced-configuration.md) for more information.

**Default port:** 5269

### Configuration

* `shaper` (atom, default: `s2s_shaper`) - Connection shaper to use for incoming S2S data.
* `max_stanza_size` (positive integer, default: 131072) - Maximum allowed incoming stanza size. **Warning:** this limit is checked **after** input data parsing, so it does not limit the input data size itself.
* `protocol_options` List of supported SSL protocols, default "no_sslv3". It also accepts "no_tlsv1" and "no_tlsv1_1"
* `dhfile` (string, optional, no default value) - Path to Diffie Hellman parameters file

## XMPP components: `ejabberd_service`

Interface for external XMPP components ([XEP-0114: Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html)), enabling communication between servers and "external" components over the XMPP network.

**Default port:** 8888

### Configuration

* `access` (atom, default: `all`) - Access Rule to use for incoming component connections.
* `hosts` (tuple: `{hosts, [Domain1, Domain2, ...], [{password, "password here"}]}`, optional when `host` present) - List of domains allowed for components, protected by specified password. If set, `host` is ignored.
* `host` ( tuple: `{host, Domain, [{password, "password here"}]}`, optional when `hosts` present) - Only allowed domain for components, protected by password. Must be set when `hosts` not present.
* `shaper_rule` (atom, default: `fast`) - Connection shaper to use for incoming component traffic.
* `service_check_from` (boolean, default: `true`) - Checks whether the server should verify the "from" field in stanzas from component
* `max_fsm_queue` (positive integer, the value of this option set global) - message queue limit to prevent resource exhaustion; overrides the global value of this option

### Custom extension to the protocol

In order to register a component for all virtual hosts served by the
server, the component must add attribute `is_subdomain="true"`to the opening stream element.
This maybe helpful if someone wants to have a single instance of a
component serving multiple virtual hosts. The `is_subdomain` attribute
is optional and the default behaviour is as described in the XEP.
