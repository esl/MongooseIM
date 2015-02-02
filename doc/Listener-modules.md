Some of MongooseIM modules are specialised in handling user connections. They can be used in the `listen` clause in `ejabberd.cfg` file. See this section for their description and configuration options.

Options described with a value type (e.g. string, integer) are key-value tuples. 
Other options are enabled by being added as atoms. E.g. a tuple option might be: `{access, c2s}` while other options are added as: `starttls`

## ejabberd_c2s

Handles pure XMPP connections, relies on `ejabberd_listener` for listening. It processes the incoming data from the user client, while the data reception and parsing is executed with `ejabberd_recevier`'s help. You only need to declare running `ejabberd_c2s’, to have the other 2 modules started and used.

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

## ejabberd_cowboy

Manages all HTTP-based services. Unlike `ejabberd_c2s`, it doesn't use `ejabberd_receiver` or `ejabberd_listener`.

Currently it is not possible to use different ports e.g. for BOSH and Websockets.

**Default port:** 5280

### Configuration

* `ip` (IP tuple, optional, default: `{0,0,0,0}`) - IP address to bind to.
* `num_acceptors` (positive integer, optional, default: 100) - Number of acceptors.
* `max_connections` (positive integer, optional, default: 1024) - Maximum total number of HTTP(S) connections.
* `cert` (string, optional, no default value) - Path to the SSL certificate in X509 format.
* `key` (string, optional, no default value) - Path to the SSL private key in X509 format.
* `key_pass` (string, optional, default: `undefined`) - Password to a private key, `undefined` for no password.
* `modules` (list of tuples: `{Host, Path, Modules}`) - List of enabled HTTP-based modules. `"_"` equals any host.
  * `mod_bosh` - BOSH connections handler. Default declaration: `{"_", "/http-bind", mod_bosh}`
  * `mod_websockets` - Websocket connections, both [old](http://xmpp.org/extensions/xep-0206.html) and [new](http://datatracker.ietf.org/doc/draft-ietf-xmpp-websocket/?include_text=1) type. Default declaration: `{"_", "/ws-xmpp", mod_websockets}`
  * `mod_metrics` - REST API for accessing internal MongooseIM metrics. Please refer to [REST interface to folsom metrics](REST-interface-to-folsom-metrics.md) for more information. Default declaration: `{"localhost", "/metrics", mod_metrics}`.

## ejabberd_s2s_in

Handles incoming S2S connections. Relies on `ejabberd_listener` and `ejabberd_receiver` just like `ejabberd_c2s`.

**Note:** Many S2S options are configured as top-level config options and they apply to both incoming and outgoing connections. Please refer to [Advanced configuration](Advanced-configuration.md) for more information.

**Default port:** 5269

### Configuration

* `shaper` (atom, default: `s2s_shaper`) - Connection shaper to use for incoming S2S data.
* `max_stanza_size` (positive integer, default: 131072) - Maximum allowed incoming stanza size. **Warning:** this limit is checked **after** input data parsing, so it does not limit the input data size itself.

## ejabberd_service

Interface for external [XMPP components](http://xmpp.org/extensions/xep-0114.html).

**Default port:** 8888

### Configuration

* `access` (atom, default: `all`) - Access Rule to use for incoming component connections.
* `hosts` (tuple: `{hosts, [Domain1, Domain2, ...], [{password, "password here"}]}`, optional when `host` present) - List of domains allowed for components, protected by specified password. If set, `host` is ignored.
* `host` ( tuple: `{host, Domain, [{password, "password here"}]}`, optional when `hosts` present) - Only allowed domain for components, protected by password. Must be set when `hosts` not present.
* `shaper_rule` (atom, default: `fast`) - Connection shaper to use for incoming component traffic.
* `service_check_from` (boolean, default: `true`) - Checks whether the server should verify the "from" field in stanzas from component