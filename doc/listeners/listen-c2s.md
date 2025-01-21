# Client to server (C2S): `[[listen.c2s]]`

Handles XMPP client-to-server (C2S) connections.
The recommended [port number](../configuration/listen.md#listenport) for a C2S listener is 5222 [as registered in the XMPP protocol](https://tools.ietf.org/html/rfc6120#section-14.7).

## Configuration options

The following options are supported for each C2S listener:

### `listen.c2s.access`
* **Syntax:** string, rule name or `"all"`
* **Default:** `"all"`
* **Example:** `access = "c2s"`

The rule that determines who is allowed to connect. By default, the rule is `"all"`, which means that anyone can connect. The rule referenced here needs to be defined in the `access` configuration section.

### `listen.c2s.shaper`
* **Syntax:** string, rule name
* **Default:** `"none"` (no shaper)
* **Example:** `shaper = "c2s_shaper"`

The rule that determines what traffic shaper is used to limit the incoming XMPP traffic to prevent the server from being flooded with incoming data.
The rule referenced here needs to be defined in the [`access`](../configuration/access.md) configuration section.
The value of the access rule needs to be either the shaper name or the string `"none"`, which means no shaper.

### `listen.c2s.max_connections`
* **Syntax:** positive integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `max_connections = 10000`

Maximum number of open connections. This is a *soft limit* according to the [Ranch](https://ninenines.eu/docs/en/ranch/2.1/manual/ranch) documentation.

### `listen.c2s.state_timeout`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `5000`
* **Example:** `state_timeout = 10_000`

Timeout value (in milliseconds) used by the C2S state machine when waiting for the connecting client to respond during stream negotiation and SASL authentication. After the timeout the server responds with the `connection-timeout` stream error and closes the connection.

### `listen.c2s.reuse_port`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `reuse_port = true`

Enables linux support for `SO_REUSEPORT`, see [Stack Overflow](https://stackoverflow.com/questions/14388706/how-do-so-reuseaddr-and-so-reuseport-differ) for more details.

### `listen.c2s.backwards_compatible_session`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `backwards_compatible_session = false`

Enables backward-compatible session establishment IQs. See <https://www.rfc-editor.org/rfc/rfc6121.html#section-1.4>:

> [RFC3921] specified one additional
precondition: formal establishment of an instant messaging and
presence session.  Implementation and deployment experience has
shown that this additional step is unnecessary.  However, for
backward compatibility an implementation MAY still offer that
feature.  This enables older software to connect while letting
newer software save a round trip.

### `listen.c2s.allowed_auth_methods`

* **Syntax:** array of strings. Allowed values: `"internal"`, `"rdbms"`, `"external"`, `"anonymous"`, `"ldap"`, `"jwt"`, `"http"`, `"pki"`, `"dummy"`
* **Default:** not set
* **Example:** `allowed_auth_methods = ["internal"]`

A subset of enabled methods to login with for this listener.
This option allows to enable only some backends.
It is useful, if you want to have several listeners for different type of users (for example, some users use PKI while other users use LDAP auth).
Same syntax as for [`auth.methods`](../configuration/auth.md#authmethods) option.

## TLS options for C2S

To enable TLS, a TOML subsection called `tls` has to be present in the listener options.
To disable TLS, make sure that the section is not present, and no TLS options are set.
You can set the following options in this section:

### `listen.c2s.tls.mode`
* **Syntax:** string, one of `"tls"`, `"starttls"`, `"starttls_required"`
* **Default:** `"starttls"`
* **Example:** `tls.mode = "starttls"`

This option determines how clients are supposed to set up the TLS encryption:

* `tls` - clients must initiate a TLS session immediately after connecting, before beginning the normal XML stream,
* `starttls` - enables StartTLS support; requires `certfile`,
* `starttls_required` - enables and enforces StartTLS usage.

### `listen.c2s.tls.module`
* **Syntax:** string, one of `"just_tls"`, `"fast_tls"`
* **Default:** `"just_tls"`
* **Example:** `tls.module = "just_tls"`

By default, the TLS library used for C2S connections is `just_tls` - Erlang TLS implementation provided by OTP.
Usage of `fast_tls`, which uses OpenSSL-based NIFs for C2S is deprecated, however it is still possible to use this option.
Some TLS-related options described here have different formats for these two libraries.

### `listen.c2s.tls.verify_mode`
* **Syntax:** string, one of `"peer"`, `"selfsigned_peer"`, `"none"`
* **Default:** `"peer"`
* **Example:** `tls.verify_mode = "none"`

Specifies the way client certificate verification works:

* `peer` - makes sure the client certificate is valid and signed by a trusted CA. Requires a valid `cacertfile`.
* `selfsigned_peer` - makes sure the client certificate is valid, but allows self-signed certificates; supported only by `just_tls`. Requires a valid `cacertfile`.
* `none` - client certificate is not checked.

### `listen.c2s.tls.certfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.certfile = "server.pem"`

Path to the X509 PEM file with a certificate and a private key (not protected by a password). If the certificate is signed by an intermediate CA, you should specify here the whole CA chain by concatenating all public keys together and appending the private key after that.

!!! Note
    For `just_tls` this file should only contain the certificate and the path to the private key can be provided separately as `keyfile`.

### `listen.c2s.tls.cacertfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.cacertfile = "ca.pem"`

Path to the X509 PEM file with a CA chain that will be used to verify clients. It won't have any effect if `verify_mode` is `"none"`.

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

### `listen.c2s.tls.keyfile` - only for `just_tls`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.keyfile = "key.pem"`

Path to the X509 PEM file with the private key.

### `listen.c2s.tls.password` - only for `just_tls`
* **Syntax:** string
* **Default:** not set
* **Example:** `tls.password = "secret"`

Password to the X509 PEM file with the private key.

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

## C2S listener configuration example

The following section configures two C2S listeners.

```toml
[[listen.c2s]]
  port = 5222
  access = "c2s"
  shaper = "c2s_shaper"
  max_stanza_size = 65536
  tls.certfile = "server.pem"
  tls.dhfile = "dh_server.pem"

[[listen.c2s]]
  port = 5223
  access = "c2s"
  shaper = "c2s_shaper"
  max_stanza_size = 65536
```

* One at port 5222, which accepts a plain TCP connection and allows to use StartTLS for upgrading it to an encrypted one. The files containing the certificate and the DH parameter are also provided.
* One at port 5223, which accepts only encrypted TLS connections - this is the legacy method as StartTLS is preferred.

Both listeners use `c2s` and `c2s_shaper` rules for access management and traffic shaping, respectively.
