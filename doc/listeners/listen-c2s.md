# Client to server (C2S): `[[listen.c2s]]`

Handles XMPP client-to-server (C2S) connections.
The recommended [port number](../configuration/listen.md#listenport) for a C2S listener is 5222 [as registered in the XMPP protocol](https://tools.ietf.org/html/rfc6120#section-14.7).

## Configuration options

For each C2S listener, all the [general](../configuration/listen.md#general-listener-options) and [XMPP](../configuration/listen.md#xmpp-listener-options) options are accepted. Additionally, the following options are supported:

### `listen.c2s.access`
* **Syntax:** string, rule name or `"all"`
* **Default:** `"all"`
* **Example:** `access = "c2s"`

The rule that determines who is allowed to connect. By default, the rule is `"all"`, which means that anyone can connect. The rule referenced here needs to be defined in the `access` configuration section.

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

### `listen.c2s.tls.verify_mode`
* **Syntax:** string, one of `"peer"`, `"selfsigned_peer"`, `"none"`
* **Default:** `"peer"`
* **Example:** `tls.verify_mode = "none"`

Specifies the way client certificate verification works:

* `peer` - makes sure the client certificate is valid and signed by a trusted CA.
* `selfsigned_peer` - makes sure the client certificate is valid, but allows self-signed certificates.
* `none` - client certificate is not checked.

Options: `peer` and `selfsigned_peer` will use certificates specified in `cacertfile` or system certificates, if `cacertfile` is not provided.

### `listen.c2s.tls.certfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.certfile = "cert.pem"`

Path to the X509 PEM file with a certificate (not protected by a password). If the certificate is signed by an intermediate CA, you should specify here the whole CA chain by concatenating all public keys together.

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
* **Default:** this option is not set by default - all supported suites are accepted.
* **Example:** `tls.ciphers = "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384"`

Cipher suites to use with StartTLS or TLS. Please refer to the [OpenSSL documentation](https://docs.openssl.org/master/man1/openssl-ciphers/) for the cipher string format. See the [Erlang/OTP SSL documentation](https://erlang.org/doc/man/ssl.html#t:ciphers/0) for allowed values.

### `listen.c2s.tls.keyfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.keyfile = "key.pem"`

Path to the X509 PEM file with the private key.

### `listen.c2s.tls.password`
* **Syntax:** string
* **Default:** not set
* **Example:** `tls.password = "secret"`

Password to the X509 PEM file with the private key.

### `listen.c2s.tls.disconnect_on_failure`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `tls.disconnect_on_failure = false`

This option specifies what happens when client certificate is verified during TLS handshake.
It therefore only applies when client certificate verification is enabled, that is `tls.verify_mode` is set to `"peer"` or `"selfsigned_peer"`.

When set to `true`, client verification is performed during TLS handshake and in case of error the connection is aborted.
Additionally empty client certificate is treated as an error.

When set to `false`, TLS handshake will succeed even if there were errors in client certificate verification.
This allows to use other methods of authentication (like SASL) later as part of XMPP stream.

### `listen.c2s.tls.versions`
* **Syntax:** array of strings
* **Default:** not set, all supported versions are accepted
* **Example:** `tls.versions = ["tlsv1.2", "tlsv1.3"]`

TLS versions to use with StartTLS or TLS. For allowed values, see the [Erlang/OTP SSL documentation](https://erlang.org/doc/man/ssl.html#type-protocol_version)

### `listen.c2s.tls.crl_files`
* **Syntax:** array of strings, paths in the file system
* **Default:** not set
* **Example:** `tls.crl_files = ["certs.crl"]`

Specifies the paths to Certificate Revocation Lists.

### `listen.c2s.tls.early_data`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `tls.early_data = true`

Enables `early_data`, or 0-RTT, used with [mod_fast_auth_token](../modules/mod_fast_auth_token.md) module.
Use this with Direct TLS (i.e. port 5223).

### `listen.c2s.tls.session_tickets`
* **Syntax:** string: `"stateless"`
* **Default:** not set
* **Example:** `tls.session_tickets = "stateless"`

Enables TLS [Session Tickets](https://www.erlang.org/doc/apps/ssl/using_ssl#session-tickets-and-session-resumption-in-tls-1-3),
which could be used for faster TLS connection, skipping one roundtrip when connecting.

## C2S listener configuration example

The following section configures two C2S listeners.

```toml
[[listen.c2s]]
  port = 5222
  access = "c2s"
  shaper = "normal"
  max_stanza_size = 65536
  tls.certfile = "cert.pem"
  tls.keyfile = "key.pem"
  tls.dhfile = "dh_server.pem"

[[listen.c2s]]
  port = 5223
  access = "c2s"
  shaper = "normal"
  max_stanza_size = 65536
  tls.mode = "tls"
  tls.certfile = "cert.pem"
  tls.keyfile = "key.pem"
  tls.dhfile = "dh_server.pem"
```

* One at port 5222, which accepts a plain TCP connection and allows to use StartTLS for upgrading it to an encrypted one. The files containing the certificate and the DH parameter are also provided.
* One at port 5223, which accepts only encrypted TLS connections. It is called Direct TLS.

Both listeners use the `c2s` access rule and the `normal` traffic shaper.
They need to be defined in the [`access`](../configuration/access.md) and [`shaper`](../configuration/shaper.md) sections, respectively (the default configuration file includes them).
