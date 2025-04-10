## Overview

Clients connected to MongooseIM may authenticate with their TLS certificates.
This method uses the `SASL EXTERNAL` mechanism.

## Server-side prerequisites

### Properly configure Client-to-server (C2S) listener

A server must request the certificate from a client, so you'll need to set [`verify_mode`](../listeners/listen-c2s.md#listenc2stlsverify_mode) option to `"peer"` and provide a path to CA chain that may be used for client's certificate check ([`cacertfile`](../listeners/listen-c2s.md#listenc2stlscacertfile) option).

Please check the [Listener modules](../listeners/listen-c2s.md) page for more information or simply follow the examples at the end of this section.

### Properly configure `http` listener

SASL EXTERNAL authentication is also possible for WebSocketSecure and BOSH connections over HTTPS.
Similarly as in the `client-to-server` case, the server must request the certificate from the client.
In this case it's enabled by adding the following options to the `tls` option of `listen.http` :

* [`tls.verify_mode = "peer"`](../listeners/listen-http.md#tls-https-options) - this is to tell Erlang's SSL to request the cert from the client
* [`tls.cacertfile = "ca.pem"`](../listeners/listen-http.md#tls-https-options) - this is to tell Erlang's SSL where  the CA cert file is in order to check if the cert is correctly signed

Please check [HTTP-based services configuration](../listeners/listen-http.md#http-based-services-listenhttp) for more details regarding `http` listener configuration.

### Enable `SASL EXTERNAL` mechanism

A `SASL EXTERNAL` authentication mechanism is disabled by default.
In order to enable it, please configure [`auth.sasl_mechanisms` option](../configuration/auth.md#authsasl_mechanisms) in the MongooseIM config file.
```toml
[auth]
  sasl_mechanisms = ["external"]
```

Obviously the list may be longer, if the system should support both the certificate and password based authentication.

The `SASL EXTERNAL` authentication mechanism requires a digital client certificate.
This digital certificate should contain `xmppAddr` field(s), which is always checked first.
If there is more than one JID specified in the `xmppAddr` fields, the client must include the authorisation entity which corresponds to the one of the specified JIDs.

When no `xmppAddr` is specified, the `cn` (common name) field might be used to provide the client's username, but it is optional and can be configured with the [`sasl_external`](../configuration/auth.md#authsasl_external) option in the `auth` section.

If the client certificate does not contain a JID, the client must provide one in authorisation entity.

For the details please refer to [XEP-0178: Best Practices for Use of SASL EXTERNAL with Certificates](https://xmpp.org/extensions/xep-0178.html).

### Enable compatible authentication method

You need to enable one of the following authentication methods by using the [`auth.methods` option](../configuration/auth.md#authmethods) in the MongooseIM configuration file.

* `"pki"` - accepts user credentials,
* `"http"` - accepts user credentials if the provided certificate is [known and valid](../authentication-methods/http.md#method-get_certs)
* `"ldap"` - accepts user credentials if a corresponding user account exists in LDAP.

### Self-signed certificates

By default MongooseIM doesn't accept self-signed certs for the SASL-EXTERNAL authentication.
For development purposes, it is possible to tell MongooseIM to accept them.

#### Self-signed certificates for regular TCP/TLS connections

In order to tell MongooseIM to accept self-signed certs, the `listen.c2s.tls.verify_mode` option needs to be configured like below:

```toml
[listen.c2s]
  tls.verify_mode = "selfsigned_peer"
  tls.disconnect_on_failure = false
  tls.cacertfile = "ca.pem"
```

where the `tls.disconnect_on_failure` is a boolean with the following meaning:

* `true` - the connection is closed if a certificate is invalid,
* `false` - the connection isn't closed, but the certificate is not returned if it's invalid.
  This leads to an authentication failure but allows the client to choose a different auth method if available.

#### Self-signed certificates for WS or BOSH

In order to accept self-signed certs for WS or BOSH connections, the `tls` options for `http` listener must have the following configured:

```toml
[listen.http]
  tls.verify_mode = "selfsigned_peer"
  tls.cacertfile = "ca.pem"
```

### Examples

Certificate authentication only.

```toml
[listen.c2s]
  port = 5222
  (...)
  tls.cacertfile = "ca.pem"
  tls.verify_peer = true

[listen.http]
  port = 5285
  (...)
  tls.cacertfile = "ca.pem"
  tls.verify_peer = true

  [[listen.http.handlers.mod_bosh]]
    host = "_"
    path = "/http-bind"

  [[listen.http.handlers.mod_websockets]]
    host = "_"
    path = "/ws-xmpp"

[auth]
  method = ["pki"]
  sasl_mechanisms = ["external"]
```

Authentication with a client certificate (validated with provided CA chain) or password (validated with data stored in RDBMS).

```toml
[listen.c2s]
  port = 5222
  (...)
  tls.cacertfile = "ca.pem"
  tls.verify_peer = true

[auth]
  methods = ["rdbms", "pki"]
  sasl_mechanisms = ["scram_sha1", "external"]
```

## Client certificate prerequisites

`SASL EXTERNAL` will be offered by the server **only when a client provides a valid certificate**.

Please check documentation of a specific authentication backend you're going to use.

## Usage example - Gajim

Verified with Gajim 0.16.8, installed from package `gajim-0.16.8-1.fc25.noarch`.

### Generate client certificate

```
openssl genrsa -des3 -out rootCA.key 4096
openssl req -x509 -new -nodes -key rootCA.key -sha256 -days 1024 -out rootCA.crt
openssl genrsa -out client.key 2048
openssl req -new -key client.key -out client.csr # Remember to provide username as Common Name!
openssl x509 -req -in client.csr -CA rootCA.crt -CAkey rootCA.key -CAcreateserial -out client.crt -days 500 -sha256
openssl pkcs12 -export -inkey client.key -in client.crt -out client.p12
```

### Configure MongooseIM

See examples in the section above. We recommend using the first snippet for simplicity.

You don't need to pre-create a user account in order to log in with a certificate.

### Add an account in Gajim

1. Edit -> Accounts -> Add.
2. Pick "I already have an account I want to use".
3. Jabber ID is `[Common Name from certificate]@localhost` (domain is different if you've changed it in `hosts` option). Press "Next".
5. Untick "Connect when I press Finish" and press "Advanced".
6. Unfold "Client certificate" and choose the `.p12` you've created earlier. Tick "Certificate is encrypted".
7. Click "Close" and set status to "Available". Tell Gajim to ignore the unverified server certificate (by default it's self-signed).

If Gajim fails to connect, try to restart it.
Version 0.16.8 sometimes "forgets" to ask for the client certificate password.
