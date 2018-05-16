## Overview

Clients connected to MongooseIM may authenticate with their TLS certificates.
This method is based on combination of [`ejabberd_auth_pki` backend](../authentication-backends/PKI-authentication-module.md) and `SASL EXTERNAL` mechanism.

## Server-side prerequisites

### Properly configure `ejabberd_c2s` listener

By default MongooseIM uses `fast_tls` driver but unfortunatelly it doesn't implement an API to get the client certificate.
In order for this method to work properly, we'll need `just_tls` module (`tls_module` option), which uses an OTP TLS implementation.

Proper client verification is required as well, so you'll need to enable `verify_peer` option and provide a path to a certificate of a CA that signed client's certificate (`cafile` option).

Please check [Listener modules](../advanced-configuration/Listener-modules.md#client-to-server-c2s-ejabberd_c2s) page for more information or simply follow the examples at the end of this section.

### Enable `SASL EXTERNAL` method

A `SASL EXTERNAL` authentication method is disabled by default.
In order to enable it, please add [`sasl_mechanisms` option](../Advanced-configuration.md#authentication) to MongooseIM config file.
Its value must include a `cyrsasl_external` item.
Obviously the list may be longer, if the system should support both certificate and password based authentication.

### Enable PKI authentication backend

A PKI authentication backend extracts username from certificate's Common Name.
Please modify [`auth_opts` option](../Advanced-configuration.md#authentication) in MongooseIM's config file to include `pki` item.

### Examples

Certificate authentication only.

```
{listen, [
           (...)
           {5222, ejabberd_c2s, [
                                  (...)
                                  {cafile, "/path/to/ca.pem"},
                                  {tls_module, just_tls},
                                  verify_peer,
                                  (...)
                                ]},
           (...)
         ]}.

{auth_method, [pki]}.

{sasl_mechanisms, [cyrsasl_external]}.
```

Authentication with a client certificate (validated with provided CA certificate) or password (validated with data stored in RDBMS).

```
{listen, [
           (...)
           {5222, ejabberd_c2s, [
                                  (...)
                                  {cafile, "/path/to/ca.pem"},
                                  {tls_module, just_tls},
                                  verify_peer,
                                  (...)
                                ]},
           (...)
         ]}.


{auth_method, [odbc, pki]}.

{sasl_mechanisms, [cyrsasl_scram, cyrsasl_external]}.
```

## Client certificate prerequisites

All the client must provide, is a certificate with the username in Common Name.

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

See examples before this section. Configuration without `odbc` and `cyrsasl_scram` is sufficient.

You don't need to pre-create user account in order to log in with certificate.

### Add an account in Gajim

1. Edit -> Accounts -> Add.
2. Pick "I already have an account I want to use".
3. Jabber ID is `[Common Name from certificate]@localhost` (domain is different if you've changed it in `hosts` option). Press "Next".
5. Untick "Connect when I press Finish" and press "Advanced".
6. Unfold "Client certificate" and choose the `.p12` you've created earlier. Tick "Certificate is encrypted".
7. Click "Close" and set status to "Available". Tell Gajim to ingnore unverified server certificate (by default it's self-signed).

If Gajim fails to connect, try to restart it.
Version 0.16.8 sometimes "forgets" to ask for client certificate password.

