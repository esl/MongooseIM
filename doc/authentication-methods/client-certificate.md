## Overview

Clients connected to MongooseIM may authenticate with their TLS certificates.
This method is a combination of the `SASL EXTERNAL` mechanism and a compatible certificate-aware backend.

## Server-side prerequisites

### Properly configure `ejabberd_c2s` listener

A server must request the certificate from a client, so you'll need to enable `verify_peer` option and provide a path to CA chain that may be used for client's certificate check (`cafile` option).

Please check [Listener modules](../advanced-configuration/Listener-modules.md#client-to-server-c2s-ejabberd_c2s) page for more information or simply follow the examples at the end of this section.

### Properly configure `ejabberd_cowboy` listener

SASL EXTERNAL authentication is also possible for WebSocketSecure and BOSH connections over HTTPS.
Similarly as in `ejabberd_c2s` case, the server must request the certificate from the client.
In this case it's enabled by adding the following options to `ssl` option of `ejabberd_cowboy` :

* `{verify, verify_peer}` - this is to tell Erlang's SSL to request the cert from the client
* `{cacertfile, "/path/to/ca.pem"}` - this is to tell Erlang's SSL where  the CA cert file is in order to check if the cert is correctly signed

Please check [Listener modules](../advanced-configuration/Listener-modules.md#http-based-services-bosh-websocket-rest-ejabberd_cowboy) for more details regarding `ejabberd_cowboy` configuration.

### Enable `SASL EXTERNAL` method

A `SASL EXTERNAL` authentication method is disabled by default.
In order to enable it, please add [`sasl_mechanisms` option](../Advanced-configuration.md#authentication) to MongooseIM config file.
Its value must include a `cyrsasl_external` item.
Obviously the list may be longer, if the system should support both certificate and password based authentication.

### Enable compatible authentication backend

Please modify [`auth_opts` option](../Advanced-configuration.md#authentication) in MongooseIM's config file to include proper item.
For now, only [`pki` backend](../authentication-backends/PKI-authentication-module.md) supports `SASL EXTERNAL`.

### WARNING!

Some authentication backends may enforce `plain` password storage format, which automatically disables `SASL EXTERNAL`.
Below you may find a list of backends that are safe to use with `cyrsasl_external` mechanism.

* `pki`
* `anonymous`
* `http` **without** `{is_external, true}` option
* `internal`
* `rdbms`
* `riak`

### Self-signed certificates

By default MongooseIM doesn't accept self-signed certs for the SASL-EXTERNAL authentication.
They may be handy in development so we made it possible to tell MongooseIM to accept them.

#### Self-signed certificates for regular TCP/TLS connections

In order to tell MongooseIM to accept self-signed certs, `ssl_options` list needs to be added to `ejabberd_c2s` listener config like below:

```Erlang
{ssl_options, [{verify_fun, {selfsigned_peer, true}}]}
```

#### Self-signed certificates for WS or BOSH

In order to accept self-signed certs for WS or BOSH connections, `ssl` option list of `ejabberd_cowboy` must contain the following pair:

```Erlang
{verify_mode, selfsigned_peer}
```


### Examples

Certificate authentication only.

```Erlang
{listen, [
           (...)
           {5222, ejabberd_c2s, [
                                  (...)
                                  {cafile, "/path/to/ca.pem"},
                                  verify_peer,
                                  (...)
                                ]},
           (...)

           {5285, ejabberd_cowboy, [

                                    {ssl, [(...),
                                           {verify, verify_peer},
                                           {cacertfile, "/path/to/ca.pem"}
                                           ]},
                                    {modules, [{mod_websockets, []},
                                               {mod_bosh, []}]},
                                    (...)

           ]},

           (...)
         ]}.

{auth_method, [pki]}.

{sasl_mechanisms, [cyrsasl_external]}.
```

Authentication with a client certificate (validated with provided CA chain) or password (validated with data stored in RDBMS).

```
{listen, [
           (...)
           {5222, ejabberd_c2s, [
                                  (...)
                                  {cafile, "/path/to/ca.pem"},
                                  verify_peer,
                                  (...)
                                ]},
           (...)
         ]}.


{auth_method, [rdbms, pki]}.

{sasl_mechanisms, [cyrsasl_scram, cyrsasl_external]}.
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
7. Click "Close" and set status to "Available". Tell Gajim to ingnore the unverified server certificate (by default it's self-signed).

If Gajim fails to connect, try to restart it.
Version 0.16.8 sometimes "forgets" to ask for the client certificate password.

