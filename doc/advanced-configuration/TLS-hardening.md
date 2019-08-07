## OTP TLS vs. Fast TLS

Before we explain the TLS hardening in MongooseIM, we need to describe the TLS libraries used in the project.
These are "OTP TLS" and "Fast TLS".

The former is provided by (as the name suggests) OTP as the `ssl` application.
Large part of the logic is implemented in Erlang but it calls OpenSSL API for some operations anyway.

The latter is a community-maintained driver, which is implemented as NIFs (native C code).
It uses OpenSSL API for all operations.

Most MongooseIM components use the TLS library provided by OTP.
However, some of them choose to integrate with `fast_tls` library instead.
The former one is used primarily by MIM dependencies, while the latter is used only by MIM modules.

None of them is strictly better than the other.
Below you may find a summary of the differences between them.

* `fast_tls` is faster.
* There are options that OTP TLS (a.k.a `just_tls` in the `ejabberd_c2s` configuration) supports exclusively:
    * Immediate connection drop when the client certificate is invalid
    * Certificate Revocation Lists
    * More flexible certificate verification options
* Allowed protocol versions may be configured:
    * Globally for OTP TLS via an environment variable
    * Per socket in Fast TLS via OpenSSL cipher string

## Deprecations

MongooseIM is configured to allow only TLS 1.2 or higher, due to known vulnerabilities in TLS 1.0 and 1.1.
It is still possible to enable earlier versions, however it is strongly discouraged.

## OTP TLS hardening

Protocol list for OTP TLS is set via the `protocol_version` environment variable.
It's an Erlang runtime variable, so it is not configured in the OS but rather in the`app.config` file.
It may be found in `etc/` folder inside MongooseIM release and in `[repository root]/rel/files/`.

In order to change the list, please find the following lines:

```
{protocol_version, ['tlsv1.2'
                 %, 'tlsv1.3' % supported in OTP >= 22
          ]}
```

By default only TLS 1.2 is enabled, as 1.3 is not supported by OTPs older than 22.0.
If you are using OTP 22.0 or newer, you may remove leading `%` before `'tlsv1.3'`.
The remaining valid values are: `'tlsv1.1'`, `tlsv1`, `sslv3`.

This setting affects the following MongooseIM components:

* Raw XMPP over TCP connections, if `ejabberd_c2s` listener is configured to use `just_tls`.
* All outgoing connections (databases, AMQP, SIP etc.)
* HTTP endpoints

## Fast TLS hardening

Fast TLS expects an OpenSSL cipher string as one of optional connection parameters.
This string is configured individually for every module that uses it.
By default, MongooseIM sets this option to `TLSv1.2:TLSv1.3` for each component.

The list below enumerates all components that use Fast TLS and describes how to change this string.

* `ejabberd_c2s` - main user session abstraction + XMPP over TCP listener
    * Please consult the respective section in [Listener modules](Listener-modules.md#c2s-ciphers).
* `ejabberd_s2s_in` - incoming S2S connections (XMPP Federation)
    * Please consult the respective section in [Listener modules](Listener-modules.md#s2s-ciphers).
* `ejabberd_s2s_out` - outgoing S2S connections (XMPP Federation)
    * Please check [the documentation](../Advanced-configuration.md#s2s-ciphers) for `s2s_ciphers` option.
* `mod_global_distrib` - Global Distribution module
    * Please add `{ciphers, "string"}` to `tls_opts` parameter of `mod_global_distrib`, as [described in the documentation](../modules/mod_global_distrib.md#tls_opts).

