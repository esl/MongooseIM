## Deprecations

MongooseIM is configured to allow only TLS 1.2 or higher, due to known vulnerabilities in TLS 1.0 and 1.1.
It is still possible to enable earlier versions, however it is strongly discouraged.

## OTP TLS hardening

The protocol list for OTP TLS is set via the `protocol_version` environment variable.
It's an Erlang runtime variable, so it is not configured in the OS but rather in the`app.config` file.
It may be found in `etc/` folder inside MongooseIM release and in `[repository root]/rel/files/`.

In order to change the list, please find the following line:

```
{protocol_version, ['tlsv1.2', 'tlsv1.3']}
```

The remaining valid values are: `'tlsv1.1'`, `tlsv1`, `sslv3`.

See [EEF guidelines for protocol versions and ciphers](https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/ssl#selecting-protocol-versions-and-ciphers) for more information.
