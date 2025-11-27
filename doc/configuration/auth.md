The `auth` section is used to choose and configure the **method** which is used by MongooseIM to authenticate connecting users.
The following methods are supported:

* `internal` - stores the user accounts in an internal Mnesia database,
* [`rdbms`](../authentication-methods/rdbms.md) - stores the user accounts in a SQL database,
* [`external`](../authentication-methods/external.md) - uses an external program to authenticate the user,
* [`anonymous`](../authentication-methods/anonymous.md) - allows anonymous connections,
* [`ldap`](../authentication-methods/ldap.md) - checks the user credentials in LDAP,
* [`jwt`](../authentication-methods/jwt.md) - authenticates the users with JSON Web Tokens,
* [`http`](../authentication-methods/http.md) - uses an external HTTP service to authenticate the user,
* [`pki`](../authentication-methods/pki.md) - uses the certificate provided by the user to authenticate them,
* [`dummy`](../authentication-methods/dummy.md) - no authentication, only for development and testing.

To allow the users to connect, you need to choose the authentication method from the list above and enable it by adding a corresponding section. For example, the default configuration file has the `[auth.internal]` section, which enables the `internal` method, using the internal Mnesia database to store users and their passwords. However, for production systems other methods like `rdbms` are recommended, as using an external database offers easier maintenance, flexibility, scalability and configurability in a typical setup. Some methods have more complex setup procedures and have their own specific options - the method names above are links to their descriptions. There are some general authentication options as well, which are described below.

!!! Warning
    Make sure that the compatible SASL mechanisms are enabled, see [capabilities](#authentication-method-capabilities).

## General Options

The options listed here affect more than one configuration method.

### `auth.methods`
* **Syntax:** array of strings. Allowed values: `"internal"`, `"rdbms"`, `"external"`, `"anonymous"`, `"ldap"`, `"jwt"`, `"http"`, `"pki"`, `"dummy"`
* **Default:** not set
* **Example:** `methods = ["internal", "anonymous"]`

It is possible to enable more than one method - they are queried one by one in the alphabetical order until one of them succeeds or there are no more methods. You can change the default order by using this option. Make sure that all methods from the list have their corresponding sections included in the `auth` section, e.g.

```toml
[auth]
  methods = ["internal", "dummy"]

  [auth.internal]

  [auth.dummy]
    variance = 1000
```

### `auth.sasl_mechanisms`
* **Syntax:** array of strings. Allowed values: `"scram_sha512_plus"`, `"scram_sha512"`, `"scram_sha384_plus"`, `"scram_sha384"`, `"scram_sha256_plus"`, `"scram_sha256"`, `"scram_sha224_plus"`, `"scram_sha224"`, `"scram_sha1_plus"`, `"scram_sha1"`, `"plain"`, `"anonymous"`, `"oauth"`, `"external"`, `"digest"`
* **Default:** `["scram_sha512_plus", "scram_sha512", "scram_sha384_plus", "scram_sha384", "scram_sha256_plus", "scram_sha256", "scram_sha224_plus", "scram_sha224", "scram_sha1_plus", "scram_sha1", "plain", "anonymous", "oauth"]`
* **Example:** `sasl_mechanisms = ["external", "plain"]`

Specifies the list of allowed SASL mechanisms, which are announced during stream negotiation and eventually enforced (users can't pick a mechanism not listed here).

!!! Notes

    * This list is still filtered by [capabilities](#authentication-method-capabilities). For example, if you use the `internal` method, only the `PLAIN`, `DIGEST-MD5` and `SCRAM-SHA-*` mechanisms from the list will be supported. If there are no compatible mechanisms on the list, the users will not be able to authenticate.
    * Configuring the `sasl_mechanisms` replaces the default list entirely.
    * The order in which the mechanisms are listed in the config will be taken as the order in which they are advertised.
    * All `SCRAM-SHA-*` mechanisms (specified as `scram_sha*`) have their counterparts which support channel binding and are advertised as separate authentication mechanisms suffixed by `-PLUS` (specified as `scram_sha*_plus`).
    * The `DIGEST-MD5` mechanism (specified as `digest`) is deprecated and will be removed in the next release.

#### Authentication method capabilities

The table below shows the supported SASL mechanisms (columns) for each authentication method (row).

|                   | plain | digest | scram_sha* | anonymous | external |
|-------------------|:-----:|:------:|:----------:|:---------:|:--------:|
| internal          |   x   |   x    |     x      |           |          |
| rdbms             |   x   |   x    |     x      |           |          |
| external          |   x   |        |            |           |          |
| anonymous         |   x   |   x    |     x      |     x     |          |
| ldap              |   x   |        |            |           |    x     |
| jwt               |   x   |        |            |           |          |
| http              |   x   |   x    |     x      |           |          |
| pki               |       |        |            |           |    x     |
| dummy             |   x   |        |            |           |          |

### `auth.sasl_external`
* **Syntax:** list of strings, allowed values: `"standard"`, `"common_name"`, `"auth_id"`
* **Default:** `["standard"]`
* **Example:** `sasl_external = ["standard", "common_name"]`

There are three possible ways of using the `SASL EXTERNAL` mechanism:

* `standard` - do not accept a certificate with no `xmpp_addrs` field (default),
* `common_name` - use the `common_name` field if it is provided in the certificate,
* `auth_id` - accept a certificate without `xmpp_addrs` and use the user identity from the authentication request.

This option allows you to list the enabled ones in the order of preference (they are tried until one succeeds or the list is exhausted).

!!! Note
    There is another possibility: a custom verification module that you can implement using [cyrsasl_external_verification](https://github.com/esl/MongooseIM/blob/master/src/sasl/cyrsasl_external_verification.erl) as an example.
    After including the module in your MongooseIM release, you can put its name in the list of values of the `sasl_external` option.

### `auth.max_users_per_domain`
* **Syntax:** positive integer or string `"infinity"`, representing maximum amount of users that can be registered in a domain
* **Default:** `"infinity"`
* **Example:** `max_users_per_domain = 10000`

Limits the number of users that can be registered for each domain. If the option is configured to the value `"infinity"`, no limit is present.

!!! Warning
    The limit only works for the following authentication methods: `internal`, `rdbms` and `ldap`.

## SASL External common name options

These options are only effective for the `SASL EXTERNAL` mechanism using `common_name` (see [`sasl_external`](#authsasl_external)).

#### `auth.sasl_external_common_name.prefix`
* **Syntax:** string, a valid [JID localpart](https://www.rfc-editor.org/rfc/rfc6122.html#appendix-A)
* **Default:** empty string
* **Example:** `sasl_external_common_name.prefix = "user_"`

An optional prefix prepended to the value of `common_name` in order to convert it to a user name.

#### `auth.sasl_external_common_name.suffix`
* **Syntax:** string, a valid [JID localpart](https://www.rfc-editor.org/rfc/rfc6122.html#appendix-A)
* **Default:** empty string
* **Example:** `sasl_external_common_name.prefix = ".1"`

An optional suffix appended to the value of `common_name` in order to convert it to a user name.

## Password-related options

These options are common to the `http`, `rdbms` and `internal` methods.

### `auth.password.format`
* **Syntax:** string, one of: `"plain"`, `"scram"`
* **Default:** `"scram"`
* **Example:** `password.format = "plain"`

Decide whether user passwords will be kept plain or hashed in the database.
Currently, popular XMPP clients support the SCRAM method and it is strongly recommended to use the hashed version.
The older XMPP clients can still use the `PLAIN` mechanism even if the format is set to `scram`.

!!! Note
    The `DIGEST-MD5` mechanism is not available with the `scram` password format.

### SCRAM options

For these options to take effect, `password.format` should be set to `scram`.

!!! Warning
    If you are using MongooseIM 4.1.0 to 6.3.1 with SCRAM authentication and OpenSSL >=3.4.1, a known issue affects hash calculation for algorithms stronger than SHA-1.
    See [SCRAM hashing issue](../developers-guide/SCRAM-serialization.md#scram-hash-calculation-issue-in-mongooseim-410631) for details on how to resolve this.

### `auth.password.hash`
* **Syntax:** list of strings, allowed values: `"sha"`, `"sha224"`, `"sha256"`, `"sha384"`, `"sha512"`
* **Default:** not set - all hash functions supported
* **Example:** `password.hash = ["sha384", "sha512"]`

MongooseIM supports SHA-1, SHA-224, SHA-256, SHA-384 and SHA-512 for SCRAM hashing.
You can use this option to limit the supported hash functions by listing them explicitly.
The value `"sha"` stands for the SHA-1 algorithm.

!!! Warning
    This option limits the supported `SCRAM-SHA-*` SASL mechanisms to the ones compatible with the specified hash functions.

### `auth.password.scram_iterations`
* **Syntax:** positive integer
* **Default:** 10000, as recommended in this [XEP](https://xmpp.org/extensions/xep-0438.html#pbkdf2) and this [NIST Guidelines](https://pages.nist.gov/800-63-3/sp800-63b.html#sec5)
* **Example:** `password.scram_iterations = 20_000`

Hash function round count.
This is a tradeoff between latency and security.
The higher the value, the more difficult breaking the hashes is: increasing the count increases the work it requires to compute a full derivation, which effectively slows down brute-force attacks.
But it adds load on both client and server, so this parameter should be tuned as high as the business-rules allow.
Note that increasing the security of a password has a higher impact over the security of the algorithm, without impacting its load.
See more information in this [NIST guide, Appendix A.2.2](https://csrc.nist.gov/publications/detail/sp/800-132/final)

## Examples

Internal authentication method without any general options - you can skip the `auth` section in this case:

```toml
[auth.internal]
```

Internal authentication method with some general options:

```toml
[auth]
  password.hash = ["sha512"]
  password.scram_iterations = 20000

  [auth.internal]
```

For more specific examples, see the links below.

## Method-specific options

See the links below for options related to the particular methods:

* [RDBMS method options](../authentication-methods/rdbms.md#configuration-options)
* [Anonymous method options](../authentication-methods/anonymous.md#configuration-options)
* [External method options](../authentication-methods/external.md#configuration-options)
* [LDAP method options](../authentication-methods/ldap.md#configuration-options)
* [JWT method options](../authentication-methods/jwt.md#configuration-options)
* [HTTP method options](../authentication-methods/http.md#configuration-options)
