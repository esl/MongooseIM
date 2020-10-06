The `auth` section is used to choose and configure the **methods** which are used by MongooseIM to authenticate connecting users.
The following methods are supported:

* `internal` - stores the user accounts in an internal Mnesia database,
* [`rdbms`](../authentication-methods/rdbms.md) - stores the user accounts in a SQL database,
* [`external`](../authentication-methods/external.md) - uses an external program to authenticate the user,
* [`anonymous`](../authentication-methods/anonymous.md) - allows anonymous connections,
* [`ldap`](../authentication-methods/ldap.md) - checks the user credentials in LDAP,
* [`jwt`](../authentication-methods/jwt.md) - authenticates the users with JSON Web Tokens,
* [`riak`](../authentication-methods/riak.md) - stores the user accounts in a Riak database,
* [`http`](../authentication-methods/http.md) - uses an external HTTP service to authenticate the user,
* [`pki`](../authentication-methods/pki.md) - uses the certificate provided by the user to authenticate them,
* [`dummy`](../authentication-methods/dummy.md) - no authentication, only for development and testing.

To enable user connections, you need to set up at least one of the methods listed above (see `auth.methods` below).
Some methods have more complex setup procedures - the method names above are links to their descriptions,
which list their specific configuration options. The general options are described below.

# General Options

The options listed here are used to configure the authentication methods.

### `auth.methods`
* **Syntax:** array of strings. Allowed values: `"internal"`, `"rdbms"`, `"external"`, `"anonymous"`, `"ldap"`, `"jwt"`, `"riak"`, `"http"`, `"pki"`, `"dummy"`
* **Default:** not set
* **Example:** `methods = ["internal", "anonymous"]`

Specifies the methods used to authenticate connecting users. Methods from the list are queried one after another until one of them replies positively. By default there are no methods, so nobody can authenticate.

**Warning:** Make sure that the compatible SASL mechanisms are enabled, see [capabilities](#authentication-method-capabilities).

### `auth.sasl_mechanisms`
* **Syntax:** array of strings. Allowed values: `"scram_sha512_plus"`, `"scram_sha512"`, `"scram_sha384_plus"`, `"scram_sha384"`, `"scram_sha256_plus"`, `"scram_sha256"`, `"scram_sha224_plus"`, `"scram_sha224"`, `"scram_sha1_plus"`, `"scram_sha1"`, `"plain"`, `"anonymous"`, `"oauth"`, `"external"`, `"digest"`
* **Default:** `["scram_sha512_plus", "scram_sha512", "scram_sha384_plus", "scram_sha384", "scram_sha256_plus", "scram_sha256", "scram_sha224_plus", "scram_sha224", "scram_sha1_plus", "scram_sha1", "plain", "anonymous", "oauth"]`
* **Example:** `sasl_mechanisms = ["external", "plain"]`

Specifies the list of allowed SASL mechanisms, which are announced during stream negotiation and eventually enforced (users can't pick a mechanism not listed here).

**Notes:**

* This list is still filtered by [capabilities](#authentication-method-capabilities).
* Configuring the `sasl_mechanisms` replaces the default list entirely.
* The order in which the mechanisms are listed in the config will be taken as the order in which they are advertised.
* All `SCRAM-SHA-*` mechanisms (specified as `scram_sha*`) have their counterparts which support channel binding and are advertised as separate authentication mechanisms suffixed by `-PLUS` (specified as `scram_sha*_plus`).
* The `DIGEST-MD5` mechanism (specified as `digest`) is deprecated and will be removed in the next release.

#### Authentication method capabilities

The table below shows the supported SASL mechanisms (columns) for each authentication method (row).

|           | plain | digest | scram_sha* | anonymous | external |
|-----------|:-----:|:------:|:----------:|:---------:|:--------:|
| internal  |   x   |   x    |     x      |           |          |
| rdbms     |   x   |   x    |     x      |           |          |
| external  |   x   |        |            |           |          |
| anonymous |   x   |   x    |     x      |     x     |          |
| ldap      |   x   |        |            |           |    x     |
| jwt       |   x   |        |            |           |          |
| riak      |   x   |   x    |     x      |           |          |
| http      |   x   |   x    |     x      |           |          |
| pki       |       |        |            |           |    x     |

### `auth.sasl_external`
* **Syntax:** list of strings, allowed values: `"standard"`, `"common_name"`, `"auth_id"`
* **Default:** `["standard"]`
* **Example:** `sasl_external = ["standard", "common_name"]`

There are three possible ways of using the `SASL EXTERNAL` mechanism:

* `standard` - do not accept a certificate with no `xmpp_addrs` field (default),
* `common_name` - use the `common_name` field if it is provided in the certificate,
* `auth_id` - accept a certificate without `xmpp_addrs` and use the user identity from the authentication request.

This option allows you to list the enabled ones in the order of preference (they are tried until one succeeds or the list is exhausted).

## Password-related options

These options are common to the `http`, `rdbms`, `internal` and `riak` methods.

### `auth.password.format`
* **Syntax:** string, one of: `"plain"`, `"scram"`
* **Default:** `"scram"`
* **Example:** `password.format = "plain"`

Decide whether user passwords will be kept plain or hashed in the database.
Currently, popular XMPP clients support the SCRAM method and it is strongly recommended to use the hashed version.
The older XMPP clients can still use the `PLAIN` mechanism.

**Note:** The `DIGEST-MD5` mechanism is not available with the `scram` password format.

### `auth.password.hash`
* **Syntax:** list of strings, allowed values: `"sha"`, `"sha224"`, `"sha256"`, `"sha384"`, `"sha512"`
* **Default:** not set - all hash functions supported
* **Example:** `password.hash = ["sha384", "sha512"]`

MongooseIM supports SHA-1, SHA-224, SHA-256, SHA-384 and SHA-512 for SCRAM hashing.
You can use this option to limit the supported hash functions by listing them explicitly.
The value `"sha"` stands for the SHA-1 algorithm.

### `auth.scram_iterations`
* **Syntax:** positive integer
* **Default:** 10000,  as recommended in this [XEP](https://xmpp.org/extensions/xep-0438.html#pbkdf2) and this [NIST Guidelines](https://pages.nist.gov/800-63-3/sp800-63b.html#sec5)
* **Example:** `scram_iterations = 20_000`

Hash function round count.
This is a tradeoff between latency and security.
The higher the value, the more difficult breaking the hashes is: increasing the count increases the work it requires to compute a full derivation, which effectively slows down brute-force attacks.
But it adds load on both client and server, so this parameter should be tuned as high as the business-rules allow.
Note that increasing the security of a password has a higher impact over the security of the algorithm, without impacting its load.
See more information in this [NIST guide, Appendix A.2.2](https://csrc.nist.gov/publications/detail/sp/800-132/final)

## Example

This minimal authentication setup uses the internal Mnesia database to store users and their passwords:

```toml
[auth]
  methods = ["internal"]
```

According to the [capabilities](#authentication-method-capabilities) of the `internal` method, the `PLAIN`, `DIGEST-MD5` and `SCRAM-SHA-*` mechanisms will be supported.

However, for production systems other methods like `rdbms` are recommended, as using an external database offers easier maintenance, flexibility, scalability and configurability in a typical setup.

# Method-specific options

See the links below for options related to the particular methods:

* [Anonymous method options](../authentication-methods/anonymous.md#configuration-options)
* [External method options](../authentication-methods/external.md#configuration-options)
* [LDAP method options](../authentication-methods/ldap.md#configuration-options)
* [JWT method options](../authentication-methods/jwt.md#configuration-options)
* [Riak method options](../authentication-methods/riak.md#configuration-options)
* [HTTP method options](../authentication-methods/http.md#configuration-options)
