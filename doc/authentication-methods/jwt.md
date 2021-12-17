## Overview

This authentication method can verify [JSON Web Tokens](https://jwt.io) provided by the clients.
A wide range of signature algorithms is supported, including those using public key cryptography.

The module checks the signature and validity of the following parameters:

* `exp` - an expired token is rejected,
* `iat` - a token must be issued in the past,
* `nbf` - a token might not be valid *yet*.

It requires the `SASL PLAIN` mechanism listed in `sasl_mechanisms`.

## Configuration options

### `auth.jwt.secret`
* **Syntax:** TOML table with exactly one of the possible items listed below:
    * `file` - string, path to the file with the JWT secret,
    * `env`- string, environment variable name with the JWT secret,
    * `value` - string, the JWT secret value.
* **Default:** no default, this option is mandatory
* **Example:** `secret.env = "JWT_SECRET"`

This is the JWT secret used for the authentication. You can store it in a file, as an environment variable or specify it directly.

### `auth.jwt.algorithm`
* **Syntax:** string, one of: `"HS256"`, `"RS256"`, `"ES256"`, `"HS386"`, `"RS386"`, `"ES386"`, `"HS512"`, `"RS512"`, `"ES512"`
* **Default:** no default, this option is mandatory
* **Example:** `algorithm = "HS512"`

Name of the algorithm used to sign the JWT.

### `auth.jwt.username_key`
* **Syntax:** string
* **Default:** no default, this option is mandatory
* **Example:** `username_key = "user_name"`

Name of the JWT key that contains the user name to verify.

### Example

```toml
[auth.jwt]
  secret.value = "top-secret123"
  algorithm = "HS256"
  username_key = "user"
```
