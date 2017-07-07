## Overview

JWT authentication backend can verify JSON Web Tokens provided by the clients.
A wide range of signature algorithms is supported, including those using public key cryptography.
Not only the signature is checked, but also the validity of `exp` (expired token is rejected), `iat` (token must be issued in the past) and `nbf` (token might be invalid *yet*) parameters.

## Configuration options

* **jwt_secret_source**
    * **Description:** A path to a file or environment variable, which contents will be used as a JWT secret.
    * **Warning:** A direct path to a file is read only once during startup, a path in the environment variable is read on every auth request.
    * **Value:** string, e.g. `/etc/secrets/jwt` or `{env, "env-variable-name"}`
    * **Default:** none, either `jwt_secret_source` or `jwt_secret` must be set

* **jwt_secret**
    * **Description:** A binary with a JWT secret. This options is ignored and overwritten, if `jwt_secret_source` is defined.
    * **Value:** binary
    * **Default:** none (either `jwt_secret_source` or `jwt_secret` must be set)

* **jwt_algorithm**
    * **Description:** A name of the algorithm used to sign JWT.
    * **Valid values:** `"HS256", "RS256", "ES256", "HS386", "RS386", "ES386", "HS512", "RS512", "ES512"`
    * **Default:** none, it's a mandatory option

* **jwt_username_key**
    * **Description:** A JWT key that contains username to verify.
    * **Value:** atom
    * **Default:** none, it's a mandatory option

