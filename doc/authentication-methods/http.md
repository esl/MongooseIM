## Overview

The purpose of this method is to connect to an external REST API and delegate the authentication operations to it.
The component must implement the [API described below](#authentication-service-api).

This method can be especially useful when the user database is shared with other services. It fits perfectly when the client application uses a custom authentication token and MongooseIM has to validate it externally.

### Configuration options

The `auth` method uses an outgoing HTTP connection pool called `auth`, which has to be defined in the `outgoing_pools` section.

For additional configuration, the following options can be provided in the `auth` section:

### `auth.http.basic_auth`
* **Syntax:** string
* **Default:** not set
* **Example:** `basic_auth = "admin:secret"`

Optional HTTP Basic Authentication in format `"username:password"` - used to authenticate MongooseIM in the HTTP service.

### Example

Authentication:

```toml
[auth.http]
  basic_auth = "mongooseim:DzviNQw3qyGJDrJDu+ClyA"
```

Outgoing pools:

```toml
[outgoing_pools.http.auth]
  connection.host = "https://auth-service:8000"
```

## SCRAM support

The `http` method can use the `SASL SCRAM-*` mechanisms.
When SCRAM is enabled, the passwords sent to the auth service are serialised and the same serialised format is expected when fetching a password from the component.

It is transparent when MongooseIM is responsible for all DB operations such as password setting, account creation etc.

The service CAN perform the (de)serialization of SCRAM-encoded passwords.
You can find more details on the [SCRAM serialization](../developers-guide/SCRAM-serialization.md) page.

## Authentication service API

### URL format

All GET requests include the following URL-encoded query string: `?user=<username>&server=<domain>&pass=<password>`.

All POST requests have the following URL-encoded string in the request body: `user=<username>&server=<domain>&pass=<password>`.

If a certain method does not need a password, the value of `pass` is **undefined**, so it shouldn't be used.

### Return codes

For the best integration, the return code range should not exceed the list below:

* 500 - internal server error
* 409 - conflict
* 404 - not found
* 403 - not allowed
* 401 - not authorised
* 400 - other error, should be sent in response body
* 204 - success, no return data
* 201 - created
* 200 - success, return value in response body

Whenever the specification says "anything else", service should use one of the codes from the list above.

Some requests consider multiple return codes a "success".
It is up to the server-side developer to pick one of the codes.

### HTTP header `Content-Length`

**IMPORTANT:** The authentication server MUST include a `Content-Length` HTTP header in the response.
A body can be missing in the first data chunk read from a socket, leading to strange authentication errors.

### Method `register`

* **Description:** Creates a user account.
* **HTTP method:** POST
* **Type:** mandatory when `mod_register` is enabled
* **Return values:**
    * 201 - success
    * 409 - user already exists
    * anything else - will be treated as failure

### Method `check_password`

* **Description:** Must respond if the password is valid for the user.
* **HTTP method:** GET
* **Type:** mandatory when SCRAM is not used
* **Return values:**
    * 200, `true` or `false` in the body
    * anything else - will be treated as `false`

### Method `get_password`

* **Description:** Must return the user's password in plaintext or in the SCRAM serialised form.
* **HTTP method:** GET
* **Type:** mandatory when SCRAM or DIGEST SASL mechanism is used
* **Return values:**
    * 200, password in the body
    * anything else - `get_password` will fail

### Method `get_certs`

* **Description:** Must return all the valid certificates of a user in the [PEM format](https://en.wikipedia.org/wiki/Privacy-Enhanced_Mail).
* **HTTP method:** GET
* **Type:** mandatory when EXTERNAL SASL mechanism is used
* **Return values:**
    * 200, all the user's certificates listed one after another (as in a PEM file)
    * anything else - `get_certs` will fail

### Method `user_exists`

* **Description:** Must return the information whether the user exists in DB.
* **HTTP method:** GET
* **Type:** mandatory
* **Return values:**
    * 200, `true` or `false` in body
    * anything else - will be treated as `false`

### Method `set_password`

* **Description:** Must set user's password in the internal database to a provided value.
 The value should not be transformed (except for URL-decoding) before writing into the DB.
* **HTTP method:** POST
* **Type:** mandatory when `mod_register` is enabled
* **Return values:**
    * 200 or 201 or 204 - success
    * anything else - will be treated as `false`

### Method `remove_user`

* **Description:** Removes a user account.
* **HTTP method:** POST
* **Type:** mandatory when `mod_register` is enabled
* **Return values:**
    * 200 or 201 or 204 - success
    * 404 - user does not exist
    * 403 - not allowed for some reason
    * 40X - will be treated as `bad request`

### Authentication service API recipes

Below you can find some examples of the auth service APIs and MongooseIM-side configuration along with use cases.

#### System using a common, custom auth token

An Auth token is provided as a password.

* **Service implements:** `check_password`, `user_exists`
* **MongooseIM config:** [`password.format`](../configuration/auth.md#authpasswordformat): `plain`, `mod_register` disabled
* **Client side:** Must NOT use the `DIGEST-MD5` mechanism; use `PLAIN` instead

#### Central database of plaintext passwords

* **Service implements:** `check_password`, `get_password`, `user_exists`
* **MongooseIM config:** [`password.format`](../configuration/auth.md#authpasswordformat): `plain`, `mod_register` disabled
* **Client side:** May use any available SASL mechanism

#### Central database able to process SCRAM

* **Service implements:** `get_password`, `user_exists`
* **MongooseIM config:** [`password.format`](../configuration/auth.md#authpasswordformat): `scram`, `mod_register` disabled
* **Client side:** May use any available SASL mechanism

#### Godlike MongooseIM

* **Service implements:** all methods
* **MongooseIM config:** [`password.format`](../configuration/auth.md#authpasswordformat): `scram` (recommended) or `plain`, `mod_register` enabled
* **Client side:** May use any available SASL mechanism
