# HTTP authentication module

## Overview

The purpose of this module is to connect with an external REST API and delegate the authentication operations to it whenever possible. The component must implement the API described in one of the next sections for `ejabberd_auth_http` to work out of the box.

The module can be especially useful for users maintaining their own, central user database, which is shared with other services. It fits perfectly when client application uses custom authentication token and MongooseIM has to validate it externally.

## Configuration

### How to enable

For full reference please check [[Advanced-configuration#authentication]]. The simplest way is to just replace default `auth_method` option in `rel/files/ejabberd.cfg` with `{auth_method, http}`.

Enabling the module **is not enough!** Please follow instructions below.

### Configuration options

`ejabberd_auth_http` requires some parameters to function properly. The following options can be set in `auth_opts` tuple in `rel/files/ejabberd.cfg`:

* `host` (mandatory, `string`) - consists of protocol, hostname (or IP) and port (optional). Examples:
  * `{host, "http://localhost:12000"}`
  * `{host, "https://10.20.30.40"}`
* `connection_pool_size` (optional, `integer`, default: 10) - the number of connections open to auth service
* `connection_opts` (optional, default: `[]`) - extra options for hackers: http://erlang.org/doc/man/gen_tcp.html#type-connect_option
* `basic_auth` (optional, default: `""`) - HTTP Basic Authentication in format `"username:password"`; auth service doesn't have to require authentication for HTTP auth to work
* `path_prefix` (optional, default: `"/"`) - a path prefix to be inserted between `host` and method name; must be terminated with `/`

## SCRAM support

`ejabberd_auth_http` can use the SCRAM method. When SCRAM is enabled, the passwords sent to the auth service are serialised and the same serialised format is expected when fetching a password from the component.

It is transparent when MongooseIM is responsible for all DB operations such as password setting, account creation etc.

The service CAN perform the (de)serialisation of SCRAM-encoded passwords, using a format that will be described in near future in separate document.

## Authentication service API

All GET requests include the following URL-encoded query string: `?user=<username>&server=<domain>&pass=<password>`.

All POST requests have the following URL-encoded string in the request body: `user=<username>&server=<domain>&pass=<password>`.

If certain method does not need a password, the value of `pass` is **undefined**, so it shouldn't be used.

If specific value is expected in response body, it MUST NOT be put in quotes!

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

Some requests consider multiple return codes a "success". It is up to the server-side developer to pick one of the codes.

### `check_password`

* **Method:** GET
* **Type:** mandatory when SCRAM is not used
* **Return values:**
  * 200, "true" or "false" in body
  * anything else - will be treated as "false"
* **Description:** Must respond if the password is valid for user.

### `get_password`

* **Method:** GET
* **Type:** mandatory when SCRAM or DIGEST SASL mechanism is used
* **Return values:**
  * 200, password in the body
  * anything else - `get_password` will fail
* **Description:** Must return the user's password in plaintext or in the SCRAM serialised form.

### `user_exists`

* **Method:** GET
* **Type:** mandatory
* **Return values:**
  * 200, "true" or "false" in body
  * anything else - will be treated as "false"
* **Description:** Must return the information whether the user exists in DB.

### `set_password`

* **Method:** POST
* **Type:** mandatory when `mod_register` is enabled
* **Return values:**
  * 200 or 201 or 204 - success
  * anything else - will be treated as "false"
* **Description:** Must set user's password in the internal database to a provided value. The value should not be transformed (except for URL-decoding) before writing into DB.

### `remove_user`

* **Method:** POST
* **Type:** mandatory when `mod_register` is enabled
* **Return values:**
  * 200 or 201 or 204 - success
  * 404 - user does not exist
  * 403 - not allowed for some reason
  * 40X - will be treated as "bad request"
* **Description:** Removes a user account.

### `remove_user_validate`

* **Method:** POST
* **Type:** mandatory when `mod_register` is enabled
* **Return values:**
  * 200 or 201 or 204 - success
  * 404 - user does not exist
  * 403 - invalid user password or not allowed for other reason
  * 40X - will be treated as "bad request"
* **Description:** Removes a user account only if provided password is valid.

### `register`

* **Method:** POST
* **Type:** mandatory when `mod_register` is enabled
* **Return values:**
  * 201 - success
  * 409 - user already exists
  * anything else - will be treated as failure
* **Description:** Creates a user account.

## Authentication service API recipes

Below are some examples of the auth service APIs and MongooseIM-side configuration along with use cases.

### System using common, custom auth token

An Auth token is provided as a password.

* **Service implements:** `check_password`, `user_exists`
* **MongooseIM config:** `password format`: `plain`, `mod_register` disabled
* **Client side:** MUST NOT use `DIGEST-MD5` mechanism; use `PLAIN`

### Central database of plaintext passwords

* **Service implements:** `check_password`, `get_password`, `user_exists`
* **MongooseIM config:** `password format`: `plain`, `mod_register` disabled
* **Client side:** May use any available auth method

### Central database able to process SCRAM

* **Service implements:** `get_password`, `user_exists`
* **MongooseIM config:** `password format`: `scram`, `mod_register` disabled
* **Client side:** May use any available auth method

### Godlike MongooseIM

* **Service implements:** all methods
* **MongooseIM config:** `password format`: `scram` (recommended) or `plain`, `mod_register` enabled
* **Client side:** May use any available auth method