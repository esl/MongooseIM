## Overview

The purpose of this module is to connect with an external REST API and delegate the authentication operations to it whenever possible.
The component must implement the API described in one of the next sections for `ejabberd_auth_http` to work out of the box.

The module can be especially useful for users maintaining their own central user database which is shared with other services. It fits perfectly when the client application uses a custom authentication token and MongooseIM has to validate it externally.

## Configuration

### How to enable

For a full reference please check [Advanced-configuration#authentication](../Advanced-configuration.md#authentication).
The simplest way is to just replace the default `auth_method` option in `rel/files/mongooseim.cfg` with `{auth_method, http}`.

Enabling the module **is not enough!** 
Please follow instructions below.

### Configuration options

`ejabberd_auth_http` uses an outgoing http connection pool called `auth`.
The pool has to be defined in outgoing_pools section (see [Outgoing-connections#/http-connections](../advanced-configuration/outgoing-connections#http-connections-setup)).
The following options can be set in the `auth_opts` tuple in `rel/files/mongooseim.cfg`:

* `basic_auth` (default: `""`) - HTTP Basic Authentication in format `"username:password"`; auth service doesn't have to require authentication for HTTP auth to work

#### Example

```
{auth_opts, [
             {basic_auth, "mongooseim:DzviNQw3qyGJDrJDu+ClyA"},
            ]}.
```

## SCRAM support

`ejabberd_auth_http` can use the SCRAM method.
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

### Method `remove_user_validate`

* **Description:** Removes a user account only if the provided password is valid.
* **HTTP method:** POST
* **Type:** mandatory when `mod_register` is enabled
* **Return values:**
    * 200 or 201 or 204 - success
    * 404 - user does not exist
    * 403 - invalid user password or not allowed for other reason
    * 40X - will be treated as `bad request`

### Authentication service API recipes

Below you can find some examples of the auth service APIs and MongooseIM-side configuration along with use cases.

#### System using a common, custom auth token

An Auth token is provided as a password.

* **Service implements:** `check_password`, `user_exists`
* **MongooseIM config:** `password_format`: `plain`, `mod_register` disabled
* **Client side:** MUST NOT use `DIGEST-MD5` mechanism; use `PLAIN`

#### Central database of plaintext passwords

* **Service implements:** `check_password`, `get_password`, `user_exists`
* **MongooseIM config:** `password_format`: `plain`, `mod_register` disabled
* **Client side:** May use any available auth method

#### Central database able to process SCRAM

* **Service implements:** `get_password`, `user_exists`
* **MongooseIM config:** `password_format`: `scram`, `mod_register` disabled
* **Client side:** May use any available auth method

#### Godlike MongooseIM

* **Service implements:** all methods
* **MongooseIM config:** `password_format`: `scram` (recommended) or `plain`, `mod_register` enabled
* **Client side:** May use any available auth method
