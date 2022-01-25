## Overview

This authentication method delegates the authentication to an external script.

It uses the `SASL PLAIN` mechanism.

## Script API specification

All "commands" sent from Erlang VM to the script are prefixed with a 2-byte unsigned integer (command length), MSB first.
The script is expected to return responses in the same format.

Currently only 2 response packets are supported:

* `0x0000` = `false` (for failure).
* `0x0001` = `true` (for success).

The following list describes packets that the script should support.

* `auth:<username>:<domain>:<password>` - Check password.
* `setpass:<username>:<domain>:<password>` - Set password.
* `tryregister:<username>:<domain>:<password>` - Register a user.
* `removeuser:<username>:<domain>` - Remove a user.
* `isuser:<username>:<domain>` - Check if a user exists.

## Configuration options

### `auth.external.program`
* **Syntax:** string
* **Default:** no default, this option is mandatory for the `external` authentication method
* **Example:** `program = "/usr/bin/auth-script.sh"`

Path to the external authentication program.

### `auth.external.instances`
* **Syntax:** positive integer
* **Default:** `1`
* **Example:** `instances = 2`

Specifies the number of workers serving external authentication requests.

### Example

```toml
[auth.external]
  program = "/home/user/authenticator"
  instances = 5
```
