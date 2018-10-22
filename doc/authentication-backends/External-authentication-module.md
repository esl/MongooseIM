## Overview

This backend delegates the authentication to an external script.

Requires the SASL PLAIN method.

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
* `removeuser3:<username>:<domain>:<password>` - Remove a user if the password is correct.
* `isuser:<username>:<domain>` - Check if a user exists.

## Configuration options

* **extauth_program**
     * **Description:** Path to the authentication script used by the `external` auth module.

