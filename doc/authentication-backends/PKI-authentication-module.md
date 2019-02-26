## Overview

It is a simple auth backend, meant to be used with SASL EXTERNAL authentication mechanism.
It simply accepts all usernames as long as they are validated by SASL logic.

## WARNING

Some of its callbacks return hardcoded values, as it's impossible for this backend to properly acquire certain pieces of information.
These include:

| Function | Hardcoded value | Explanation |
| ---------- | ----------------- | ----------- |
| `does_user_exist` | `true` | PKI reponds with `true` to modules checking if user's interlocutor actually exists so e.g. messages to nonexistent users will always be stored by `mod_mam`. This is not necessarily a security threat but something to be aware of. |
| `dirty_get_registered_users`, `get_vh_registered_users`, `get_vh_registered_users_number` | `[]` | Any metrics or statistics (e.g. available via `mongooseimctl`) related to accounts list or numbers, won't display proper values, as this backend cannot possibly "know" how many users there are. |

## Configuration options

None.

