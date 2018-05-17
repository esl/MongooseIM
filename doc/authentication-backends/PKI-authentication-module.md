## Overview

PKI authentication backend is compatible only with SASL mechanisms, that are based on client certificates.
Currently there is only one such mechanism: `SASL EXTERNAL`.

It extracts Common Name from a certificate and returns it as a username part in JID.

## Client certificate requirements

Common Name must be equal to username part of the client JID.

## WARNING

Some of its callbacks return hardcoded values, as it's impossible for this backend to properly acquire certain pieces of information.
These include:

* `function` - `hardcoded value` - Explanation.
* `does_user_exist` - `true` - This function is used e.g. by `mod_mam` to verify if the user's interlocutor actually exists, so messages to nonexistent users will be stored anyway. It is not necessarily a security threat but requires extra caution.
* `dirty_get_registered_users`, `get_vh_registered_users`, `get_vh_registered_users_number` - `[]` - Any metrics or statistics (e.g. available via `mongooseimctl`) related to accounts list or count, won't display proper values, as this backend cannot possibly "know" how many users there are.

## Configuration options

None.

