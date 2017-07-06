## Overview

This document describes the SCRAM serialization format used by MongooseIM.
Developers can use this information to create advanced endpoints for `ejabberd_auth_http` or enable other software to read (i.e. share) the user authentication data.

## Format description

`==SCRAM==,<stored key>,<server key>,<salt>,<iteration count>`

* `<stored key>` - Base64-encoded Stored Key
* `<server key>` - Base64-encoded Server Key
* `<salt>` - Base64-encoded Salt
* `<iteration count>` - Iteration Count formatted as a human-readable integer

In order to learn more about the meaning of Stored Key, Server Key, Salt and Iteration Count, please check [the SCRAM specification](https://tools.ietf.org/html/rfc5802).

### Example

* *Password:* `misio`
* *Erlang record:* `#scram{ storedkey = <<"tmi5IE+9pceRV/jkPLFHEaVY33c=">>, serverkey = <<"MiWNa8T3dniVDwmh77ufJ41fpAQ=">>, salt = <<"inKXODlSY5y5SCsLxibi0w==">>, iterationcount = 4096 }`
* *Serialized password:* `==SCRAM==,tmi5IE+9pceRV/jkPLFHEaVY33c=,MiWNa8T3dniVDwmh77ufJ41fpAQ=,inKXODlSY5y5SCsLxibi0w==,4096`

