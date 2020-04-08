## Overview

This document describes the SCRAM serialization format used by MongooseIM.
Developers can use this information to create advanced endpoints for `ejabberd_auth_http` or enable other software to read (i.e. share) the user authentication data.

## Format description

`==MULTI_SCRAM==,<salt>,<iteration count>,===SHA1===<stored key>|<server key>,==SHA256==<stored key>|<server key>`

* `<salt>` - Base64-encoded Salt
* `<iteration count>` - Iteration Count formatted as a human-readable integer
* `<stored key>` - Base64-encoded Stored Key
* `<server key>` - Base64-encoded Server Key

The SCRAM format can vary depending on the SHA algorithms that are used for SCRAM. Salt and iteration count is common for different SHA types. Stored Key and Server Key are specific to a given SHA and are following a SHA prefix that is indicating which SHA they belong to.

In order to learn more about the meaning of the Stored Key, Server Key, Salt and Iteration Count, please check [the SCRAM specification](https://tools.ietf.org/html/rfc5802).

### Example

* *Password:* `padthai`
* *Erlang map:*
```
#{iteration_count => 4096,
   salt => <<"glF/hUXiCWD6TaJLpgVaaw==">>,
   sha =>
      #{server_key => <<"ztvci4eBktO3NLviLhwN3ktF15Q=">>,
        stored_key => <<"Hxfegcj54A6p/ZzWkw7BjJsLZpM=">>},
   sha256 =>
      #{server_key => <<"0Tm46vWQyO/XWpAtcDCUH++7ImWVDe0VDx274zMzMXQ=">>,
        stored_key => <<"S4XiSkYI81m688nMuCgFGUBIGbW6oG1a9rlTKktzS/A=">>}}
```
* *Serialized password:* `==MULTI_SCRAM==,glF/hUXiCWD6TaJLpgVaaw==,4096,===SHA1===Hxfegcj54A6p/ZzWkw7BjJsLZpM=|ztvci4eBktO3NLviLhwN3ktF15Q=,==SHA256==S4XiSkYI81m688nMuCgFGUBIGbW6oG1a9rlTKktzS/A=|0Tm46vWQyO/XWpAtcDCUH++7ImWVDe0VDx274zMzMXQ=`

## Legacy format description

MongooseIM installations prior to 3.6.3 were supporting only SHA-1 as a hashing algorithm for SCRAM. The SCRAM format that was used can be seen below.

`==SCRAM==,<stored key>,<server key>,<salt>,<iteration count>`

* `<stored key>` - Base64-encoded Stored Key
* `<server key>` - Base64-encoded Server Key
* `<salt>` - Base64-encoded Salt
* `<iteration count>` - Iteration Count formatted as a human-readable integer

In order to learn more about the meaning of the Stored Key, Server Key, Salt and Iteration Count, please check [the SCRAM specification](https://tools.ietf.org/html/rfc5802).

### Example

* *Password:* `misio`
* *Erlang record:* `#scram{ storedkey = <<"tmi5IE+9pceRV/jkPLFHEaVY33c=">>, serverkey = <<"MiWNa8T3dniVDwmh77ufJ41fpAQ=">>, salt = <<"inKXODlSY5y5SCsLxibi0w==">>, iterationcount = 4096 }`
* *Serialized password:* `==SCRAM==,tmi5IE+9pceRV/jkPLFHEaVY33c=,MiWNa8T3dniVDwmh77ufJ41fpAQ=,inKXODlSY5y5SCsLxibi0w==,4096`
