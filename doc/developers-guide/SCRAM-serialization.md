## Overview

This document describes the SCRAM serialization format used by MongooseIM.
Developers can use this information to create advanced endpoints for `ejabberd_auth_http` or enable other software to read (i.e. share) the user authentication data.

## Format description

`==MULTI_SCRAM==,<iteration count>,===SHA1===<salt>|<stored key>|<server key>,==SHA224==<salt>|<stored key>|<server key>,==SHA256==<salt>|<stored key>|<server key>,==SHA384==<salt>|<stored key>|<server key>,==SHA512=<salt>|<stored key>|<server key>`

* `<iteration count>` - Iteration Count formatted as a human-readable integer
* `<salt>` - Base64-encoded Salt
* `<stored key>` - Base64-encoded Stored Key
* `<server key>` - Base64-encoded Server Key

The SCRAM format can vary depending on the SHA algorithms that are used for SCRAM.
Salt and iteration count is common for different SHA types.
Stored Key and Server Key are specific to a given SHA and are following a SHA prefix that is indicating which SHA they belong to.

In order to learn more about the meaning of the Stored Key, Server Key, Salt and Iteration Count, please check [the SCRAM specification][the SCRAM specification].

### Example

* *Password:* `padthai`
* *Erlang map:*
```erlang
#{iteration_count => 4096,
  sha =>
      #{salt => <<"QClQsw/sfPEnwj4AEp6E1w==">>,
        server_key => <<"EJvxXWM42tO7BgW21lNZyBc1dD0=">>,
        stored_key => <<"ys1104hRhqMoRputBY5sLHKXoSw=">>},
  sha224 =>
      #{salt => <<"dk0ImXFVPoUfqD5FveV7YA==">>,
        server_key => <<"EvE2EkZcUb3k4CooeOcVFy95P32t+NDX0xbQUA==">>,
        stored_key =>
            <<"G0ibQ/YYuCtoun4I+1IF2zJ7Q8x2T23ETnq5Gg==">>},
  sha256 =>
      #{salt => <<"M7BYKSo04XbzBr4C7b056g==">>,
        server_key =>
            <<"XhtGFf6NDWsnVSCO4xkzPD3qc046fPL0pATZi7RmaWo=">>,
        stored_key =>
            <<"A779MC05nSGQln5no0hKTGHFSaQ7oguKBZgORW3s+es=">>},
  sha384 =>
      #{salt => <<"Ryu0fA29gbwgqFOBk5Mczw==">>,
        server_key =>
            <<"kR+LMI/E0QBG3oF405/MTAT6NAlCOfPrFOaWH3WBVGM0Viu9Brk6kGwVwXjSP8v0">>,
        stored_key =>
            <<"k3QwC0Lb1y1/V/31byC5KML5t3mH4JTPjFyeAz7lV2l4SPfzi3JHvLEdoNB5K/VY">>},
  sha512 =>
      #{salt => <<"SLNuVNcWiNBmnYZNIdj+zg==">>,
        server_key =>
            <<"jUUDbuQ9ae4UnAWS6RV6W4yifX3La3ESjfZjGol+TBROIb/ihR8UawPHrSHkp4yyDJXtRhR9RlHCHy4bcCm1Yg==">>,
        stored_key =>
            <<"3ey3gzSsmbxcLnoc1VKCR/739uKX6uuPCyAzn6x8o87ibcjOdUaU8qhL5X4MUI9UPTt667GagNpVTmAWTFNsjA==">>}}

```
* *Serialized password:*
```
==MULTI_SCRAM==,4096,
===SHA1===QClQsw/sfPEnwj4AEp6E1w==|ys1104hRhqMoRputBY5sLHKXoSw=|EJvxXWM42tO7BgW21lNZyBc1dD0=,
==SHA224==dk0ImXFVPoUfqD5FveV7YA==|G0ibQ/YYuCtoun4I+1IF2zJ7Q8x2T23ETnq5Gg==|EvE2EkZcUb3k4CooeOcVFy95P32t+NDX0xbQUA==,
==SHA256==M7BYKSo04XbzBr4C7b056g==|A779MC05nSGQln5no0hKTGHFSaQ7oguKBZgORW3s+es=|XhtGFf6NDWsnVSCO4xkzPD3qc046fPL0pATZi7RmaWo=,
==SHA384==Ryu0fA29gbwgqFOBk5Mczw==|k3QwC0Lb1y1/V/31byC5KML5t3mH4JTPjFyeAz7lV2l4SPfzi3JHvLEdoNB5K/VY|kR+LMI/E0QBG3oF405/MTAT6NAlCOfPrFOaWH3WBVGM0Viu9Brk6kGwVwXjSP8v0,
==SHA512==SLNuVNcWiNBmnYZNIdj+zg==|3ey3gzSsmbxcLnoc1VKCR/739uKX6uuPCyAzn6x8o87ibcjOdUaU8qhL5X4MUI9UPTt667GagNpVTmAWTFNsjA==|jUUDbuQ9ae4UnAWS6RV6W4yifX3La3ESjfZjGol+TBROIb/ihR8UawPHrSHkp4yyDJXtRhR9RlHCHy4bcCm1Yg==
```

## Legacy format description

MongooseIM installations older or equal to 3.6.2 were supporting only SHA-1 as a hashing algorithm for SCRAM.
The SCRAM format that was used can be seen below.

`==SCRAM==,<stored key>,<server key>,<salt>,<iteration count>`

* `<stored key>` - Base64-encoded Stored Key
* `<server key>` - Base64-encoded Server Key
* `<salt>` - Base64-encoded Salt
* `<iteration count>` - Iteration Count formatted as a human-readable integer

In order to learn more about the meaning of the Stored Key, Server Key, Salt and Iteration Count, please check [the SCRAM specification][the SCRAM specification].

### Example

* *Password:* `misio`
* *Erlang record:* `#scram{ storedkey = <<"tmi5IE+9pceRV/jkPLFHEaVY33c=">>, serverkey = <<"MiWNa8T3dniVDwmh77ufJ41fpAQ=">>, salt = <<"inKXODlSY5y5SCsLxibi0w==">>, iterationcount = 4096 }`
* *Serialized password:* `==SCRAM==,tmi5IE+9pceRV/jkPLFHEaVY33c=,MiWNa8T3dniVDwmh77ufJ41fpAQ=,inKXODlSY5y5SCsLxibi0w==,4096`

[the SCRAM specification]: https://tools.ietf.org/html/rfc5802

## Known issues
### SCRAM hash calculation issue in MongooseIM 4.1.0–6.3.1

If you are using MongooseIM 4.1.0 to 6.3.1 with SCRAM authentication and have OpenSSL >=3.4.1 installed, hashes for algorithms stronger than SHA-1 are calculated incorrectly.
To fix this issue, you must upgrade to MongooseIM 6.3.2, which includes `fast_pbkdf2` version 2.0 with the bug fixed or downgrade OpenSSL to a version lower than 3.4.1.
After applying one of these fixes, all affected users must reset their passwords, as the previously stored hashes are incorrect.
