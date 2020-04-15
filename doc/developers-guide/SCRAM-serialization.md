## Overview

This document describes the SCRAM serialization format used by MongooseIM.
Developers can use this information to create advanced endpoints for `ejabberd_auth_http` or enable other software to read (i.e. share) the user authentication data.

## Format description

`==MULTI_SCRAM==,<salt>,<iteration count>,===SHA1===<stored key>|<server key>,==SHA224==<stored key>|<server key>,==SHA256==<stored key>|<server key>,==SHA384==<stored key>|<server key>,==SHA512==<stored key>|<server key>`

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
  salt => <<"aml22qUoKvwJHccCCH00eQ==">>,
  sha =>
      #{server_key => <<"gkoblYUZnW8GRBhIyJnbflHlLYs=">>,
        stored_key => <<"+nm4+0ONdpgnoypippxdzV5sQ80=">>},
  sha224 =>
      #{server_key =>
            <<"JwSXbKqMRJEwOr/iNmqN+x3UzxRikmKym9E71g==">>,
        stored_key =>
            <<"nUi8YwRBRMAusH/KpINo3/AO32UWzlSONX9wMA==">>},
  sha256 =>
      #{server_key =>
            <<"y0cB/hZ7AKtVMC2WCkXlo4XTNfOQVg30PLfIhK+Wf/U=">>,
        stored_key =>
            <<"tiDUGNpvmt75PGcCvwoTLVOF/og/BiX1FOpihXlYqW8=">>},
  sha384 =>
      #{server_key =>
            <<"LJKetdkPytdOXg6aj4NN25KmJatJsl5zaU78bzjowYrBcjG+wux/I5q7E78sQVSn">>,
        stored_key =>
            <<"s7SdIo5a+LH/EsKIMoqa4PPEveScCnDwP1LeaAzVdANT5pPSMio/CoMDN4uXfnHr">>},
  sha512 =>
      #{server_key =>
            <<"TCuJFv5dmpshThJbQnURW0LOz7D55d5hgYndA3jdklQd2omL6PpfgfIgToyVvYlsF9sRGYOg255y+Q+ltwW3tQ==">>,
        stored_key =>
            <<"4ATRLxRB+d6YyZXxi3PorT6kyS4Mr6tEuKUVhInJcRU0NDpXh94Y/Yrd+EDOSZUnPno8aAzj78NUXOTyoB98rg==">>}}
```
* *Serialized password:*
```

==MULTI_SCRAM==,aml22qUoKvwJHccCCH00eQ==,4096,
===SHA1===+nm4+0ONdpgnoypippxdzV5sQ80=|gkoblYUZnW8GRBhIyJnbflHlLYs=,
==SHA224==nUi8YwRBRMAusH/KpINo3/AO32UWzlSONX9wMA==|JwSXbKqMRJEwOr/iNmqN+x3UzxRikmKym9E71g==,
==SHA256==tiDUGNpvmt75PGcCvwoTLVOF/og/BiX1FOpihXlYqW8=|y0cB/hZ7AKtVMC2WCkXlo4XTNfOQVg30PLfIhK+Wf/U=,
==SHA384==s7SdIo5a+LH/EsKIMoqa4PPEveScCnDwP1LeaAzVdANT5pPSMio/CoMDN4uXfnHr|LJKetdkPytdOXg6aj4NN25KmJatJsl5zaU78bzjowYrBcjG+wux/I5q7E78sQVSn,
==SHA512==4ATRLxRB+d6YyZXxi3PorT6kyS4Mr6tEuKUVhInJcRU0NDpXh94Y/Yrd+EDOSZUnPno8aAzj78NUXOTyoB98rg==|TCuJFv5dmpshThJbQnURW0LOz7D55d5hgYndA3jdklQd2omL6PpfgfIgToyVvYlsF9sRGYOg255y+Q+ltwW3tQ==

```

## Legacy format description

MongooseIM installations older or equal to 3.6.2 were supporting only SHA-1 as a hashing algorithm for SCRAM. The SCRAM format that was used can be seen below.

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
