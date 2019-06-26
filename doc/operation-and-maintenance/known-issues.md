This document provides a list of all known issues with MongooseIM operation and configuration.
You may also find proposed workarounds if any is available.

## MySQL + TLS + OTP 20.3

MongooseIM will not connect to MySQL over TLS on OTP 20.3 due to [the MySQL driver bug](https://github.com/mysql-otp/mysql-otp/issues/85).

### Proposed workarounds

* Upgrade OTP to 21.0 or higher.
* Use unencrypted communication with MySQL.

