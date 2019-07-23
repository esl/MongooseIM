This document provides a list of all known issues with MongooseIM operation and configuration.
You may also find proposed workarounds if any is available.

## MySQL + TLS + OTP 20.3

MongooseIM will not connect to MySQL over TLS on OTP 20.3 due to [the MySQL driver bug](https://github.com/mysql-otp/mysql-otp/issues/85).

### Proposed workarounds

* Upgrade OTP to 21.0 or higher.
* Use unencrypted communication with MySQL.

## MSSQL connectivity via ODBC

The ODBC driver currently used by MongooseIM is known to work only with Ubuntu Xenial x64 and FreeTDS 0.91-6.1build1.

It does not currently work on Ubuntu Bionic, CentOS 7 and macOS Mojave with the latest FreeTDS version.

The team is working on resolving this issue.
Please watch for updates in MongooseIM release notes.

### Proposed workarounds

* Use Ubuntu Xenial x64 for MongooseIM deployment. This OS version is still maintained.

