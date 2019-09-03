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

## GDPR retrieval for MAM MUC limitation

When the personal data retrieval is executed for a user in a specific domain, Message Archive Management for groupchats must be running for this particular domain.
This is the case for most configurations but the problem manifests when a MongooseIM operator configures `mod_mam_muc`/`mod_mam_meta` to start only for a subset of domains supported by the cluster (`host_config` option).

In such case, personal data stored by MAM MUC will not be retrieved for this user.

### Proposed workaround

Start a dedicated MongooseIM instance with a slightly different config, which enables Message Archive Management for the user's domain.
This instance doesn't have to be clustered with other nodes and doesn't have to be accessible for actual users.

After a successful retrieval, this instance may be terminated and deleted if necessary.

