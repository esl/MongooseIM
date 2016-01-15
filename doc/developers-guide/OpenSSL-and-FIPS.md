# OpenSSL FIPS

Support for OpenSSL FIPS was added to MongooseIM in version 1.7.0

## Incompatibilities

Currently known incompatible features are:

* SASL auth mechanism DIGEST-MD5 - due to forbidden md5 hash function in fips mode.

## Requirements

### Custom Erlang/OTP 17.4.1 with FIPS support
Compile Erlang/OTP from [michalwski/otp 17.4.1-fips](https://github.com/michalwski/otp/tree/fips-17.4.1)
Make sure option `--enable-fips` is specified for `configure` command.
If you want to use different openssl than the default one, specify option `--with-ssl=PATH_TO_YOUR_OPENSSL` as well.
Example command for building Erlang/OTP with kerl would look like the
following:
```
KERL_CONFIGURE_OPTIONS="--enable-fips --with-ssl=/home/vagrant/openssl" \
./kerl build git https://github.com/michalwski/otp.git fips-17.4.1 17.4.1-fips
```

### Building MongooseIM with custom openssl

Before running any `make compile` or `make rel` please export OPENSSL_LIB and OPENSSL_INC env vars, f.e.

```
export OPENSSL_LIB=/home/vagrant/openssl/lib
export OPENSSL_INC=/home/vagrant/openssl/include
```

### How to enable/disable fips mode

In the release directory there is file `etc/app.config`. Fips mode is an option of crypto application. In order to enable/disable fips mode add following section to `app.config`

```erlang
{crypto, [{fips_mode, Value}]},
```

Where `Value` is `true` or `false`

### How to check if FIPS mode is enabled

#### Log message
When MongooseIM starts it prints following log message if fips mode is enabled

```
2015-02-25 14:30:54.501 [warning] <0.242.0>@ejabberd_app:do_notify_fips_mode:265 FIPS mode enabled
```

#### Run-time check

In MongooseIM's console run function:

```erlang
ejabberd_config:fips_mode().
```

### Cipher suites difference

A test using `cipher_suites_test.sh` script (to be found in tools
directory) was run against MongooseIM with FIPS mode enabled and
disabled. The OpenSSL version was: `OpenSSL 1.0.1j-fips`

Below there is a list of all possible cipher suites when **FIPS** mode was
**enabled**:

* ECDHE-RSA-AES256-SHA
* DHE-RSA-AES256-SHA
* AES256-SHA
* ECDHE-RSA-DES-CBC3-SHA
* EDH-RSA-DES-CBC3-SHA
* DES-CBC3-SHA
* ECDHE-RSA-AES128-SHA
* DHE-RSA-AES128-SHA
* AES128-SHA

Here goes list of all possible cipher suites when **FIPS** mode was
**disabled**:

* ECDHE-RSA-AES256-SHA
* DHE-RSA-AES256-SHA
* DHE-RSA-CAMELLIA256-SHA
* AES256-SHA
* CAMELLIA256-SHA
* ECDHE-RSA-DES-CBC3-SHA
* EDH-RSA-DES-CBC3-SHA
* DES-CBC3-SHA
* ECDHE-RSA-AES128-SHA
* DHE-RSA-AES128-SHA
* DHE-RSA-SEED-SHA
* DHE-RSA-CAMELLIA128-SHA
* AES128-SHA
* SEED-SHA
* CAMELLIA128-SHA
* ECDHE-RSA-RC4-SHA
* RC4-SHA
* RC4-MD5

