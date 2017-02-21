# OpenSSL FIPS

Support for OpenSSL FIPS was added to MongooseIM in version 1.7.0.

## Incompatibilities

Currently known incompatible features are:

* SASL auth mechanism DIGEST-MD5: due to forbidden MD5 hash function in FIPS mode.

## Requirements

### Custom Erlang/OTP 17.4.1 with FIPS support

Compile Erlang/OTP from [michalwski/otp 17.4.1-fips](https://github.com/michalwski/otp/tree/fips-17.4.1)
Make sure option `--enable-fips` is specified for `configure` command.
If you want to use different OpenSSL than the default one, specify option `--with-ssl=PATH_TO_YOUR_OPENSSL` as well.
Example command for building Erlang/OTP with kerl would look like the
following:
```
KERL_CONFIGURE_OPTIONS="--enable-fips --with-ssl=/home/vagrant/openssl" \
./kerl build git https://github.com/michalwski/otp.git fips-17.4.1 17.4.1-fips
```

### Building MongooseIM with custom OpenSSL

Before running any `./rebar3 compile` or `make rel` please export CFLAGS and LDFLAGS env vars
pointing to FIPS compliant OpenSSL, f.e.

```
export LDFLAGS=-Wl,-rpath=$OPENSSL_LIB -L/home/vagrant/openssl/lib
export CFLAGS=-I/home/vagrant/openssl/include
```

### How to enable/disable FIPS mode

In the release directory there is file `etc/app.config`. FIPS mode is an option of crypto application. In order to enable/disable FIPS mode add following section to `app.config`

```erlang
{crypto, [{fips_mode, Value}]},
```

Where `Value` is `true` or `false`.

### How to check if FIPS mode is enabled

#### Log message

When MongooseIM starts it prints following log message if FIPS mode is enabled

```
2015-02-25 14:30:54.501 [warning] <0.242.0>@mongoose_fips:do_notify:37 FIPS mode enabled
```

#### Run-time check

In MongooseIM's console run function:

```erlang
mongoose_fips:status().
```

### Cipher suites difference

A test using `cipher_suites_test.sh` script (to be found in tools
directory) was run against MongooseIM with FIPS mode enabled and
disabled. The OpenSSL version was: `OpenSSL 1.0.1j-fips`.

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

