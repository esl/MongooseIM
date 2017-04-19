# OpenSSL FIPS

Support for OpenSSL FIPS was added to MongooseIM in version 1.7.0.

## Incompatibilities

Currently known incompatible features are:

* SASL auth mechanism DIGEST-MD5: due to a forbidden MD5 hash function in FIPS mode.

## Requirements

### Custom Erlang/OTP 17.4.1 with FIPS support

Compile Erlang/OTP from [michalwski/otp 17.4.1-fips](https://github.com/michalwski/otp/tree/fips-17.4.1).
Make sure the option `--enable-fips` is specified for `configure` command.
If you want to use a different OpenSSL than the default one, specify the option `--with-ssl=PATH_TO_YOUR_OPENSSL` as well.
Here's an example of a command for building Erlang/OTP with kerl:
```
KERL_CONFIGURE_OPTIONS="--enable-fips --with-ssl=/home/vagrant/openssl" \
./kerl build git https://github.com/michalwski/otp.git fips-17.4.1 17.4.1-fips
```

### Building MongooseIM with a custom OpenSSL

Before running `./rebar3 compile` or `make rel` please export the CFLAGS and LDFLAGS env vars pointing to a FIPS compliant OpenSSL, f.e.

```
export LDFLAGS=-Wl,-rpath=$OPENSSL_LIB -L/home/vagrant/openssl/lib
export CFLAGS=-I/home/vagrant/openssl/include
```

### How to enable/disable FIPS mode

Find `etc/app.config` in the release directory. 
FIPS mode is an option of the crypto application. 
In order to enable/disable it, add the following section to `app.config`:

```erlang
{crypto, [{fips_mode, Value}]},
```

where `Value` is either `true` or `false`.

### How to check if the FIPS mode is enabled

#### Log message

When MongooseIM starts, it prints the following log message if FIPS mode is enabled

```
2015-02-25 14:30:54.501 [warning] <0.242.0>@mongoose_fips:do_notify:37 FIPS mode enabled
```

#### Run-time check

Run the following function in the MongooseIM console:

```erlang
mongoose_fips:status().
```

### Cipher suites difference

A test using a `cipher_suites_test.sh` script (available in the tools directory) can be performed on MongooseIM with FIPS mode enabled and disabled. 
We've used `OpenSSL 1.0.1j-fips`.

Here are all the cipher suites available when the **FIPS** mode is **enabled**:

* ECDHE-RSA-AES256-SHA
* DHE-RSA-AES256-SHA
* AES256-SHA
* ECDHE-RSA-DES-CBC3-SHA
* EDH-RSA-DES-CBC3-SHA
* DES-CBC3-SHA
* ECDHE-RSA-AES128-SHA
* DHE-RSA-AES128-SHA
* AES128-SHA

Here are all the cipher suites available when the **FIPS** mode is **disabled**:

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

