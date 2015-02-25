# OpenSSL FIPS

Support for OpenSSL FIPS was added to MongooseIM and is available in dedicated branch [fips-support](https://github.com/esl/MongooseIM/tree/fips-support)

## Incompatibilities

Currently known incompatible features are:

* SASL auth mechanism DIGEST-MD5 - due to forbidden md5 hash function in fips mode.

## Requirements

### Custom Erlang/OTP 17.4.1 with FIPS support
Compile Erlang/OTP from [michalwski/otp 17.4.1-fips](https://github.com/michalwski/otp/tree/fips-17.4.1)
Make sure option `--enable-fips` is specified for `configure` command. If you want to use different openssl than the default one specify option `--with-openssl=PATH_TO_YOUR_OPENSSL`

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

### How to check if fips mode is enabled

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
