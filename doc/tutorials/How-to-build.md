# How to build MongooseIM

Instructions provided in this page are verified for:

* CentOS 7
* Ubuntu 16.04 LTS (Xenial)
* Ubuntu 18.04 LTS (Bionic)
* macOS 13.3 (Ventura)

For any other OS versions, the instructions should still work, however, some steps or file paths may be different.

## Requirements

To compile MongooseIM you need:

=== "CentOS"

      *   Make: `make`,
      *   C and C++ compiler: `gcc`, `gcc-c++`,
      *   Erlang/OTP 24.0 or higher:
        * `erlang` package, or,
        * `esl-erlang` from [Erlang Solutions website](https://www.erlang-solutions.com/resources/download.html), or,
        * install using [kerl](https://github.com/kerl/kerl),
      *   OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption: `openssl` and `openssl-devel`,
      *   ODBC library: `unixODBC-devel`,
      *   Zlib 1.2.3 or higher: `zlib-devel`.

=== "Ubuntu"

      *   Make: `make`,
      *   C and C++ compiler: `gcc`, `g++`,
      *   Erlang/OTP 24.0 or higher:
        * `erlang` package, or,
        * `esl-erlang` from [Erlang Solutions website](https://www.erlang-solutions.com/resources/download.html), or,
        * install using [kerl](https://github.com/kerl/kerl),
      *   OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption: `olibssl-dev`,
      *   ODBC library: `unixodbc-dev`,
      *   Zlib 1.2.3 or higher: `zlib1g-dev`.

=== "macOS"

      *   Make, C and C++ compiler: Xcode Command Line Tools,
      *   Erlang/OTP 24.0 or higher:
        * [`erlang`](https://formulae.brew.sh/formula/erlang) from Homebrew,
        * install using [kerl](https://github.com/kerl/kerl),
      *   OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption: [`openssl`](https://formulae.brew.sh/formula/openssl@1.1) from Homebrew
      *   ODBC library: [`unixodbc`](https://formulae.brew.sh/formula/unixodbc) from Homebrew.

## Preparing the environment

=== "centOS"

    Please install the required dependencies:

    ```bash
    sudo yum install git make zlib-devel openssl openssl-devel unixODBC-devel gcc gcc-c++ erlang
    ```

    Now, please proceed to the "Building" section.

=== "Ubuntu"

    Please install the required dependencies:

    ```bash
    sudo apt install git make zlib1g-dev libssl-dev unixodbc-dev gcc g++ erlang
    ```

    Now, please proceed to the "Building" section.

=== "macOS"

    **Step 1**

    Install [Homebrew](http://brew.sh) to manage packages on your Mac.
    You may use a different package manager but you'll need to figure out the package names and file paths on your own.

    **Step 2**

    Install Xcode Command Line Tools.

    ```bash
    xcode-select --install # install compilation tools
    ```

    **Step 3**

    Install dependencies with Brew.

    ```bash
    brew install erlang openssl unixodbc
    ```

    **Step 4**

    Add OpenSSL paths to the compiler and linker environment variables:

    ```bash
    export LDFLAGS="-L/usr/local/opt/openssl/lib"
    export CFLAGS="-I/usr/local/opt/openssl/include"
    ```

    Now, please proceed to the "Building" section.


## Building

To compile MongooseIM, navigate to the main repo directory (referenced as `$REPO` in this guide) and execute:

```bash
make [rel]
```

`rel` is optional as it is the default target.
This will download all dependencies, compile everything and build a `prod` release.

If a more advanced release is required (with only specific DB support, e.g. mysql or pgsql) or you want to set the `prefix` or `user` for the installation script please refer to the [release configuration](../developers-guide/release_config.md) page in our documentation.

The `make rel` commands will generate a self-contained OTP system structure in the project's `_build/prod/rel/mongooseim` subdirectory.
The contents of that directory are as follows:

*   `bin` - startup/administration scripts,
*   `etc` - configuration files,
*   `lib` - MongooseIM binary, header and runtime files,
*   `var` - spool directory,
*   `log` - log file directory,
*   `releases` - release files directory.

## Running MongooseIM

To run MongooseIM from the project tree after compiling it, change the directory to `$REPO/_build/prod/rel/mongooseim`.

There you can use the `mongooseim` command line administration script to start and stop MongooseIM.
For example, this command will start the server:

```bash
bin/mongooseim start
```

You can also run the server in interactive mode (drop into an Erlang shell):

```bash
bin/mongooseim live
```

There's also a tool called `mongooseimctl` to perform some operations on a running instance, e.g.:

```
$ bin/mongooseimctl status
MongooseIM node mongooseim@localhost:
    operating system pid: 3105
    Erlang VM status: started (of: starting | started | stopping)
    boot script status: started
    version: 3.4.0-7-gaec944c92 (as mongooseim)
    uptime: 0 days 00:00:12
    distribution protocol: inet_tcp
    logs:
        log/mongooseim.log
```

## Building the testing target and running tests

For testing purposes there's a different make target available:

```bash
make devrel
```

which will generate releases `mim1`, `mim2`, `mim3`, `fed1`, `reg1` in `$REPO/_build/` and prepare them for testing and generating coverage reports.

In order to learn how to execute tests, please consult [Testing MongooseIM page](../developers-guide/Testing-MongooseIM.md).
