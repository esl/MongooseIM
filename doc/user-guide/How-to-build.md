# How to build MongooseIM

Instructions provided in this page are verified for:

* CentOS 7
* Ubuntu 16.04 LTS (Xenial)
* Ubuntu 18.04 LTS (Bionic)
* macOS 10.14 (Mojave)

For any other OS versions, the instructions should still work, however, some steps or file paths may be different.

## Requirements

To compile MongooseIM you need:

*   Make
    * CentOS: `make`
    * Ubuntu: `make`
    * Mac: Xcode Command Line Tools
*   C and C++ compiler
    * CentOS: `gcc`, `gcc-c++`
    * Ubuntu: `gcc`, `g++`
    * Mac: Xcode Command Line Tools
*   Erlang/OTP 21.2 or higher;
    * CentOS: `erlang` 
    * Ubuntu: `erlang`
    * Mac (Homebrew): `erlang`
    * Alternative for CentOS and Ubuntu: `esl-erlang` from [Erlang Solutions website](https://www.erlang-solutions.com/resources/download.html)
    * Alternative for all OS: [kerl](https://github.com/kerl/kerl)
*   OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption
    * CentOS: `openssl-devel`
    * Ubuntu: `libssl-dev`
    * Mac (Homebrew): `openssl`
*   ODBC library
    * CentOS: `unixODBC-devel`
    * Ubuntu: `unixodbc-dev`
    * Mac (Homebrew): `unixodbc`
*   Zlib 1.2.3 or higher
    * CentOS: `zlib-devel`
    * Ubuntu: `zlib1g-dev`
    * Mac: built-in

## Preparing macOS environment

### Step 1

Install [Homebrew](http://brew.sh) to manage packages on your Mac.
You may use a different package manager but you'll need to figure out the package names and file paths on your own.

### Step 2

Install Xcode Command Line Tools.

```bash
$ xcode-select --install # install compilation tools
```

### Step 3

Install dependencies with Brew.

```
$ brew install erlang openssl unixodbc
```

### Step 4

Add OpenSSL paths to the compiler and linker environment variables:

```bash
$ export LDFLAGS="-L/usr/local/Cellar/openssl/[installed version]/lib/ -undefined dynamic_lookup $LDFLAGS"
$ export CFLAGS="-I/usr/local/Cellar/openssl/[installed version]/include/ $CFLAGS"
```

Please remember to replace `[installed version]` with the one that is present in your file system.
1.0.2s was the most recent one when this guide was written.

Now, please proceed to the "Building" section.

## Preparing CentOS environment

Please install the required dependencies:

```bash
$ sudo yum install git make zlib-devel unixODBC-devel gcc gcc-c++ erlang
```

Now, please proceed to the "Building" section.

## Preparing Ubuntu environment

Please install the required dependencies:

```bash
$ sudo apt install git make zlib1g-dev unixodbc-dev gcc g++ erlang
```

Now, please proceed to the "Building" section.

## Building

To compile MongooseIM, navigate to the main repo directory (referenced as `$REPO` in this guide) and execute:

```bash
$ make [rel]
```

`rel` is optional as it is the default target.
This will download all dependencies, compile everything and build a `prod` release.

If a more advanced release is required (with only specific DB support, e.g. mysql or pgsql) or you want to set the `prefix` or `user` for the installation script please refer to the [release configuration](release_config.md) page in our documentation.

The `make rel` commands will generate a self-contained OTP system structure in the project's `_build/prod/rel/mongooseim` subdirectory.
The contents of that directory are as follows:

*   `bin` - startup/administration scripts,
*   `etc` - configuration files,
*   `lib` - MongooseIM binary, header and runtime files,
*   `var` - spool directory,
*   `log` - log file directory,
*   `releases` - release files directory.

## Running MongooseIM

To run MongooseIM from the project tree after compiling it, change to `$REPO/_build/prod/rel/mongooseim`.

There you can use the `mongooseim` command line administration script to start and stop MongooseIM.
For example, this command will start the server:

```bash
$ bin/mongooseim start
```

You can also run the server in interactive mode (drop into an Erlang shell):

```bash
$ bin/mongooseim live
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
        log/ejabberd.log
```


## Building the testing target and running tests

For testing purposes there's a different make target available:

```
$ make devrel
```

which will generate releases `mim1`, `mim2`, `mim3`, `fed1`, `reg1` in `$REPO/_build/` and prepare them for testing and generating coverage reports.

In order to learn how to execute tests, please consult [Testing MongooseIM page](../developers-guide/Testing-MongooseIM.md)

