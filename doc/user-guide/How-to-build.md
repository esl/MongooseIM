# How to build

## Requirements

To compile MongooseIM you need:

*   GNU Make
*   GCC
*   Libexpat 1.95 or higher
*   Erlang/OTP 17.5 or higher
*   Reltool 0.6.6 or higher
*   OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption
*   Zlib 1.2.3 or higher for Stream Compression support (XEP-0138). Optional.

## Compiling on UNIX-like systems

To compile MongooseIM, go to the main repo directory `$REPO` and execute the command (`$` stands for the shell prompt):


        $ ./rebar3 compile

To generate a full MongooseIM release (i.e. an executable with mysql, pgsql or other dependencies):

        $ make rel

If a more advanced release is required (with only specific DB support, e.g. mysql or pgsql) or you want to set the `prefix` or `user` for the installation script please refer to the [release configuration](release_config.md) page in our documentation.

The `make rel` commands will generate a self-contained OTP system image in the project's `_build/prod/rel/mongooseim` subdirectory.
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

    $ bin/mongooseim start

You can also run the server in interactive mode (drop into an Erlang shell):

    $ bin/mongooseim live

There's also a tool called `mongooseimctl` to perform some operations on a running instance, e.g.:

    $ bin/mongooseimctl status
    MongooseIM node mongooseim@localhost:
      operating system pid: 86026
      Erlang VM status: started (of: starting | started | stopping)
      boot script status: started
      version: 1.6.2-61-g48b8332
      uptime: 1:12:46
      logs:
        log/ejabberd.log

## Building the testing target and running tests

For testing purposes there's a different make target available:

    $ make devrel

which will generate releases `mim1`, `mim2`, `mim3`, `fed1` in `$REPO/_build/` and prepare them for testing and generating coverage reports.

To run the tests (from project's root directory, i.e. `$REPO`):

    $ tools/travis-test
