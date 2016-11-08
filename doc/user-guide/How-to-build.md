# How to build

## 1.  Requirements

To compile MongooseIM you need:

*   GNU Make
*   GCC
*   Libexpat 1.95 or higher
*   Erlang/OTP 17.5 or higher
*   Reltool 0.6.6 or higher
*   OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption
*   Zlib 1.2.3 or higher for Stream Compression support (XEP-0138). Optional.

## 2.  Compiling on UNIX-like systems

To compile MongooseIM, go to the main repo directory `$REPO` and execute the command (`$` stands for the shell prompt):

        $ make

or

        $ ./rebar get-deps
        $ ./rebar compile

To generate full MongooseIM release (with mysql, pgsql or other deps):

        $ make rel

If more advanced release is required (with some specific db support only, f.e. mysql or pgsql) or you want to set `prefix` or `user` for the installation script please refer to the [release configuration](doc/user-guide/release_config.md) page in our documentation.

The `make rel` commands will generate a self-contained OTP system image in the project's `rel/mongooseim` subdirectory. The contents of that directory are as follows:

*   `rel/mongooseim/bin` - startup/administration scripts,
*   `rel/mongooseim/etc` - configuration files,
*   `rel/mongooseim/lib` - MongooseIM binary, header and runtime files,
*   `rel/mongooseim/var` - spool directory,
*   `rel/mongooseim/log` - log file directory,
*   `rel/mongooseim/releases` - release files directory.

## 3.  Running MongooseIM

To run MongooseIM from the project tree after compiling it, change to `$REPO/rel/mongooseim`.

There you can use the `mongooseim` command line administration script to start and stop MongooseIM. For example, this command will start the server:

    $ bin/mongooseim start

You can also run the server in interactive mode:

    $ bin/mongooseim live

There's also a tool called `mongooseimctl` allowing you to perform some operations on a running instance, e.g.:

    $ bin/mongooseimctl status
    MongooseIM node mongooseim@localhost:
      operating system pid: 86026
      Erlang VM status: started (of: starting | started | stopping)
      boot script status: started
      version: 1.6.2-61-g48b8332
      uptime: 1:12:46
      logs:
        log/ejabberd.log

## 4.  Building the testing target and running tests

For testing purposes there's a different make target available:

    $ make devrel

which will generate releases in `$REPO/dev/` and prepare them for testing and generating coverage reports.

To run the tests (from project's root directory, i.e. `$REPO`):

    $ dev/mongooseim_node1/bin/mongooseim start
    $ dev/mongooseim_node2/bin/mongooseim start
    $ make quicktest

The test results will show up in the console`.
