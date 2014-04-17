MongooseIM  [![](http://opensource.erlang-solutions.com/mongooseim/icon.png "Build status")](http://opensource.erlang-solutions.com/mongooseim/ct_report/all_runs.html) [![Build Status](https://travis-ci.org/esl/MongooseIM.svg?branch=master)](https://travis-ci.org/esl/MongooseIM)
============
MongooseIM (previously esl-ejabberd) is Erlang Solutions' robust and efficient XMPP
server aimed at large installations. Specifically designed for enterprise purposes,
it is fault-tolerant, can utilize resources of multiple clustered machines
and easily scale in need of more capacity (by just adding a box/VM).
It provides support for WebSockets and reimplemented BOSH.

Its home at GitHub is http://github.com/esl/MongooseIM.


Quickstart guide
----------------
For a quick start just
[download the pre-built package](https://www.erlang-solutions.com/downloads/download-mongooseim)
that suits your platform.


Main differences from the parent project
----------------------------------------
This project began its life as a fork of
[ejabberd v.2.1.8](https://github.com/processone/ejabberd)
and later underwent some major cleanup, refactorization and optimization.

Major steps performed:
*   bringing the project source tree to compliance with OTP project structure
    recommendations,
*   swapping `autotools` for the Erlang community-standard build tool `rebar`,
*   removal of obsolete and/or rarely used modules to reduce maintenance
    burden,
*   reduction of runtime memory consumption by refactoring the code
    to use Erlang's binary data type for string manipulation and storage
    instead of operating on linked lists of characters,
*   functional test coverage of the system according to corresponding
    RFCs and XEPs.


How to build
------------
1.  Requirements.

    To compile MongooseIM you need:
    *   GNU Make,
    *   GCC,
    *   Libexpat 1.95 or higher,
    *   Erlang/OTP R15B or higher,
    *   Reltool 0.5.4 or higher,
    *   OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption,
    *   Zlib 1.2.3 or higher for Stream Compression support (XEP-0138). Optional,
    *   PAM library. Optional. For Pluggable Authentication Modules (PAM).

2.  Compiling on UNIX-like systems.

    To compile MongooseIM, go to the main repo directory `$REPO` and execute
    the command (`$` stands for the shell prompt):

        $ make

    or

        $ ./rebar get-deps
        $ ./rebar compile

    To generate MongooseIM release:

        $ make rel

    or

        $ ./rebar generate

    These commands will generate a self-contained OTP system image in the
    project's `rel/mongooseim` subdirectory. The contents of that directory are as
    follows:
    *   `rel/mongooseim/bin` - startup/administration scripts,
    *   `rel/mongooseim/etc` - configuration files,
    *   `rel/mongooseim/lib` - MongooseIM binary, header and runtime files,
    *   `rel/mongooseim/var` - spool directory,
    *   `rel/mongooseim/log` - log file directory,
    *   `rel/mongooseim/releases` - release files directory.

3.  Running MongooseIM.

    To run MongooseIM from the project tree after compiling it, change
    to `$REPO/rel/mongooseim`.

    There you can use the `mongooseim` command line administration script to
    start and stop MongooseIM. For example:

        $ bin/mongooseim start

    will start the server.

    You can also run the server in interactive mode:

        $ bin/mongooseim live

    There's also a tool called `mongooseimctl` allowing you to perform some
    operations on a running instance, e.g.:

        $ bin/mongooseimctl status
        The node mongooseim@localhost is started with status: started
        MongooseIM version 1.3.1 is running on that node

4.  Building the testing target and running tests.

    For testing purposes there's a different make target available:

        $ make testrel

    which will generate releases in `$REPO/dev/` and prepare
    them for testing and generating coverage reports.

    To run the tests (from project's root directory, i.e. `$REPO`):

        $ cd test
        $ make cover_test

    The test results will show up in the console and a coverage report will
    be generated in `$REPO/test/ct_report/cover.html`.


MongooseIM documentation notice
---------------------------------
MongooseIM, being a fork of ejabberd, is currently in a phase
of rapid development. Because of that the documentation found in the `doc/`
subdirectory of the source tree, while mostly relevant, may be inaccurate.

The main reason is that some rarely useful features were removed from
the repository or are still waiting to be brought up to the Erlang Solutions standards.

Don't forget to check out our [wiki](https://github.com/esl/MongooseIM/wiki) - hopefully, its scope will grow with time.


Want to get in touch with us?
-----------------------------
In case of any suggestions, questions or any random thoughts on this project,
please feel free to contact us by the standard GitHub ways or at
<a href='mailto:mongoose-im@erlang-solutions.com'>mongoose-im@erlang-solutions.com</a>.
