## TL;DR

In shell #1:

```sh
$ cd $MONGOOSEIM
$ make devrel
```

In shell #2:

```sh
$ cd $MONGOOSEIM/dev/mongooseim_node1
$ ./bin/mongooseimctl live
```

In shell #3:

```sh
$ cd $MONGOOSEIM/dev/mongooseim_node2
$ ./bin/mongooseimctl live
```

Back to shell #1:

```sh
$ make quicktest
```

Wait for the tests to finish and celebrate in joy (or despair in grief)!

## Step by step breakdown

`make devrel` builds two server nodes:
`$MONGOOSEIM/dev/mongooseim_node1` and `$MONGOOSEIM/dev/mongooseim_node2`.
These are preconfigured for breadth of features and compatible
with as many test suites as possible.
There are two of them in order to test XMPP federation (server to server
communication).

In general, running a server in interactive mode (i.e. `mongooseimctl
live`) is not required to test it, but it's convenient as any warnings /
errors can be spotted in real time.
It's also easy to inspect server state or trace execution (e.g. using `dbg`)
in case of anything going wrong in some of the tests.
To run server in the background instead of interactive mode,
use `mongooseimctl start && mongooseimctl started`.

The `quicktest` configuration is a relatively comprehensive one,
giving good overview of what does and what doesn't work in the system,
without repeating tests.
Why would we want to ever repeat tests?
In order to test different backends of the same parts of the system.
E.g. a message archive might store messages in MySQL or Cassandra - the
glue code between the XMPP logic module and database is different
in each case,
therefore repeating the same tests with different databases is necessary
to guarantee a truthful code coverage measurement.

## Testing a feature in development / TDD

The whole suite takes a significant amount of time to complete.
When you focus on a new feature, fast iteration speed is crucial to maintain
the flow (who doesn't like the feeling?!) and not lose focus.

Therefore, it's better to run tests from `$MONGOOSEIM/test/ejabberd_tests/`
instead of running them from the main project directory,
as it gives finer grained control on what exactly to test and what settings to use.
There we have:

```
$ tree test/ejabberd_tests/ -L 1 -F
test/ejabberd_tests/
├── Makefile
├── README.md
├── default.spec
├── full.spec
├── t.spec
├── test.config
├── tests/
├── vcard.config
└── ...
```

`tests/` is where the test suites reside.
`*.config` files are suite configuration files - they contain predefined
XMPP client specifications, server addresses and XMPP domains to use
and options required by test support libraries
(i.e. [Escalus](https://github.com/esl/escalus/)).
`*.spec` files are test specifications - they define the configuration
file to use, the suites, test groups or individual test cases to run
or skip and some less important things.
`default.spec` is, well, the default when running `make quicktest`,
but it can be overridden with `TESTSPEC` variable:

```sh
# make sure we're in $MONGOOSEIM/test/ejabberd_tests/
# Makefile in $MONGOOSEIM/ itself doesn't accept the extra parameters
cd $MONGOOSEIM/test/ejabberd_tests/
make quicktest TESTSPEC=my-feature.spec
```

It's customary to create a per-feature (or per-project, if you're cloning
away for your WhatsApp-only-better startup) `.spec` file and only enable
the suites / test groups you want to test - this speeds up the iteration
cycle by not testing parts of the system that you know have not changed.
It's worth running `default.spec` or even `full.spec` once in a while to
check for regressions, though.
Have a look into `default.spec` file to see how to pick only the
interesting tests to run.

If you're sure that none of the test dependencies have changed
and you only edited the test suites, it's possible to speed up
the test run even a bit more by skipping Rebar dependency / compilation
checks by providing `PREPARE=` (i.e. an empty value):

```sh
make quicktest PREPARE=
```

Have a look inside the `Makefile` to see how it works.
