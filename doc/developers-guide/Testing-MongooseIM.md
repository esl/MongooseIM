## TL;DR

In shell #1:

```sh
$ cd $MONGOOSEIM
$ ./rebar3 compile
$ make devrel
```

In shell #2:

```sh
$ cd $MONGOOSEIM/_build/mim1/rel
$ ./bin/mongooseimctl live
```

In shell #3:

```sh
$ cd $MONGOOSEIM/_build/mim2/rel
$ ./bin/mongooseimctl live
```

In shell #4:

```sh
$ cd $MONGOOSEIM/_build/mim3/rel
$ ./bin/mongooseimctl live
```

In shell #5:

```sh
$ cd $MONGOOSEIM/_build/fed1/rel
$ ./bin/mongooseimctl live
```

Back to shell #1:

```sh
$ cd test.disabled/ejabberd_tests
$ make quicktest
```

Wait for the tests to finish and celebrate (or wallow in despair and grief)!

## Step by step breakdown

`make devrel` builds required server nodes:
These are preconfigured for breadth of features and compatible with as many test suites as possible.
There are four of them:
- `$MONGOOSEIM/_build/mim1/rel`, for most test SUITEs
- `$MONGOOSEIM/_build/mim*/rel`, in order to test cluster-related commands;;
- `$MONGOOSEIM/_build/fed1/rel`, in order to test XMPP federation (server to server communication, S2S).

In general, running a server in the interactive mode (i.e. `mongooseimctl live`) is not required to test it, but it's convenient as any warnings and errors can be spotted in real time.
It's also easy to inspect the server state or trace execution (e.g. using `dbg`) in case of anything going wrong in some of the tests.
To run the server in the background instead of the interactive mode, use `mongooseimctl start && mongooseimctl started`.

The `quicktest` configuration is a relatively comprehensive one, giving good overview of what does and what doesn't work in the system, without repeating tests.
Why would we want to ever repeat the tests?
In order to test different backends of the same parts of the system.
E.g. a message archive might store messages in MySQL/PostgreSQL or Riak KV - the glue code between the XMPP logic module and database is different in each case, therefore repeating the same tests with different databases is necessary to guarantee a truthful code coverage measurement.

## Testing a feature in development / TDD

The whole suite takes a significant amount of time to complete.
When you develop a new feature, the speed of iterating is crucial to maintain the flow (who doesn't like the feeling?!) and not lose focus.

In  `$MONGOOSEIM/test.disabled/ejabberd_tests/` we have:

```
$ tree test/ejabberd_tests/ -L 1 -F
test/ejabberd_tests/
├── Makefile
├── README.md
├── default.spec
├── test.config
├── tests/
└── ...
```

`tests/` is where the test suites reside.

`*.config` files are the suite configuration files - they contain predefined XMPP client specifications, server addresses and XMPP domains to use, and options required by test support libraries (i.e. [Escalus](https://github.com/esl/escalus/)).

`*.spec` files are the test specifications - they define the configuration file to use, the suites, test groups or individual test cases to run or skip, and some less important things.

`default.spec` is the default when running `make quicktest`, but it can be overridden with a `TESTSPEC` variable:

```sh
# make sure we're in $MONGOOSEIM/test.disabled/ejabberd_tests/
cd $MONGOOSEIM/test/ejabberd_tests/
make quicktest TESTSPEC=my-feature.spec
```

It's customary to create a per-feature (or per-project, if you're cloning away) `.spec` file and only enable the suites / test groups you want to test - this speeds up the iteration cycle by not testing the parts of the system that you know have not changed.
It's worth running `default.spec` once in a while to check for regressions, though.

Have a look into the `default.spec` file to see how to pick only the interesting tests to run.

If you're sure that none of the test dependencies have changed and you only edited the test suites, it's possible to speed up the tests by skipping the Rebar dependency and compilation checks by providing `PREPARE=` (i.e. an empty value):

```sh
make quicktest PREPARE=
```

Have a look inside the `Makefile` to see how it works.

### Reloading node(s) code

When working on a feature or a bug fix, often you modify the code and check if it works as expected. 
In order to change the code on dev nodes that are already generated (`mim*` and `fed*`), recompile the code for a specific node.
For example to update the code on `mim1` node, all you have to do is:
```sh
./rebar3 as mim1 compile
```

A similar command applies to other nodes, the important thing here is rebar3's profile.

When the above command finishes, the code can be reloaded on the server by:
1. loading new module(s) in the node's shell, f.e. `l(mongoose_riak)`
1. restarting the node

