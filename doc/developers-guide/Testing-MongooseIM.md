# Unit tests (a.k.a. "small tests")

These test suites are aimed at testing various modules and libraries standalone, without launching a MongooseIM instance.
They are very useful for developing/debugging libraries.

The test suites are located in `test/` directory.
To run all of them, use `./rebar3 ct`; to run just a selected suite, use `./rebar3 ct --suite test/my_selected_SUITE`.
Rebar recompiles all the code automatically, there is no need for a separate compilation step.

If all the tests pass, you wll get no output and summary log will be available in ct.log.
If any of the tests fail the summary log is printed to stdout.

Detailed test results in a nice HTML format are saved in
```
_build/test/logs/ct_run.[something][datetime]/
```

# End-to-end tests (a.k.a. "big tests")

## TL;DR

In shell #1:

```sh
$ cd $MONGOOSEIM
$ ./rebar3 compile
$ make devrel
```

In shell #2:

```sh
$ cd $MONGOOSEIM/_build/mim1/rel/mongooseim
$ ./bin/mongooseimctl live
```

In shell #3:

```sh
$ cd $MONGOOSEIM/_build/mim2/rel/mongooseim
$ ./bin/mongooseimctl live
```

In shell #4:

```sh
$ cd $MONGOOSEIM/_build/mim3/rel/mongooseim
$ ./bin/mongooseimctl live
```

In shell #5:

```sh
$ cd $MONGOOSEIM/_build/fed1/rel/mongooseim
$ ./bin/mongooseimctl live
```

Back to shell #1:

```sh
$ cd test.disabled/ejabberd_tests
$ make quicktest
```

Wait for the tests to finish and celebrate (or wallow in despair and grief)!

One-liner alternative for tmux users:

```sh
./rebar3 compile
make devrel
tmux new-window -n mim1 '_build/mim1/rel/mongooseim/bin/mongooseimctl live'
tmux new-window -n mim2 '_build/mim2/rel/mongooseim/bin/mongooseimctl live'
tmux new-window -n mim3 '_build/mim3/rel/mongooseim/bin/mongooseimctl live'
tmux new-window -n fed1 '_build/fed1/rel/mongooseim/bin/mongooseimctl live'
_build/mim1/rel/mongooseim/bin/mongooseimctl started
_build/mim2/rel/mongooseim/bin/mongooseimctl started
_build/mim3/rel/mongooseim/bin/mongooseimctl started
_build/fed1/rel/mongooseim/bin/mongooseimctl started
make -C test.disabled/ejabberd_tests quicktest
```

Start a new tmux and paste the commands.

## Step-by-step breakdown

`make devrel` builds four server nodes, preconfigured for a wide range of features covered by end-to-end tests.

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
$ tree test.diabled/ejabberd_tests/ -L 1 -F
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

To speed up the development cycle, developers usually create a `.spec` file for each feature (or each project, if you're cloning away) and only enable the suites / test groups they are working on.
The allows testing only the parts of the system that are actually being changed.
It's worth running `default.spec` once in a while to check for regressions.

Consult the `default.spec` file to see how to run only selected tests/groups/cases.

If you're sure that none of the test dependencies have changed and you only edited the test suites and/or MongooseIM code, it's possible to speed up the tests by skipping the Rebar dependency and compilation checks by providing `PREPARE=` (i.e. an empty value):

```sh
make quicktest PREPARE=
```

Consult the `test.disabled/ejabberd_tests/Makefile` to see how it works.

### Applying code changes

When working on a feature or a bug fix you often modify the code and check if it works as expected.
In order to change the code on dev nodes that are already generated (`mim*` and `fed*`) recompile the code for a specific node.
For example, to update the code on `mim1` node all you have to do is:
```sh
./rebar3 as mim1 compile
```

A similar command applies to other nodes, the important thing being rebar3's profile.

When the above command finishes, the code can be reloaded on the server by either reloading changed module(s) in the node's shell, e.g. `l(mongoose_riak)`, or restarting the node.

### Reading test reports

When finished, the test engine writes detailed html reports into a directory:

```
test.disabled/ejabberd_tests/ct_report/ct_run.[gobbledygook][datetime]/
```

Each run is saved into a new directory. This snippet:

```
#!/bin/bash

lst=$(ls -rt ct_report | grep ct_run | tail -n 1)
rm ct_report/lastrun
ln -s $lst ct_report/lastrun
```

can be of some help.

## Checking coverage

If you want to check how much of the code is covered by tests, run:

```
make cover_quicktest
```

Note: You need all the mim nodes (mim1, mim2 and mim3) up and running, even if you only run some of the tests. If any of the nodes is down, the test will crash.

This command will recompile and reload the code on dev nodes with coverage enabled and run test suites as defined in the spec.
Coverage statistics will be available in `test.disabled/ejabberd_tests/ct_report/cover.html` and `coverage` subdirectory.

## Advanced topics

There are many more options available.
One of them is sequentially testing a number of preset configurations - we do it every day on Travis, testing MongooseIM with various OTP versions and database backends.
Altogether, we have eight preset configuration.

If you want to dig deeper, consult `.travis.yml` and `tools/travis-test.sh`, everything we do is there.

### Gathering test reports from Travis tests

If you test your MongooseIM fork on Travis, you might want to access test reports (which also include node logs and crash dumps) that are created by the test runner.

#### Uploading reports to S3

Our script uses AWS CLI to upload test results to an S3 bucket.
Simply set [relevant environment variables](https://docs.aws.amazon.com/cli/latest/userguide/cli-environment.html) in your repository settings on Travis (at least `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` have to be set), and enjoy test reports landing straight into your bucket (`AWS_BUCKET` variable should store the bucket's name).

#### Uploading reports to Google Drive

To store test results in Google Drive you need to [create a new project and obtain service account credentials](https://developers.google.com/identity/protocols/OAuth2ServiceAccount).
You must also add Google Drive API to your project - to do this, navigate to *APIs & Services* in your project console and find & add *Google Drive API* in the *Library* tab.
Once downloaded, encode the credentials file with base64 (e.g. `cat serviceCreds.json | base64`) and use the result as `GDRIVE_SERVICE_ACCOUNT_CREDENTIALS` environment variable in your Travis repository settings.

##### Sharing reports with your personal account

The uploaded files will belong to the project that you created, i.e. will not be immediately visible from your personal Google Drive UI.
To be able to access from your personal account, you can share the reports' directory with your account.
To do this, we suggest using [gdrive](https://github.com/prasmussen/gdrive) utility:

```sh
$ go get github.com/prasmussen/gdrive # install gdrive (requires go language)
$ # Create a directory where all test results will be uploaded.
$ # This command assumes that service account credentials file
$ # you downloaded previously is saved under path `/tmp/serviceAccountCredentials`
$ ~/go/bin/gdrive mkdir -c /tmp --service-account serviceAccountCredentials --parent root mongooseim-test-reports
Directory 12345abcdef created
$ # Note the id of the created directory and use it (along with your email) in the next command
$ ~/go/bin/gdrive share -c /tmp --service-account serviceAccountCredentials --role writer --type user --email 'your.email@gmail.com' '1234abcdef'
```

Set `GDRIVE_PARENT_DIR` environment variable of your Travis build to ID of the directory you created in the previous step.

##### Additional notes

Be aware that uploads to Google Drive will take much longer time than upload to S3 and you may encounter rate limiting errors if you perform many builds in a short timespan.

## Load testing

Alongside CI, we do also CLT (Continuous Load Testing).
We have our own load testing infrastructure, called Tide, which is triggered after every successful test run, and gives us a feedback on changes to MongooseIM performance.

Test results are publicly available on the [Hello Tide!](http://tide.erlang-solutions.com/public) page.
