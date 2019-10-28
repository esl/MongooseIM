# Test runner

The test runner script is used to compile MongooseIM and run tests.

## IMPORTANT!

* ODBC preset can only be tested [on Ubuntu Xenial x64](../operation-and-maintenance/known-issues.md).
* SELinux may prevent containers from accessing the disk. Please either disable it or add proper rules to the policy.

## Requirements

### Docker

Docker must be installed on the local system and the user executing the tests must have privileges to start new containers (usually achieved by adding the user to the `docker` group).

### MSSQL connectivity

MongooseIM requires FreeTDS in order to connect to MSSQL container.

First of all, please install the driver itself:

```bash
# Ubuntu
$ sudo apt install freetds-dev tdsodbc

# CentOS
$ sudo yum install freetds

# macOS
$ brew install freetds
```

Then, please modify `tools/travis-setup-db.sh` script to use the proper FreeTDS paths.
Find a configuration block starting with `[mongoose-mssql]`.

* In case of Ubuntu, you don't need to change anything.
* For CentOS, change `Driver` and `Setup` to point `/usr/lib64/libtdsodbc.so.0` and `/usr/lib64/libtdsS.so` respectively.
* For macOS, remove the `Setup` line and set `Driver` to `Driver = /usr/local/Cellar/freetds/[current version]/lib/libtdsodbc.so`.

## How to print the instructions

The help command prints a list of supported options.

```bash
./tools/test-runner.sh --help
```

## Test runner examples

Usage example:

```bash
./tools/test-runner.sh --db redis --preset internal_mnesia
```

The command runs both big (feature) and small (unit) tests.

To view more examples, run:

```bash
./tools/test-runner.sh --examples
```

## Test runner completion

Test runner supports shell TAB completion.

To enable completion in bash or zsh, run:

```bash
source tools/test-runner-complete.sh
```

To view completion examples, run:

```bash
./tools/test-runner.sh --examples-complete
```

## Viewing test reports

To view test execution results, run:

```bash
./tools/test-runner.sh --show-big-reports
./tools/test-runner.sh --show-small-reports
```

## Rerun big tests

Very often we want to restart a specific suite when some test failed.

For example, some test has failed in `mam_SUITE`. The command was used to
execute tests:

```bash
./tools/test-runner.sh --skip-small-tests --db mysql --preset mysql_mnesia --skip-stop-nodes
```

`--skip-stop-nodes` is optional here, because if any big test fails, then nodes
would be still running.

We can just execute the same command, but it would rebuild nodes and start
them.

The command can be used instead:

```bash
./tools/test-runner.sh --rerun-big-tests -- mam
```

`--rerun-big-tests` expands into
`--skip-small-tests --skip-setup-db --dev-nodes --test-hosts --skip-cover --skip-preset`.

And `mam` is used to run `mam_SUITE` suite only.


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

Unit test running example using test runner:

```bash
# Run all small tests, show progress
./tools/test-runner.sh --skip-big-tests --verbose

# Run sha_SUITE without cover
./tools/test-runner.sh --skip-big-tests sha --skip-cover

# Run reload_cluster group in ejabberd_config_SUITE, show progress
./tools/test-runner.sh --skip-big-tests ejabberd_config:reload_cluster --verbose
```


# End-to-end tests (a.k.a. "big tests")

## Using test runner

Most important options are preset and database:

```bash
# Runs privacy_SUITE and private_SUITE with MySQL
./tools/test-runner.sh --skip-small-tests --db mysql --preset mysql_mnesia -- privacy private


# Runs MAM tests for MUC light with MySQL and Postgres
./tools/test-runner.sh --skip-small-tests --db mysql pgsql --preset mysql_mnesia pgsql_mnesia -- mam:rdbms_muc_light

# Runs rdbms_SUITE with MSSQL
# Inits a single MongooseIM node (works for some tests only)
# Disables cover
./tools/test-runner.sh --skip-small-tests --db mssql --preset rdbms_mssql_mnesia --test-hosts mim --dev-nodes mim1 -- rdbms --skip-cover
```

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

In shell #6:

```sh
$ cd $MONGOOSEIM/_build/reg1/rel/mongooseim
$ ./bin/mongooseimctl live
```

Back to shell #1:

```sh
$ cd big_tests/
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
tmux new-window -n reg1 '_build/fed1/rel/mongooseim/bin/mongooseimctl live'
_build/mim1/rel/mongooseim/bin/mongooseimctl started
_build/mim2/rel/mongooseim/bin/mongooseimctl started
_build/mim3/rel/mongooseim/bin/mongooseimctl started
_build/fed1/rel/mongooseim/bin/mongooseimctl started
_build/reg1/rel/mongooseim/bin/mongooseimctl started
make -C big_tests quicktest
```

Start a new tmux and paste the commands.

## Step-by-step breakdown

`make devrel` builds four server nodes, preconfigured for a wide range of features covered by end-to-end tests.

- `$MONGOOSEIM/_build/mim1/rel`, for most test SUITEs
- `$MONGOOSEIM/_build/mim*/rel`, in order to test cluster-related commands;;
- `$MONGOOSEIM/_build/fed1/rel`, in order to test XMPP federation (server to server communication, S2S).
- `$MONGOOSEIM/_build/reg1/rel`, in order to test global distribution feature.

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

In  `$MONGOOSEIM/big_tests/` we have:

```
$ tree big_tests/ -L 1 -F
big_tests/
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
# make sure we're in $MONGOOSEIM/big_tests/
cd $MONGOOSEIM/big_tests/
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

Consult the `big_tests/Makefile` to see how it works.

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
big_tests/ct_report/ct_run.[gobbledygook][datetime]/
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
Coverage statistics will be available in `big_tests/ct_report/cover.html` and `coverage` subdirectory.

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

##### Saving reports on your personal account

The uploaded files will belong to the project that you created, i.e. will not be immediately visible from your personal Google Drive UI.
To be able to upload files to your personal account, you can share the reports' directory with the project account.
First, note the ID of the project's user that you created to gain the service account credentials (e.g. `test-123@fair-smile-123456.iam.gserviceaccount.com`).
You can see this [on the Service Accounts tab of the project console](https://console.developers.google.com/iam-admin/serviceaccounts/project).
Now, create a directory on your Google Drive that will serve as the test root directory.
Go into the directory's sharing options and paste in the project's user ID, granting it write access.
Click to expand the *advanced* sharing options and note the ID of the shared directory that's displayed in the share link (e.g. if the link is `https://drive.google.com/drive/folders/1234567890abcdef?usp=sharing`, the directory's ID is `1234567890abcdef`).
Finally, set `GDRIVE_PARENT_DIR` environment variable of your Travis build to the directory ID that you noted in the previous step.

## Load testing

Alongside CI, we do also CLT (Continuous Load Testing).
We have our own load testing infrastructure, called Tide, which is triggered after every successful test run, and gives us a feedback on changes to MongooseIM performance.

Test results are publicly available on the [Hello Tide!](http://tide.erlang-solutions.com/public) page.
