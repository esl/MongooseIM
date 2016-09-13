Test suite for ESL's branch of ejabberd
=======================================

This directory contains test code for ejabberd/MongooseIM - previously
located in
[esl/ejabberd_tests](https://github.com/esl/ejabberd_tests/commit/913fb0a5a8c1ab753eb35c1e1b491e8572633b54).
It's used mainly for [ESL's branch of ejabberd](https://github.com/esl/MongooseIM),
but one that may be reused in other projects.

## How to run tests

There are 2 ways of running the tests:
* quick (`make quicktest`) - this command tests the current configuration of the server
* full (`make test`) - this command tests all server configurations defined in test.config file - [see the section on various server configurations](#testing-various-server-configurations)

By default both commands are using the `default.spec` file where options like suites to run, config file(s) and log dir are specified. To use a custom .spec file (for example to add other suites or remove the existing ones) one must add it to one of the mentioned commands:

`make quicktest TESTSPEC=custom.spec`

Additional erlang binaries required by custom suites can be added to the path by providing ADD_OPTS parameter to the make command. For example:

`make quicktest ADD_OPTS="-pa /full/path/to/erlcloud/ebin"`

Of course TESTSPEC and ADD_OPTS can be specified at the same time:

`make quicktest TESTSPEC=custom.spec ADD_OPTS="-pa /full/path/to/erlcloud/ebin"`

## Testing various server configurations

It is possible to run tests for various ejabberd configurations.  
In order to do this define configuration variables in `test.config` file, for example:

```
{ejabberd_configs, [
    {internal_mnesia,
        [{sm_backend, "{mnesia, []}"},
        {auth_method, "internal"}]},
    {internal_redis,
        [{sm_backend, "{redis, [{pool_size, 3}, {worker_config, [{host, \"localhost\"}, {port, 6379}]}]}"},
        {auth_method, "internal"}]}
    ]}.
```
For each configuration the given variables merged with values from `node1_vars.config` 
will be overlaid with the original `ejabberd.cfg` file and ejabberd will be restarted.


If there is no `ejabberd_configs` entry in the config file the test will run ordinarily with running ejabberd configuration.

## Changing server configuration in suites ##

Sometimes it is required to change server configuration for a particular
test or test suite. [ejabberd_node_utils](tests/ejabberd_node_utils.erl)
module introduces a set of functions allowing such server manipulation.

For example to make server require `starttls` and utilize given certificate,
use the module as follows:
```erlang
init_per_suite(Config) ->
    setup_ejabberd_node(Config).

end_per_suite(Config) ->
    restore_ejabberd_node(Config).

...

setup_ejabberd_node(Config0) ->
    Config1 = ejabberd_node_utils:init(Config0),
    ejabberd_node_utils:backup_config_file(Config1),
    assert_cert_file_exists(),
    make_ejabberd_node_require_starttls(Config1),
    ejabberd_node_utils:restart_application(ejabberd),
    Config1.

restore_ejabberd_node(Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(ejabberd).

assert_cert_file_exists() ->
    ejabberd_node_utils:file_exists(?CERT_FILE) orelse
        ct:fail("cert file ~s not exists", [?CERT_FILE]).

make_ejabberd_node_require_starttls(Config) ->
    ejabberd_node_utils:modify_config_file([mk_value_for_tls_config_pattern()],
                                           Config).

mk_value_for_tls_config_pattern() ->
    {tls_config, "{certfile, \"" ++ ?CERT_FILE ++ "\"}, starttls_required,"}.
```

The above snippet backups current server's config file, asserts that
the expected certificate files exists and then produces new config file.
At the end it restart the server so that the new configuration can be applied.
In the `end_per_suite` previously backuped config file is restored and
the server is restarted again. See [ejabberd_node_utils](tests/ejabberd_node_utils.erl)
for more information.

