Test suite for ESL's branch of ejabberd
=======================================

This repository contains test code for ejabberd/MongooseIM.
It's used mainly for [ESL's branch of ejabberd](https://github.com/esl/MongooseIM),
but one that may be reused in other projects.

## How to run tests
There are 2 ways of running the tests:
* quick (`make quicktest`) - this command tests the current configuration of the server
* full (`make test`) - this command tests all server configurations defined in test.config file - [see the section on various server configurations](#testing-various-server-configurations)

By default both commands are using the `default.spec` file where options like suites to run, config file(s) and log dir are specified. To use custom .spec file (for example to add other suites or remove the existing ones) one must add it to one of the mentioned commands:

`make quicktest TESTSPEC=full.spec`

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
