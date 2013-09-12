Test suite for ESL's branch of ejabberd
=======================================

This repository contains test code for ejabberd.
It's used mainly for [ESL's branch of ejabberd](https://github.com/esl/ejabberd),
but one that may be reused in other projects.

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
