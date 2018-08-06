## Basic IQ Handler
This tutorial will show you how to add & test a simple module with an IQ
handler to MongooseIM.
`gen_iq_handler` module provides functionality for registering IQ
handlers for specific namespaces.


### Clone & build

See [How-to-build](../../user-guide/How-to-build) for details on building MongooseIM
from source code.


### Create module & add basic IQ handler

* Go to `src` and create a basic module implementing `gen_mod` behaviour
* In `start/2` register the IQ handler with specified namespace, type
  (processing iq policy) and function which will handle the incoming IQ stanza
* In `stop/1` remove the registered handler
* Implement the function for handler:
    * If the incoming IQ stanza is of type `get` or `set` it will be
      returned with the type set to `result`
    * If the server doesn't recognise the hostname, the returning stanza
      will be of type `error`

See [Server Rules for Processing XML Stanzas](https://tools.ietf.org/html/rfc6120#section-10) for more
detailed information on the topic.

```erlang
-module(mod_iq_example).
-behaviour(gen_mod).

-include("mongoose.hrl").
-include("jlib.hrl").

%% gen_mod callbacks
-export([start/2, stop/1]).

%% IQ handlers
-export([process_iq/4]).

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, <<"erlang-solutions.com:example">>,
                                  ?MODULE, process_iq, no_queue).
stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, <<"erlang-solutions.com:example">>).

process_iq(_From, _To, Acc, IQ) ->
    IQRes = IQ#iq{type = result},
    ?INFO_MSG("event=example_handler request=~w response=~w", 
                [IQ, IQRes]),
    {Acc, IQRes}.
```


### Test your handler

* Go to `big_tests/tests` and create a test suite for your handler
* Implement the test case for success and failure. Our IQ handler is
  enabled only for one domain, which is `localhost`. We will register two
users, which are predefined in `$REPO/big_tests/test.config`:

```erlang
{alice, [
    {username, <<"alicE">>},
    {server, <<"localhost">>},
    {password, <<"matygrysa">>}]},
{alice_bis, [
    {username, <<"alicE">>},
    {server, <<"localhost.bis">>},
    {host, <<"localhost">>},
    {password, <<"matygrysa">>}]},
```
After sending IQ stanza to `alice` we should get a result, but our IQ
handler is not enabled for `localhost.bis` domain, so we should get an
error.

```erlang
-modul(mod_iq_example_SUITE).

-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Tests
-export([should_return_result/1,
         should_return_error/1]).

-include_lib("exml/include/exml.hrl").

-define(EXAMPLE_NS, <<"erlang-solutions.com:example">>).
-define(USERS, [alice, alice_bis]).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mod_iq_example}].

groups() ->
    G = [{mod_iq_example, [], [should_return_result,
                               should_return_error]}],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    escalus:create_users(Config, ?USERS).

end_per_group(_, Config) ->
    escalus:delete_users(Config, ?USERS).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

should_return_result(Config) ->
    %% given
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        %% when sending a request
        Req = escalus_stanza:iq_get(?EXAMPLE_NS, [#xmlel{name = <<"example">>}]),
        ct:pal("req: ~p", [Req]),
        escalus:send(Alice, Req),
        %% then we should get a result
        Res = escalus:wait_for_stanza(Alice),
        ct:pal("res: ~p", [Res]),
        escalus:assert(is_iq, [<<"result">>, ?EXAMPLE_NS], Res)
    end).

should_return_error(Config) ->
    %% given
    escalus:story(Config, [{alice_invalid_domain, 1}], fun(Alice) ->
        %% when sending a request with unregistered server
        Req = escalus_stanza:iq_get(?EXAMPLE_NS, [#xmlel{name = <<"example">>}]),
        ct:pal("req: ~p", [Req]),
        escalus:send(Alice, Req),
        %% then we should get an error
        Res = escalus:wait_for_stanza(Alice),
        ct:pal("res: ~p", [Res]),
        escalus:assert(is_iq, [<<"error">>, ?EXAMPLE_NS], Res),
        escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Res)
    end).
```


### Run

Compile & generate releases for testing purposes according to
[How-to-build](../../user-guide/How-to-build/#building-the-testing-target-and-running-tests).
Go to `$REPO/_build/mim1/rel/mongooseim` and start one MongooseIM node.

```bash
$ bin/mongooseim live
```

The module has to be compiled and then started in order to test it.

```erlang
(mongooseim@localhost)> c(mod_iq_example).
Recompiling /Users/zofiapolkowska/MongooseIM/_build/mim1/lib/mongooseim/src/mod_iq_example.erl
{ok,mod_iq_example}
(mongooseim@localhost)1> gen_mod:is_loaded(<<"localhost">>, mod_iq_example).
false
(mongooseim@localhost)> gen_mod:start_module(<<"localhost">>, mod_iq_example, [no_opts]).
{ok,{register_iq_handler,<<"localhost">>,
                         <<"erlang-solutions.com:example">>,mod_iq_example,
                         process_iq,no_queue}}
(mongooseim@localhost)1> gen_mod:is_loaded(<<"localhost">>, mod_iq_example).
true
```

Open up a new terminal window, go to `$REPO` and use [Test runner](Testing-MongooseIM).
Run single suite with already started mim1 node.

```bash
source tools/test-runner-complete.sh
test-runner.sh --rerun-big-tests -- mod_iq_example
```
