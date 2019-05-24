# Basic IQ Handler

XMPP stands for Extensible Messaging and Presence Protocol.
One way the protocol can be extended is by defining new types of queries,
or _IQs_, that XMPP entities should be able to handle.
It's usual that a XEP defining some XMPP extension contains some new type of IQ.
IQs can also be used to implement custom features - required
in a particular problem domain - but not defined by any official XEP.

This tutorial will show you how to add and test a simple module with an IQ
handler to MongooseIM.
`gen_iq_handler` module provides functionality for registering IQ
handlers for specific namespaces.


## Clone & build

See [How-to-build](../../user-guide/How-to-build) for details on building MongooseIM
from source code.


## Create a module & add a basic IQ handler

Go to `src/` and create a basic module implementing the `gen_mod` behaviour.
In `start/2` register the IQ handler with a specified namespace, type
(IQ processing policy), and function which will handle the incoming IQ stanza.
In `stop/1` remove the registered handler.
Implement the function for handler:

* If the incoming IQ stanza is of type `get` or `set` it will be
  returned with the type set to `result`.

* If the server doesn't recognise the hostname, the returning stanza
  will be of type `error`.

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


### IQ processing policies

The server may use one of the following strategies to handle incoming stanzas:

* `no_queue` registers a new IQ  handler, which will be called in the
  context of a process serving the connection on which the IQ arrives 
* `one_queue` spawns a new process by which the incoming IQ stanzas will
  be handled
* `{queues, N}` spawns **N** processes. Every incoming stanza will be then
  handled by one of those processes
* `parallel` registers the handler without spawning a new process, a new process
  will be spawned for each incoming stanza


## Test your handler

Go to `big_tests/tests` and create a test suite for your handler.
Implement the test case for success and failure.
We will register two users, which are predefined in `$REPO/big_tests/test.config`:

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

Our IQ handler will be enabled only for one domain, `localhost`.
After sending an IQ stanza to `alice` we should get a result, but as our IQ
handler is not enabled for `localhost.bis` domain, we should get an error.

```erlang
-module(mod_iq_example_SUITE).

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
    Domain = ct:get_config({hosts, mim, domain}),
    dynamic_modules:start(Domain, mod_iq_example, [no_opts]),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    Domain = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Domain, mod_iq_example),
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
    escalus:story(Config, [{alice_bis, 1}], fun(Alice) ->
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


## Run it

Compile & generate releases for testing purposes according to
[How-to-build](../../user-guide/How-to-build/#building-the-testing-target-and-running-tests).
Go to `$REPO/_build/mim1/rel/mongooseim` and start one MongooseIM node.

```bash
$ bin/mongooseim live
```
Open up a new terminal window, go to `$REPO` and use the [test runner](Testing-MongooseIM).
Run single suite with the already started `mim1` node.

```bash
source tools/test-runner-complete.sh
test-runner.sh --rerun-big-tests -- mod_iq_example
```
