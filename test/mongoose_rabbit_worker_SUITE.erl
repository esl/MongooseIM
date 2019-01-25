%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mongoose_rabbit_worker_SUITE).
-compile(export_all).
-author('kacper.mentel@erlang-solutions.com').

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("assert_received_match.hrl").

-define(HOST, <<"localhost">>).
-define(AMQP_MOCK_MODULES, [amqp_connection, amqp_channel]).
-define(MAX_QUEUE_LEN, 1000).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     no_request_in_worker_queue_is_lost_when_amqp_call_fails,
     worker_creates_fresh_amqp_conection_and_channel_when_amqp_call_fails,
     worker_processes_msgs_when_queue_msg_len_limit_is_not_reached,
     worker_drops_msgs_when_queue_msg_len_limit_is_reached
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    application:ensure_all_started(lager),
    mock_amqp(),
    mock_metrics(),
    Config.

end_per_suite(Config) ->
    unload_amqp(),
    meck:unload(mongoose_metrics),
    Config.

init_per_testcase(_Case, Config) ->
    WorkerOpts = [{host, ?HOST},
                  {amqp_client_opts, []},
                  {confirms, false},
                  {max_queue_len, ?MAX_QUEUE_LEN}],
    {ok, WorkerPid} = gen_server:start(mongoose_rabbit_worker, WorkerOpts, []),
    Config ++ [{worker_pid, WorkerPid}].

end_per_testcase(_Case, Config) ->
    exit(proplists:get_value(worker_pid, Config), kill),
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

no_request_in_worker_queue_is_lost_when_amqp_call_fails(Config) ->
    %% given
    Self = self(),
    Worker = proplists:get_value(worker_pid, Config),
    Ref = make_ref(),
    Lock = lock_fun(),
    SendBack = send_back_fun(),
    Exception = exception_fun(),

    %% when
    gen_server:cast(Worker, {amqp_publish, {Lock, [Ref]}, ok}),
    gen_server:cast(Worker, {amqp_publish, {Exception, [ok]}, ok}),
    gen_server:cast(Worker, {amqp_publish, {SendBack, [Self, Ref]}, ok}),
    spawn(fun() ->
                  gen_server:call(Worker, {amqp_call, {Exception, [ok]}})
          end),
    spawn(fun() ->
                  gen_server:call(Worker, {amqp_call, {SendBack, [Self, Ref]}})
          end),

    %% unlock the worker
    Worker ! Ref,

    %% then
    [?assertReceivedMatch(Ref) || _ <- lists:seq(1,2)].

worker_creates_fresh_amqp_conection_and_channel_when_amqp_call_fails(Config) ->
    %% given
    Worker = proplists:get_value(worker_pid, Config),
    Exception = exception_fun(),
    ConnectionAndChannel0 = get_worker_conn_and_chann(Worker),

    %% when amqp_publish fails
    gen_server:cast(Worker, {amqp_publish, {Exception, [ok]}, ok}),
    %% then
    ConnectionAndChannel1 = get_worker_conn_and_chann(Worker),
    ?assertNotMatch(ConnectionAndChannel0, ConnectionAndChannel1),

    %% when amqp_call fails
    gen_server:call(Worker, {amqp_call, {Exception, [ok]}}),

    %% then
    ConnectionAndChannel2 = get_worker_conn_and_chann(Worker),
    ?assertNotMatch(ConnectionAndChannel1, ConnectionAndChannel2).


worker_processes_msgs_when_queue_msg_len_limit_is_not_reached(Config) ->
    %% given
    Worker = proplists:get_value(worker_pid, Config),
    Ref = make_ref(),
    Lock = lock_fun(),
    SendBack = send_back_fun(),

    %% when
    gen_server:cast(Worker, {amqp_publish, {Lock, [Ref]}, ok}),
    gen_server:cast(Worker, {amqp_publish, {SendBack, [self(), Ref]}, ok}),
    [gen_server:cast(Worker, {amqp_publish, ok, ok})
     || _ <- lists:seq(1, ?MAX_QUEUE_LEN-1)],

    %% unlock the worker
    Worker ! Ref,

    %% then
    ?assertReceivedMatch(Ref, 100).

worker_drops_msgs_when_queue_msg_len_limit_is_reached(Config) ->
    %% given
    Worker = proplists:get_value(worker_pid, Config),
    Ref = make_ref(),
    Lock = lock_fun(),
    SendBack = send_back_fun(),

    %% when
    gen_server:cast(Worker, {amqp_publish, {Lock, [Ref]}, ok}),
    gen_server:cast(Worker, {amqp_publish, {SendBack, [self(), Ref]}, ok}),
    [gen_server:cast(Worker, {amqp_publish, ok, ok})
     || _ <- lists:seq(1, ?MAX_QUEUE_LEN+1)],

    %% unlock the worker
    Worker ! Ref,

    %% then
    ?assertError({assertReceivedMatch_failed, _},
                 ?assertReceivedMatch(Ref, 100)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

mock_amqp() ->
    [meck:new(M, [no_link, passthrough]) || M <- ?AMQP_MOCK_MODULES],
    meck:expect(amqp_connection, start, fun(_) -> {ok, random_pid()} end),
    meck:expect(amqp_connection, open_channel, fun(_) -> {ok, random_pid()} end),
    meck:expect(amqp_connection, close, fun(_) -> ok end),
    meck:expect(amqp_channel, close, fun(_) -> ok end),
    meck:expect(amqp_channel, call,
                fun(_, {F, A}) when is_function(F) -> apply(F, A);
                   (_, _) -> ok
                end),
    meck:expect(amqp_channel, call,
                fun(_, {F, A}, _) when is_function(F) -> apply(F, A);
                   (_, _, _) -> ok
                end).

mock_metrics() ->
    meck:new(mongoose_metrics, [no_link, passthrough]),
    meck:expect(mongoose_metrics, update, fun(_, _, _) -> ok end).

unload_amqp() ->
    [meck:unload(M) || M <- ?AMQP_MOCK_MODULES].

random_pid() ->
    spawn(fun() -> ok end).

get_worker_conn_and_chann(Worker) ->
    State = sys:get_state(Worker),
    {maps:get(connection, State), maps:get(channel, State)}.

%% Funs are wrapped into functions for tracing

lock_fun() ->
    fun lock/1.

lock(R) ->
    receive
        R -> unlocked
    end.

send_back_fun() ->
    fun erlang:send/2.

exception_fun() ->
    fun exception/1.

exception(_) ->
    throw(exception).
