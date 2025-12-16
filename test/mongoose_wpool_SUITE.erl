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

-module(mongoose_wpool_SUITE).
-compile([export_all, nowarn_export_all]).
-author('konrad.zemek@erlang-solutions.com').

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(config_parser_helper, [config/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     get_pools_returns_pool_names,
     stats_passes_through_to_wpool_stats,
     two_distinct_redis_pools_are_started,
     generic_pools_are_started_for_all_vhosts,
     host_specific_pools_are_preserved,
     pools_for_different_tag_are_expanded_with_host_specific_config_preserved,
     global_pool_is_used_by_default,
     request_behaves_as_gen_server_send_request,
     dead_pool_is_restarted,
     dead_pool_is_stopped_before_restarted,
     redis_pool_cant_be_started_with_available_worker_strategy,
     cassandra_prepare_opts,
     max_worker_queue_len_with_one_worker,
     max_worker_queue_len_with_two_workers
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = meck:new(wpool, [no_link, passthrough]),
    ok = meck:new(mongoose_wpool, [no_link, passthrough]),
    mongoose_config:set_opts(opts()),
    Self = self(),
    spawn(fun() ->
                  register(test_helper, self()),
                  mongoose_wpool:ensure_started(),
                  Self ! ready,
                  receive stop -> ok end
          end),
    receive ready -> ok end,
    Config.

end_per_suite(Config) ->
    meck:unload(wpool),
    whereis(test_helper) ! stop,
    mongoose_config:erase_opts(),
    Config.

opts() ->
    #{hosts => [<<"a.com">>, <<"b.com">>, <<"c.eu">>], host_types => []}.

init_per_testcase(_Case, Config) ->
    cleanup_pools(),
    Config.

end_per_testcase(_Case, Config) ->
    cleanup_pools(),
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

get_pools_returns_pool_names(_Config) ->
    mongoose_wpool:start(generic, <<"b">>, c, [{workers, 1}]),
    mongoose_wpool:start(generic, <<"e">>, f, [{workers, 1}]),
    mongoose_wpool:start(generic, <<"h">>, i, [{workers, 1}]),

    ?assertEqual([{generic, <<"b">>, c}, {generic, <<"e">>, f}, {generic, <<"h">>, i}],
                 ordsets:from_list(mongoose_wpool:get_pools())).

stats_passes_through_to_wpool_stats(_Config) ->
    mongoose_wpool:start(generic, global, z, [{workers, 1}]),
    Ref = make_ref(),
    meck:expect(wpool, stats, fun(_Name) -> Ref end),
    ?assertEqual(Ref, mongoose_wpool:stats(generic, global, z)).

filter_calls_to_start_sup_pool(Pid) ->
    H = meck_history:get_history(Pid, mongoose_wpool),
    F = fun({_, {mongoose_wpool, start_sup_pool, [_, PN, Args]}, _}) -> {true, {PN, Args}};
           (_) -> false
        end,
    lists:filtermap(F, H).

two_distinct_redis_pools_are_started(_C) ->
    PoolName1 = mongoose_wpool:make_pool_name(redis, global, default),
    PoolName2 = mongoose_wpool:make_pool_name(redis, global, global_dist),
    meck:expect(mongoose_wpool, start_sup_pool, start_sup_pool_mock([PoolName1, PoolName2])),
    Pools = [#{type => redis, scope => global, tag => default, opts => #{workers => 2},
               conn_opts => #{host => "localhost",
                              port => 1805,
                              database => 0,
                              password => ""}},
             #{type => redis, scope => global, tag => global_dist, opts => #{workers => 4},
               conn_opts => #{host => "localhost2",
                              port => 1806,
                              database => 0,
                              password => ""}}],

    [{ok, PoolName1}, {ok, PoolName2}] = mongoose_wpool:start_configured_pools(Pools),

    MgrPid = whereis(mongoose_wpool_mgr:name(redis)),
    [{PoolName1, CallArgs1}, {PoolName2, CallArgs2}] = filter_calls_to_start_sup_pool(MgrPid),
    ?assertEqual(2, proplists:get_value(workers, CallArgs1)),
    ?assertEqual(4, proplists:get_value(workers, CallArgs2)),
    {eredis_client, Props1} = proplists:get_value(worker, CallArgs1),
    {eredis_client, Props2} = proplists:get_value(worker, CallArgs2),
    ?assertMatch(#{host := "localhost", port := 1805}, proplists:to_map(Props1)),
    ?assertMatch(#{host := "localhost2", port := 1806}, proplists:to_map(Props2)).

generic_pools_are_started_for_all_vhosts(_C) ->
    Pools = [#{type => generic, scope => host_type, tag => default, opts => #{}, conn_opts => #{}}],
    StartRes = mongoose_wpool:start_configured_pools(Pools),
    ?assertMatch([_, _, _], StartRes),
    ?assertMatch([{generic, <<"a.com">>, default},
                  {generic, <<"b.com">>, default},
                  {generic, <<"c.eu">>, default}],
                 ordsets:from_list(mongoose_wpool:get_pools())).

host_specific_pools_are_preserved(_C) ->
    Pools = [#{type => generic, scope => host_type, tag => default, opts => #{}, conn_opts => #{}}],
    HostSpecific = [#{type => generic, scope => <<"b.com">>, tag => default,
                      opts => #{workers => 12}, conn_opts => #{}}],
    Expanded = mongoose_wpool:expand_pools(
                 Pools, HostSpecific, [<<"a.com">>, <<"b.com">>, <<"c.eu">>]),
    Expected = lists:sort([#{type => generic, host_type => <<"a.com">>, tag => default,
                             opts => [], conn_opts => #{}},
                           #{type => generic, host_type => <<"c.eu">>, tag => default,
                             opts => [], conn_opts => #{}},
                           #{type => generic, host_type => <<"b.com">>, tag => default,
                             opts => [{workers, 12}], conn_opts => #{}}]),
    ?assertMatch(Expected, lists:sort(Expanded)).

pools_for_different_tag_are_expanded_with_host_specific_config_preserved(_C) ->
    Pools = [#{type => generic, scope => host_type, tag => default, opts => #{}, conn_opts => #{}},
             #{type => generic, scope => host_type, tag => other_tag,
               opts => #{}, conn_opts => #{}}],
    HostSpecific = [#{type => generic, scope => <<"b.com">>, tag => default,
                      opts => #{workers => 12}, conn_opts => #{}}],
    Expanded = mongoose_wpool:expand_pools(
                 Pools, HostSpecific, [<<"a.com">>, <<"b.com">>, <<"c.eu">>]),
    Expected = lists:sort([#{type => generic, host_type => <<"a.com">>, tag => default,
                             opts => [], conn_opts => #{}},
                           #{type => generic, host_type => <<"c.eu">>, tag => default,
                             opts => [], conn_opts => #{}},
                           #{type => generic, host_type => <<"b.com">>, tag => default,
                             opts => [{workers, 12}], conn_opts => #{}},
                           #{type => generic, host_type => <<"a.com">>, tag => other_tag,
                             opts => [], conn_opts => #{}},
                           #{type => generic, host_type => <<"b.com">>, tag => other_tag,
                             opts => [], conn_opts => #{}},
                           #{type => generic, host_type => <<"c.eu">>, tag => other_tag,
                             opts => [], conn_opts => #{}}]),
    ?assertMatch(Expected, lists:sort(Expanded)).

global_pool_is_used_by_default(_C) ->
    Pools = [#{type => generic, scope => global, tag => default, opts => #{}, conn_opts => #{}},
             #{type => generic, scope => <<"a.com">>, tag => default,
               opts => #{}, conn_opts => #{}}],
    StartRes = mongoose_wpool:start_configured_pools(Pools),
    ?assertMatch([{ok, _}, {ok, _}], StartRes),
    meck:expect(wpool, call, fun(Name, _Req, _Strat, _Timeout) -> Name end),
    ?assertEqual(mongoose_wpool:make_pool_name(generic, <<"a.com">>, default),
                 mongoose_wpool:call(generic, <<"a.com">>, default, request)),

    ?assertEqual(mongoose_wpool:make_pool_name(generic, global, default),
                 mongoose_wpool:call(generic, <<"b.com">>, default, request)),

    ?assertEqual(mongoose_wpool:make_pool_name(generic, global, default),
                 mongoose_wpool:call(generic, global, default, request)).

request_behaves_as_gen_server_send_request(_C) ->
    Pools = [#{type => generic, scope => global, tag => default, opts => #{}, conn_opts => #{}} ],
    StartRes = mongoose_wpool:start_configured_pools(Pools),
    ?assertMatch([{ok, _}], StartRes),
    Req1 = mongoose_wpool:send_request(generic, {?MODULE, echo, [send_request]}),
    ?assertEqual({reply, {ok, send_request}}, gen_server:wait_response(Req1, 5000)).

dead_pool_is_restarted(_C) ->
    Size = 3,
    {PoolName, KillingSwitch} = start_killable_pool(Size, kill_and_restart),
    %% set the switch to kill every new worker
    set_killing_switch(KillingSwitch, true),
    Pids = [whereis(wpool_pool:worker_name(PoolName, I)) || I <- lists:seq(1, Size)],
    %% kill existing workers so they will be restarted
    [erlang:exit(Pid, kill) || Pid <- Pids, Pid =/= undefined],

    wait_until_pool_is_dead(PoolName),
    %% set the switch to stop killing workers
    set_killing_switch(KillingSwitch, false),

    %% wait until the pool is restarted by the manager
    Fun = fun() ->
                  case erlang:whereis(PoolName) of
                      undefined -> false;
                      _ -> true
                  end
          end,

    wait_helper:wait_until(Fun, true),

    meck:unload(killing_workers).

dead_pool_is_stopped_before_restarted(_C) ->
    Size = 3,
    Tag = kill_stop_before_restart,
    {PoolName, KillingSwitch} = start_killable_pool(Size, Tag),
    %% set the switch to kill every new worker
    set_killing_switch(KillingSwitch, true),
    %% kill existing workers so they will be restarted
    [erlang:exit(whereis(wpool_pool:worker_name(PoolName, I)), kill) ||
     I <- lists:seq(1, Size)],

    wait_until_pool_is_dead(PoolName),
    %% stop the pool before it's restarted
    mongoose_wpool:stop(generic, global, Tag),
    %% set the switch to stop killing workers
    set_killing_switch(KillingSwitch, false),
    %% wait 4s (the restart attempt will happen after 2s)
    %% and check if the pool is started, it should not be
    timer:sleep(timer:seconds(4)),
    ?assertEqual(undefined, erlang:whereis(PoolName)),
    meck:unload(killing_workers).

%% --- available_worker strategy is banned for some backends --

redis_pool_cant_be_started_with_available_worker_strategy(_Config) ->
    Type = redis,
    Host = global,
    Tag = default,
    PoolName = mongoose_wpool:make_pool_name(Type, Host, Tag),
    meck:expect(mongoose_wpool, start_sup_pool, start_sup_pool_mock(PoolName)),
    PoolDef = [#{type => Type, scope => Host, tag => Tag, opts => #{strategy => available_worker},
                 conn_opts => #{address => "localhost", port => 1805}}],
    ?assertError({strategy_not_supported, Type, Host, Tag, available_worker},
                 mongoose_wpool:start_configured_pools(PoolDef)).

cassandra_prepare_opts(_Config) ->
    %% Check that we pass auth options in the correct format to the Cassandra driver
    AuthCfg = #{auth => #{plain => #{username => <<"user">>, password => <<"password">>}}},
    ?assertEqual([{auth, {cqerl_auth_plain_handler, [{<<"user">>, <<"password">>}]}},
                  {tcp_opts, [{keepalive, true}]}],
                  mongoose_wpool_cassandra:prepare_cqerl_opts(AuthCfg)).

max_worker_queue_len_with_one_worker(_C) ->
    Pools = [config([outgoing_pools, generic, default],
                    #{opts => #{workers => 1, max_worker_queue_len => 1}})],
    StartRes = mongoose_wpool:start_configured_pools(Pools),
    [W] = get_workers(generic, global, default),
    ?assertMatch([{ok, _}], StartRes),
    ?assertEqual(ok, mongoose_wpool:cast(generic, global, default, {?MODULE, wait, [self()]})),
    ?assertEqual(ok, mongoose_wpool:cast(generic, global, default, {?MODULE, wait, [self()]})),
    get_waiting_msg(W), % W: waiting, queue len: 1
    ?assertExit(no_available_workers,
                mongoose_wpool:cast(generic, global, default, {erlang, send, [self(), {msg, 1}]})),
    continue(W),
    get_waiting_msg(W), % W: waiting, queue len: 0
    ?assertEqual(ok,
                 mongoose_wpool:cast(generic, global, default, {erlang, send, [self(), {msg, 2}]})),
    continue(W), % W: free
    ?assertEqual(2, get_msg()).

max_worker_queue_len_with_two_workers(_C) ->
    Pools = [config([outgoing_pools, generic, default],
                    #{opts => #{workers => 2, max_worker_queue_len => 1}})],
    StartRes = mongoose_wpool:start_configured_pools(Pools),
    ?assertMatch([{ok, _}], StartRes),
    Workers = get_workers(generic, global, default),

    ?assertEqual(ok, mongoose_wpool:cast(generic, global, default, {?MODULE, wait, [self()]})),
    W1 = get_waiting_msg(), % W1: waiting, queue len: 1
    ?assertEqual(ok, mongoose_wpool:cast(generic, global, default, {?MODULE, wait, [self()]})),
    ?assertEqual(ok, mongoose_wpool:cast(generic, global, default, {?MODULE, wait, [self()]})),
    W2 = get_waiting_msg(), % W1: waiting, queue len: 1;  W2: waiting, queue len: 0
    ?assertEqual(Workers -- [W1], [W2]),
    ?assertEqual(ok, mongoose_wpool:cast(generic, global, default, {?MODULE, wait, [self()]})),
    %% W1 & W2: waiting, queue len: 1
    ?assertExit(no_available_workers,
                mongoose_wpool:cast(generic, global, default, {erlang, send, [self(), {msg, 1}]})),
    continue(W1),
    get_waiting_msg(W1), % W1: waiting, queue len: 0
    ?assertEqual(ok,
                 mongoose_wpool:cast(generic, global, default, {erlang, send, [self(), {msg, 2}]})),
    continue(W1), % W1: free
    ?assertEqual(2, get_msg()).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_sup_pool_mock(PoolNames) when is_list(PoolNames) ->
    fun(Type, PN, Opts) ->
            case lists:member(PN, PoolNames) of
                true ->
                    {ok, PN}; %% we don't really need a pid here for mocking
                _ ->
                    meck:passthrough([Type, PN, Opts])
            end
    end;
start_sup_pool_mock(PoolName) ->
    start_sup_pool_mock([PoolName]).

cleanup_pools() ->
    lists:foreach(fun({Type, Host, Tag}) -> mongoose_wpool:stop(Type, Host, Tag) end,
                  mongoose_wpool:get_pools()).

start_killable_pool(Size, Tag) ->
    KillingSwitch = ets:new(killing_switch, [public]),
    ets:insert(KillingSwitch, {kill_worker, false}),
    meck:new(killing_workers, [non_strict]),
    meck:expect(killing_workers, handle_worker_creation, kill_worker_fun(KillingSwitch)),
    Pools = [#{type => generic, scope => global, tag => Tag,
               opts => #{workers => Size,
                         enable_callbacks => true,
                         callbacks => [killing_workers]},
               conn_opts => #{}}],
    [{ok, _Pid}] = mongoose_wpool:start_configured_pools(Pools),
    PoolName = mongoose_wpool:make_pool_name(generic, global, Tag),
    {PoolName, KillingSwitch}.

kill_worker_fun(KillingSwitch) ->
    fun(AWorkerName) ->
            case ets:lookup_element(KillingSwitch, kill_worker, 2) of
                true ->
                    ct:pal("I'll be killing ~p", [AWorkerName]),
                    erlang:exit(whereis(AWorkerName), kill);
                _ ->
                    ok
            end
    end.

wait_until_pool_is_dead(PoolName) ->
    %% Wait until the pool is killed due to too many restarts
    Pid = whereis(PoolName),
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _} ->
            ok
    after
        timer:minutes(3) ->
            ct:fail("pool not stopped")
    end.

set_killing_switch(KillingSwitch, Value) ->
    ets:update_element(KillingSwitch, kill_worker, {2, Value}).

echo(Val) -> Val.

wait(Caller) ->
    Caller ! {waiting, self()},
    receive {continue, Caller} -> ok end.

get_waiting_msg(Worker) ->
    receive {waiting, Worker} ->
            ok
    after 5000 ->
            ct:fail("Timeout: 'waiting' not received from worker ~p", [Worker])
    end.

get_waiting_msg() ->
    receive {waiting, Worker} ->
            Worker
    after 5000 ->
            ct:fail("Timeout: 'waiting' not received from any worker")
    end.

continue(Worker) ->
    Worker ! {continue, self()}.

get_msg() ->
    receive {msg, Id} ->
            Id
    after 5000 ->
            ct:fail("Timeout: 'msg' not received from worker")
    end.

get_workers(Type, Scope, Tag) ->
    WorkerNames = wpool:get_workers(mongoose_wpool:make_pool_name(Type, Scope, Tag)),
    lists:map(fun whereis/1, WorkerNames).
