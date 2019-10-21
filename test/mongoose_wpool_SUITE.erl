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
-compile(export_all).
-author('konrad.zemek@erlang-solutions.com').

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     get_pools_returns_pool_names,
     stats_passes_through_to_wpool_stats,
     a_global_riak_pool_is_started,
     two_distinct_redis_pools_are_started,
     generic_pools_are_started_for_all_vhosts,
     host_specific_pools_are_preseved,
     pools_for_different_tag_are_expanded_with_host_specific_config_preserved,
     global_pool_is_used_by_default,
     dead_pool_is_restarted,
     dead_pool_is_stopped_before_restarted,
     riak_pool_cant_be_started_with_available_worker_strategy,
     redis_pool_cant_be_started_with_available_worker_strategy
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    application:ensure_all_started(lager),
    ok = meck:new(wpool, [no_link, passthrough]),
    ok = meck:new(mongoose_wpool, [no_link, passthrough]),
    ok = meck:new(ejabberd_config, [no_link]),
    meck:expect(ejabberd_config, get_global_option,
                fun(hosts) -> [<<"a.com">>, <<"b.com">>, <<"c.eu">>] end),
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
    meck:unload(ejabberd_config),
    whereis(test_helper) ! stop,
    Config.

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

a_global_riak_pool_is_started(_Config) ->
    PoolName = mongoose_wpool:make_pool_name(riak, global, default),
    meck:expect(mongoose_wpool, start_sup_pool, start_sup_pool_mock(PoolName)),
    [{ok, PoolName}] = mongoose_wpool:start_configured_pools([{riak, global, default,
                                                               [{workers, 12}],
                                                               [{address, "localhost"},
                                                                {port, 1805}]}]),

    MgrPid = whereis(mongoose_wpool_mgr:name(riak)),
    [{PoolName, CallArgs}] = filter_calls_to_start_sup_pool(MgrPid),
    ?assertEqual(12, proplists:get_value(workers, CallArgs)),
    ?assertMatch({riakc_pb_socket, _}, proplists:get_value(worker, CallArgs)),

    ok.

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
    Pools = [{redis, global, default, [{workers, 2}],
              [{host, "localhost"},
               {port, 1805}]},
             {redis, global, global_dist, [{workers, 4}],
              [{host, "localhost2"},
               {port, 1806}]}],

    [{ok, PoolName1}, {ok, PoolName2}] = mongoose_wpool:start_configured_pools(Pools),

    MgrPid = whereis(mongoose_wpool_mgr:name(redis)),
    [{PoolName1, CallArgs1}, {PoolName2, CallArgs2}] = filter_calls_to_start_sup_pool(MgrPid),
    ?assertEqual(2, proplists:get_value(workers, CallArgs1)),
    ?assertEqual(4, proplists:get_value(workers, CallArgs2)),
    ?assertMatch({eredis_client, ["localhost", 1805 | _]}, proplists:get_value(worker, CallArgs1)),
    ?assertMatch({eredis_client, ["localhost2", 1806 | _]}, proplists:get_value(worker, CallArgs2)),

    ok.

generic_pools_are_started_for_all_vhosts(_C) ->
    Pools = [{generic, host, default, [], []}],
    StartRes = mongoose_wpool:start_configured_pools(Pools),
    ?assertMatch([_, _, _], StartRes),
    ?assertMatch([{generic, <<"a.com">>, default},
                  {generic, <<"b.com">>, default},
                  {generic, <<"c.eu">>, default}],
                 ordsets:from_list(mongoose_wpool:get_pools())).

host_specific_pools_are_preseved(_C) ->
    Pools = [{generic, host, default, [], []},
             {generic, <<"b.com">>, default, [{workers, 12}], []}],
    Expanded = mongoose_wpool:expand_pools(Pools, [<<"a.com">>, <<"b.com">>, <<"c.eu">>]),
    ?assertMatch([{generic,<<"a.com">>,default,[],[]},
                  {generic,<<"c.eu">>,default,[],[]},
                  {generic,<<"b.com">>,default,[{workers,12}],[]}], Expanded).

pools_for_different_tag_are_expanded_with_host_specific_config_preserved(_C) ->
    Pools = [{generic, host, default, [], []},
             {generic, <<"b.com">>, default, [{workers, 12}], []},
             {generic, host, other_tag, [], []}],
    Expanded = mongoose_wpool:expand_pools(Pools, [<<"a.com">>, <<"b.com">>, <<"c.eu">>]),
    ?assertMatch([{generic,<<"a.com">>,default,[],[]},
                  {generic,<<"c.eu">>,default,[],[]},
                  {generic,<<"b.com">>,default,[{workers,12}],[]},
                  {generic,<<"a.com">>,other_tag,[],[]},
                  {generic,<<"b.com">>,other_tag,[],[]},
                  {generic,<<"c.eu">>,other_tag,[],[]}], Expanded).

global_pool_is_used_by_default(_C) ->
    Pools = [{generic, global, default, [], []},
             {generic, <<"a.com">>, default, [], []}],
    StartRes = mongoose_wpool:start_configured_pools(Pools),

    meck:expect(wpool, call, fun(Name, _Req, _Strat, _Timeout) -> Name end),
    ?assertEqual(mongoose_wpool:make_pool_name(generic, <<"a.com">>, default),
                 mongoose_wpool:call(generic, <<"a.com">>, default, request)),

    ?assertEqual(mongoose_wpool:make_pool_name(generic, global, default),
                 mongoose_wpool:call(generic, <<"b.com">>, default, request)),

    ?assertEqual(mongoose_wpool:make_pool_name(generic, global, default),
                 mongoose_wpool:call(generic, global, default, request)).

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

    async_helper:wait_until(Fun, true),

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

riak_pool_cant_be_started_with_available_worker_strategy(_Config) ->
    pool_cant_be_started_with_available_worker_strategy(riak).

redis_pool_cant_be_started_with_available_worker_strategy(_Config) ->
    pool_cant_be_started_with_available_worker_strategy(redis).

pool_cant_be_started_with_available_worker_strategy(Type) ->
    Host = global,
    Tag = default,
    PoolName = mongoose_wpool:make_pool_name(Type, Host, Tag),
    meck:expect(mongoose_wpool, start_sup_pool, start_sup_pool_mock(PoolName)),
    PoolDef = [{Type, Host, Tag, [{strategy, available_worker}],
                [{address, "localhost"}, {port, 1805}]}],
    ?assertError({strategy_not_supported, Type, Host, Tag, available_worker},
                 mongoose_wpool:start_configured_pools(PoolDef)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_sup_pool_mock(PoolNames) when is_list(PoolNames) ->
    fun(Type, PN, Opts) ->
            case lists:member(PN, PoolNames) of
                true ->
                    {ok, PN}; %% we don't realy need a pid here for mocking
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
    Pools = [{generic, global, Tag,
              [{workers, Size},
               {enable_callbacks, true},
               {callbacks, [killing_workers]}],
              []}],
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
