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
    [get_pools_returns_pool_names,
     stats_passes_through_to_wpool_stats,
     a_global_riak_pool_is_started,
     two_distinct_redis_pools_are_started,
     generic_pools_are_started_for_all_vhosts,
     host_specific_pools_are_preseved,
     pools_for_different_tag_are_expanded_with_host_specific_config_preserved,
     global_pool_is_used_by_default].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    application:ensure_all_started(lager),
    ok = meck:new(wpool, [no_link, passthrough]),
    ok = meck:new(ejabberd_config, [no_link]),
    meck:expect(ejabberd_config, get_global_option,
                fun(hosts) -> [<<"a.com">>, <<"b.com">>, <<"c.eu">>] end),
    mongoose_wpool:ensure_started(),
    Config.

end_per_suite(Config) ->
    meck:unload(wpool),
    meck:unload(ejabberd_config),
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
    mongoose_wpool:start(x, global, z, [{workers, 1}]),

    Ref = make_ref(),

        meck:expect(wpool, stats, fun(_Name) -> Ref end),
        ?assertEqual(Ref, mongoose_wpool:stats(x, global, z)).

a_global_riak_pool_is_started(_Config) ->
    PoolName = mongoose_wpool:make_pool_name(riak, global, default),
    meck:expect(wpool, start_sup_pool, start_sup_pool_mock(PoolName)),
    [{ok, PoolName}] = mongoose_wpool:start_configured_pools([{riak, global, default,
                                                               [{workers, 12}],
                                                               [{address, "localhost"},
                                                                {port, 1805}]}]),

    H = meck_history:get_history(self(), wpool),
    F = fun({_, {wpool, start_sup_pool, [PN, Args]}, _}) -> {true, {PN, Args}};
           (_) -> false
        end,
    [{PoolName, CallArgs}] = lists:filtermap(F, H),
    ?assertEqual(12, proplists:get_value(workers, CallArgs)),
    ?assertMatch({riakc_pb_socket, _}, proplists:get_value(worker, CallArgs)),

    ok.

two_distinct_redis_pools_are_started(_C) ->
    PoolName1 = mongoose_wpool:make_pool_name(redis, global, default),
    PoolName2 = mongoose_wpool:make_pool_name(redis, global, global_dist),
    meck:expect(wpool, start_sup_pool, start_sup_pool_mock([PoolName1, PoolName2])),
    Pools = [{redis, global, default, [{workers, 2}],
              [{host, "localhost"},
               {port, 1805}]},
             {redis, global, global_dist, [{workers, 4}],
              [{host, "localhost2"},
               {port, 1806}]}],

    [{ok, PoolName1}, {ok, PoolName2}] = mongoose_wpool:start_configured_pools(Pools),

    H = meck_history:get_history(self(), wpool),
    F = fun({_, {wpool, start_sup_pool, [PN, Args]}, _}) -> {true, {PN, Args}};
           (_) -> false
        end,
    [{PoolName1, CallArgs1}, {PoolName2, CallArgs2}] = lists:filtermap(F, H),
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

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_sup_pool_mock(PoolNames) when is_list(PoolNames) ->
    fun(PN, Opts) ->
            case lists:member(PN, PoolNames) of
                true ->
                    {ok, PN}; %% we don't realy need a pid here for mocking
                _ ->
                    meck:passthrough([PN, Opts])
            end
    end;
start_sup_pool_mock(PoolName) ->
    start_sup_pool_mock([PoolName]).

cleanup_pools() ->
    lists:foreach(fun({Type, Host, Tag}) -> mongoose_wpool:stop(Type, Host, Tag) end,
                  mongoose_wpool:get_pools()).
