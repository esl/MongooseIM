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
     a_global_riak_pool_is_started].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    application:ensure_all_started(lager),
    ok = meck:new(wpool, [passthrough]),
    mongoose_wpool:ensure_started(),
    Config.

end_per_suite(Config) ->
    meck:unload(wpool),
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
    mongoose_wpool:start(a, <<"b">>, c, [{workers, 1}]),
    mongoose_wpool:start(d, <<"e">>, f, [{workers, 1}]),
    mongoose_wpool:start(g, <<"h">>, i, [{workers, 1}]),

    ?assertEqual([{a, <<"b">>, c}, {d, <<"e">>, f}, {g, <<"h">>, i}],
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
%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_sup_pool_mock(PoolName) ->
    fun(PN, _Opts) when PN =:= PoolName ->
            {ok, PN}; %% we don't realy need a pid here for mocking
       (PN, Opts) ->
            meck:passthrough([PN, Opts])
    end.


cleanup_pools() ->
    lists:foreach(fun({Type, Host, Tag}) -> mongoose_wpool:stop(Type, Host, Tag) end,
                  mongoose_wpool:get_pools()).
