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
     stats_passes_through_to_wpool_stats].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    mongoose_wpool:ensure_started(),
    Config.

end_per_suite(Config) ->
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
    ok = meck:new(wpool),

    try
        meck:expect(wpool, stats, fun(_Name) -> Ref end),
        ?assertEqual(Ref, mongoose_wpool:stats(x, global, z))
    after
        meck:unload(wpool)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

cleanup_pools() ->
    lists:foreach(fun({Type, Host, Tag}) -> mongoose_wpool:stop(Type, Host, Tag) end,
                  mongoose_wpool:get_pools()).
