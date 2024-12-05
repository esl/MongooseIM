%% Copyright © 2018 Erlang Solutions Ltd
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

-module(mongoose_elasticsearch_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0, rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

suite() ->
     distributed_helper:require_rpc_nodes([mim],
         [{require, ejabberd_node},
          {require, ejabberd_cookie}]).

all() ->
    [{group, all}].

groups() ->
    [{all, [], all_test_cases()}].

all_test_cases() ->
    [start_and_stop_sequence].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    case get_elastic_pools() of
        [] ->
            {skip, elasticsearch_unavailable};
        [_] = Pools ->
            ok = rpc(mim(), mongoose_wpool, stop, [elastic, global, default]),
            [{elastic_pools, Pools} | Config]
    end.

end_per_suite(Config) ->
    Pools = ?config(elastic_pools, Config),
    rpc(mim(), mongoose_wpool, start_configured_pools, [Pools]).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

start_and_stop_sequence(Config) ->
    ElasticPools = ?config(elastic_pools, Config),
    rpc(mim(), mongoose_wpool, start_configured_pools, [ElasticPools]),
    ?assertMatch({ok, _}, rpc(mim(), mongoose_elasticsearch, health, [])),

    rpc(mim(), mongoose_wpool, stop, [elastic, global, default]),
    ?assertMatch({error, _}, rpc(mim(), mongoose_elasticsearch, health, [])),

    rpc(mim(), mongoose_wpool, start_configured_pools, [ElasticPools]),
    ?assertMatch({ok, _}, rpc(mim(), mongoose_elasticsearch, health, [])).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec get_elastic_pools() -> list().
get_elastic_pools() ->
    Pools = rpc(mim(), mongoose_config, get_opt, [outgoing_pools]),
    [Pool || Pool = #{type := elastic, scope := global, tag := default} <- Pools].
