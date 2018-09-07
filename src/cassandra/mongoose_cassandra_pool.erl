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
-module(mongoose_cassandra_pool).
-author('rafal.slota@erlang-solutions.com').

-include("mongoose_logger.hrl").


%% ====================================================================
%% Exports
%% ====================================================================

%% Module callbacks

%% API
-export([init/1, shutdown/1, all/0]).
-export([call_query/4, cast_query/3]).

%% Types

%% ====================================================================
%% Module API
%% ====================================================================

init({PoolName, PoolConfig}) ->
    init({PoolName, 20, PoolConfig});
init({PoolName, PoolSize, PoolConfig}) ->
    ExtConfig = extend_config(PoolConfig),
    application:set_env(cqerl, num_clients, PoolSize),
    Res = cqerl_cluster:add_nodes(PoolName, proplists:get_value(servers, ExtConfig), ExtConfig),
    case lists:keyfind(error, 1, Res) of
        false ->
            ok;
        _ ->
            erlang:throw({not_all_nodes_added, Res})
    end,
    {ok, _} = wpool:start_sup_pool(wpool_name(PoolName, query), [
        {workers, PoolSize},
        {worker, {mongoose_cassandra_worker, [PoolName]}}
    ]),
    ok.

shutdown(PoolName) ->
    wpool:stop_sup_pool(wpool_name(PoolName, query)).

extend_config(PoolConfig) ->
    Defaults = #{
        servers     => [{"localhost", 9042}],
        tcp_opts    => [{keepalive, true}],
        keyspace    => mongooseim
    },

    ConfigMap = maps:merge(Defaults, maps:from_list(PoolConfig)),
    maps:to_list(ConfigMap).

all() ->
    case ejabberd_config:get_local_option(cassandra_servers) of
        undefined ->
            [];
        Pools ->
            Pools
    end.

call_query(PoolName, ContextId, Call, Timeout) ->
    wpool:call(wpool_name(PoolName, query), Call, worker_strategy(ContextId), Timeout).

cast_query(PoolName, ContextId, Call) ->
    wpool:cast(wpool_name(PoolName, query), Call, worker_strategy(ContextId)).

wpool_name(PoolName, Type) ->
    NameStr = atom_to_list(?MODULE) ++ "_" ++ atom_to_list(PoolName) ++ "_" ++ atom_to_list(Type),
    list_to_atom(NameStr).

worker_strategy(undefined) ->
    best_worker;
worker_strategy(ContextId) ->
    {hash_worker, ContextId}.
