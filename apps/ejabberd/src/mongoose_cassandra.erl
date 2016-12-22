%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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
-module(mongoose_cassandra).
-author("Rafal Slota").

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("cqerl/include/cqerl.hrl").

%% ====================================================================
%% Exports
%% ====================================================================

%% Module callbacks
-export([start/0, stop/0]).

%% API
-export([cql_read/5, cql_write/5]).
-export([now_timestamp/0]).

%% Test queries
-export([prepared_queries/0, total_count_query_cql/1, test_query_sql/0]).
-export([test_query/1, test_query/2, total_count_query/2]).

%% Callbacks definitions
-callback prepared_queries() -> list({term(), string()}).


%% ====================================================================
%% Module API
%% ====================================================================

start() ->
    case ejabberd_config:get_local_option(cassandra_servers) of
        undefined ->
            ignore;
        Pools ->
            cqerl_app:start(normal, []),
            [init_pool(Pool) || Pool <- Pools]
    end.

-spec stop() -> _.
stop() ->
    cqerl_app:stop(undefined).


%% Module helpers
init_pool({PoolName, PoolConfig}) ->
    init_pool({PoolName, 20, PoolConfig});
init_pool({PoolName, PoolSize, PoolConfig}) ->
    ExtConfig = extend_config(PoolConfig),
    application:set_env(cqerl, num_clients, PoolSize),
    application:set_env(cqerl, query_timeout, timer:seconds(5)),
    cqerl_cluster:add_nodes(PoolName, proplists:get_value(servers, ExtConfig), ExtConfig).

extend_config(PoolConfig) ->
    PoolConfig
    ++ [{servers, [{"localhost", 9042}]},
%%            {tcp_opts, [{connect_timeout, 4000}]},
        {keyspace, mongooseim}].


%% ====================================================================
%% Cassandra API
%% ====================================================================

%% @doc Return timestamp in nanoseconds
now_timestamp() ->
    now_to_usec(os:timestamp()).

-spec now_to_usec(erlang:timestamp()) -> non_neg_integer().
now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.

-spec cql_write(PoolName :: atom(), UserJID :: jid(), Module :: atom(),
                QueryName :: atom() |{atom(), atom()}, Rows :: [proplists:proplist()]) ->
    ok | {error, Reason :: any()}.
cql_write(PoolName, _UserJID, Module, QueryName, Rows)  ->
    QueryStr = proplists:get_value(QueryName, Module:prepared_queries()),
    Query = #cql_query{statement = QueryStr},

    {ok, Client} = cqerl:get_client(PoolName),
    case cql_write_rows(Client, Query, Rows, 20, 3) of
        ok ->
            ok;
        Error ->
            Error
    end.

-spec cql_read(PoolName :: atom(), UserJID :: jid(), Module :: atom(),
                QueryName :: atom() |{atom(), atom()}, Params :: proplists:proplist()) ->
    {ok, Rows :: [proplists:proplist()]} | {error, Reason :: any()}.
cql_read(PoolName, _UserJID, Module, QueryName, Params)  ->
    cql_read(PoolName, _UserJID, Module, QueryName, Params, 30).
cql_read(PoolName, _UserJID, Module, QueryName, Params, 0)  ->
    error({query_timeout, QueryName, Params});
cql_read(PoolName, _UserJID, Module, QueryName, Params, TryCount)  ->
    QueryStr = proplists:get_value(QueryName, Module:prepared_queries()),
    Query = #cql_query{statement = QueryStr, values = Params},

    {ok, Client} = cqerl:get_client(PoolName),
    Tag = cqerl:send_query(Client, Query),
    Start = os:system_time(millisecond),
    Res = receive
        {result, Tag, Result} ->
            cql_read_pages(Result, [], TryCount);
        {error, Tag, {16#1200, _, _}} ->
            cql_read(PoolName, _UserJID, Module, QueryName, Params, TryCount - 1);
        {error, Tag, Reason} ->
            {error, Reason}
    after timer:seconds(10) ->
        cql_read(PoolName, _UserJID, Module, QueryName, Params, TryCount - 1)
    end,
    End = os:system_time(millisecond),
    ?WARNING_MSG("TIME ~p for ~p", [End-Start, {QueryStr, Params}]),
    Res.


flush() ->
    receive
        M ->
            M,
            ?WARNING_MSG("FLUSH ~p", [M]),
            flush()
    after 0 ->
        ok
    end.

cql_read_pages(Result, _Acc0, 0) ->
    error({query_page_timeout, Result});
cql_read_pages(Result, Acc0, Retry) ->
    Acc = [cqerl:all_rows(Result) | Acc0],
    case cqerl:has_more_pages(Result) of
        true ->
            Tag = cqerl:fetch_more_async(Result),
            receive
                {result, Tag, NextResult} ->
                    cql_read_pages(NextResult, Acc, Retry);
                {error, Tag, {16#1200, _, _}} ->
                    timer:sleep(crypto:rand_uniform(50, 500)),
                    cql_read_pages(Result, Acc0, Retry - 1);
                {error, Tag, Reason} ->
                    {error, Reason}
            after timer:seconds(10) ->
                cql_read_pages(Result, Acc0, Retry - 1)
            end;
        false ->
            {ok, lists:concat(lists:reverse(Acc))}
    end.


cql_write_rows(_, _, [], _, _) ->
    ok;

cql_write_rows(_, WriteQuery, _, _, 0) ->
    error({query_batch_timeout, WriteQuery});
cql_write_rows(Client, WriteQuery, Rows, BatchSize, TryCount) ->
    {NewRows, Tail} = lists:split(min(BatchSize, length(Rows)), Rows),
    Tag = cqerl:send_query(Client, #cql_query_batch{
        mode = unlogged,
        queries = [WriteQuery#cql_query{values = NewRow}
                   || NewRow <- NewRows]
    }),
    WriteTimeout = timer:minutes(5) + crypto:rand_uniform(10000, 20000),
    receive
        {result, _, void} ->
            cql_write_rows(Client, WriteQuery, Tail, BatchSize, TryCount);
        {error, Tag, {16#2200, _, _}} ->
            NewBatchSize = max(1, round(BatchSize * 0.75)),
            cql_write_rows(Client, WriteQuery, Rows, NewBatchSize, TryCount);
        {error, Tag, {16#1100, _, _}} ->
            ?WARNING_MSG("Write timeout 1 ~p", [{WriteQuery, length(NewRows)}]),
            timer:sleep(crypto:rand_uniform(50, 500)),
            cql_write_rows(Client, WriteQuery, Rows, BatchSize, TryCount - 1);
        {error, Tag, Reason} ->
            {error, Reason}
    after WriteTimeout ->
        ?WARNING_MSG("Write timeout 2 ~p", [{WriteQuery, length(NewRows)}]),
        cql_write_rows(Client, WriteQuery, Tail, BatchSize, TryCount)
    end.


prepared_queries() ->
    [{test_query, test_query_sql()}] ++
    total_count_queries().

test_query_sql() ->
    "SELECT now() FROM system.local". %% "SELECT 1" for cassandra

test_query(PoolName) ->
    test_query(PoolName, undefined).

test_query(PoolName, UserJID) ->
    try  mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, test_query, []) of
        {ok, _} -> ok
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

total_count_query(PoolName, Table) ->
    UserJID = undefined,
    Res = mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, {total_count_query, Table}, []),
    {ok, [Row]} = Res,
    proplists:get_value(count, Row).

total_count_queries() ->
    [{{total_count_query, T}, total_count_query_cql(T)} || T <- tables()].

total_count_query_cql(T) when is_atom(T) ->
    "SELECT COUNT(*) FROM " ++ atom_to_list(T).

tables() ->
    [
     mam_message,
     mam_muc_message,
     mam_config
%%     private_storage,
%%     privacy_default_list,
%%     privacy_list,
%%     privacy_item,
%%     rosterusers,
%%     roster_version
    ].