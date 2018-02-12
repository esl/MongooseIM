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
-author('rafal.slota@erlang-solutions.com').

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-define(READ_TIMEOUT, timer:minutes(1)).

%% ====================================================================
%% Types
%% ====================================================================

-type row() :: #{atom() => term()}.
-type parameters() :: proplists:proplist() | row().
-type row_fold_fun() :: fun((Page :: [row()], AccIn :: term()) -> AccOut :: term()).
-type pool_name() :: atom().
-type cql_query() :: #cql_query{}.
-type query_name() :: atom() | {atom(), atom()}.

%% ====================================================================
%% Exports
%% ====================================================================

%% Module callbacks
-export([start/0, stop/0]).

%% API
-export([cql_read/5, cql_foldl/7, cql_write/5, cql_write_async/5]).
-export([now_timestamp/0]).

%% Test queries
-export([prepared_queries/0, total_count_query_cql/1, test_query_sql/0]).
-export([test_query/1, test_query/2, total_count_query/2]).

%% Types
-export_type([pool_name/0, row/0, query_name/0]).

%% Callbacks definitions
-callback prepared_queries() -> list({term(), string()}).


%% ====================================================================
%% Module API
%% ====================================================================

-spec start() -> ignore | ok | no_return().
start() ->
    application:set_env(cqerl, maps, true),
    case ejabberd_config:get_local_option(cassandra_servers) of
        undefined ->
            ignore;
        Pools ->
            cqerl_app:start(normal, []),
            [init_pool(Pool) || Pool <- Pools]
    end.

-spec stop() -> _.
stop() ->
    mongoose_cassandra_sup:stop(),
    cqerl_app:stop(undefined).

%% Module helpers
-spec init_pool({pool_name(), proplists:proplist()} |
                {pool_name(), PoolSize :: non_neg_integer(), proplists:proplist()}) ->
                       ok | no_return().
init_pool({PoolName, PoolConfig}) ->
    init_pool({PoolName, 20, PoolConfig});
init_pool({PoolName, PoolSize, PoolConfig}) ->
    ExtConfig = extend_config(PoolConfig),
    application:set_env(cqerl, num_clients, PoolSize),
    ok = cqerl_cluster:add_nodes(PoolName, proplists:get_value(servers, ExtConfig), ExtConfig),
    {ok, _} = mongoose_cassandra_sup:start(PoolName, PoolSize * 4),
    ok.

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


%% --------------------------------------------------------
%% @doc Execute batch write query to cassandra (insert, update or delete).
%% Note that Cassandra doesn't like big batches, therefore this function will try to
%% split given rows into batches of 50 rows and will fall back to smaller batches if
%% Cassandra rejects the query due to its size being to big.
%% --------------------------------------------------------
-spec cql_write(PoolName :: pool_name(), UserJID :: jid:jid(), Module :: atom(),
                QueryName :: query_name(), Rows :: [parameters()]) ->
                       ok | {error, Reason :: any()}.
cql_write(PoolName, _UserJID, Module, QueryName, Rows)  ->
    QueryStr = proplists:get_value(QueryName, Module:prepared_queries()),
    Query = #cql_query{statement = QueryStr},

    {ok, Client} = cqerl:get_client(PoolName),
    cql_write_rows(Client, Query, Rows, 50, 3).

%% --------------------------------------------------------
%% @doc Execute async batch write query to cassandra (insert, update or delete).
%% Note that Cassandra doesn't like big batches and there's not retry login when query size will
%% be exceeded like in cql_write/5.
%% --------------------------------------------------------
-spec cql_write_async(PoolName :: pool_name(), UserJID :: jid:jid(), Module :: atom(),
                      QueryName :: query_name(), Rows :: [parameters()]) ->
                             ok | {error, Reason :: any()}.
cql_write_async(PoolName, UserJID, Module, QueryName, Rows)  ->
    QueryStr = proplists:get_value(QueryName, Module:prepared_queries()),
    Query = #cql_query{statement = QueryStr},

    {ok, Client} = cqerl:get_client(PoolName),
    BatchQuery =
        #cql_query_batch{
           mode = unlogged,
           queries = [Query#cql_query{values = Row}
                      || Row <- Rows]
          },

    QueryExecutor =
        fun() ->
                cqerl:send_query(Client, BatchQuery)
        end,

    Worker = mongoose_cassandra_sup:select_worker(PoolName, UserJID),
    gen_server:cast(Worker, {cql_write, QueryExecutor}).


%% --------------------------------------------------------
%% @doc Execute read query to cassandra (select).
%% Returns all rows at once even if there are several query pages.
%% --------------------------------------------------------
-spec cql_read(PoolName :: pool_name(), UserJID :: jid:jid() | undefined, Module :: atom(),
               QueryName :: query_name(), Params :: parameters()) ->
                      {ok, Rows :: [row()]} | {error, Reason :: any()}.
cql_read(PoolName, UserJID, Module, QueryName, Params)  ->
    Fun = fun(Page, Acc) -> [Page | Acc] end,
    case cql_foldl(PoolName, UserJID, Module, QueryName, Params, Fun, []) of
        {ok, Rows} ->
            {ok, lists:append(lists:reverse(Rows))};
        {error, Reason} ->
            {error, Reason}
    end.


%% --------------------------------------------------------
%% @doc Execute read query to cassandra (select).
%% This functions behaves much like the lists:foldl/3 but the input are pages from result of given
%% query. Therefore each execution of given fun gets list of several result rows (by default 100 at
%% most).
%% --------------------------------------------------------
-spec cql_foldl(PoolName :: pool_name(), UserJID :: jid:jid() | undefined, Module :: atom(),
                QueryName :: query_name(), Params :: parameters(),
                row_fold_fun(), AccIn :: term()) ->
                       {ok, AccOut :: term()} | {error, Reason :: any()}.
cql_foldl(PoolName, UserJID, Module, QueryName, Params, Fun, AccIn)  ->
    cql_foldl(PoolName, UserJID, Module, QueryName, Params, Fun, AccIn, 3).

-spec cql_foldl(PoolName :: pool_name(), UserJID :: jid:jid() | undefined, Module :: atom(),
                QueryName :: query_name(), Params :: parameters(),
                row_fold_fun(), AccIn :: term(), TryCount :: non_neg_integer()) ->
                       {ok, AccOut :: term()} | {error, Reason :: any()}.
cql_foldl(_PoolName, _UserJID, _Module, QueryName, Params, _Fun, _AccIn, 0)  ->
    {error, {query_timeout, QueryName, Params}};
cql_foldl(PoolName, UserJID, Module, QueryName, Params, Fun, AccIn, TryCount)  ->
    QueryStr = proplists:get_value(QueryName, Module:prepared_queries()),
    Query = #cql_query{statement = QueryStr, values = Params},

    {ok, Client} = cqerl:get_client(PoolName),
    Tag = cqerl:send_query(Client, Query),
    receive
        {result, Tag, Result} ->
            cql_read_pages(Result, Fun, AccIn, TryCount);
        {error, Tag, {16#1200, _, _}} ->
            cql_foldl(PoolName, UserJID, Module, QueryName, Params, Fun, AccIn, TryCount - 1);
        {error, Tag, Reason} ->
            {error, Reason}
    after ?READ_TIMEOUT ->
            cql_foldl(PoolName, UserJID, Module, QueryName, Params, Fun, AccIn, TryCount - 1)
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec cql_read_pages(ResultHandle :: term(), row_fold_fun(), AccIn :: term(),
                     TryCount :: non_neg_integer()) ->
                            {ok, AccOut :: term()} | {error, Reason :: any()}.
cql_read_pages(Result, _Fun, _AccIn, 0) ->
    {error, {query_page_timeout, Result}};
cql_read_pages(Result, Fun, AccIn, Retry) ->
    NextAcc = Fun(cqerl:all_rows(Result), AccIn),
    case cqerl:has_more_pages(Result) of
        true ->
            Tag = cqerl:fetch_more_async(Result),
            receive
                {result, Tag, NextResult} ->
                    cql_read_pages(NextResult, Fun, NextAcc, Retry);
                {error, Tag, {16#1200, _, _}} ->
                    timer:sleep(crypto:rand_uniform(50, 500)),
                    cql_read_pages(Result, Fun, NextAcc, Retry - 1);
                {error, Tag, Reason} ->
                    {error, Reason}
            after ?READ_TIMEOUT ->
                    cql_read_pages(Result, Fun, NextAcc, Retry - 1)
            end;
        false ->
            {ok, NextAcc}
    end.

-spec cql_write_rows(Client :: cqerl:client(), WriteQuery :: cql_query(), Rows :: [row()],
                     BatchSize :: non_neg_integer(), TryCount :: non_neg_integer()) ->
                            ok | {error, Reason :: term()}.
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
    WriteTimeout = timer:minutes(1),
    receive
        {result, _, void} ->
            cql_write_rows(Client, WriteQuery, Tail, BatchSize, TryCount);
        {error, Tag, {16#2200, _, _}} ->
            NewBatchSize = max(1, round(BatchSize * 0.75)),
            cql_write_rows(Client, WriteQuery, Rows, NewBatchSize, TryCount);
        {error, Tag, {16#1100, _, _}} ->
            timer:sleep(crypto:rand_uniform(10, 50)),
            cql_write_rows(Client, WriteQuery, Rows, BatchSize, TryCount - 1);
        {error, Tag, Reason} ->
            {error, Reason}
    after WriteTimeout ->
            cql_write_rows(Client, WriteQuery, Tail, BatchSize, 0)
    end.

%% ====================================================================
%% Queries
%% ====================================================================

prepared_queries() ->
    [{test_query, test_query_sql()}] ++
        total_count_queries().

test_query_sql() ->
    "SELECT now() FROM system.local". %% "SELECT 1" for cassandra

total_count_query_cql(T) when is_atom(T) ->
    "SELECT COUNT(*) FROM " ++ atom_to_list(T).


%% ====================================================================
%% Diagnostic utilities
%% ====================================================================

-spec test_query(pool_name()) -> ok | {error, Reason :: term()}.
test_query(PoolName) ->
    test_query(PoolName, undefined).

test_query(PoolName, UserJID) ->
    try  mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, test_query, []) of
         {ok, _} -> ok
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

-spec total_count_query(pool_name(), Table :: atom()) ->
                               non_neg_integer().
total_count_query(PoolName, Table) ->
    UserJID = undefined,
    Res = mongoose_cassandra:cql_read(PoolName, UserJID, ?MODULE, {total_count_query, Table}, []),
    {ok, [#{count := Count}]} = Res,
    Count.

total_count_queries() ->
    [{{total_count_query, T}, total_count_query_cql(T)} || T <- tables()].


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
