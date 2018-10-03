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

-include_lib("cqerl/include/cqerl.hrl").
-include("mongoose_logger.hrl").

%% ====================================================================
%% Definitions
%% ====================================================================

-define(WRITE_RETRY_COUNT, 3).
-define(READ_RETRY_COUNT, 3).

%% ====================================================================
%% Types
%% ====================================================================

-type row()                 :: #{atom() => term()}.
-type rows()                :: [row()].
-type parameters()          :: proplists:proplist() | row().
-type fold_fun()            :: fun((Page :: rows(), fold_accumulator()) -> fold_accumulator()).
-type fold_accumulator()    :: any().

-type pool_name()           :: atom().
-type query_name()          :: atom() | {atom(), atom()}.

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
-export_type([pool_name/0, query_name/0]).
-export_type([row/0, rows/0, parameters/0, fold_fun/0, fold_accumulator/0]).

%% Callbacks definitions
-callback prepared_queries() -> list({term(), string()}).


%% ====================================================================
%% Module API
%% ====================================================================

-spec start() -> ok.
start() ->
    ok.

-spec stop() -> ok.
stop() ->
    ok.

%% ====================================================================
%% Cassandra API
%% ====================================================================


%% --------------------------------------------------------
%% @doc Execute batch write query to cassandra (insert, update or delete).
%% Note that Cassandra doesn't like big batches, therefore this function will try to
%% split given rows into batches of 20 rows and will fall back to smaller batches if
%% Cassandra rejects the query due to its size being to big.
%% --------------------------------------------------------
-spec cql_write(PoolName :: pool_name(), UserJID :: ejabberd:jid() | undefined, Module :: atom(),
                QueryName :: query_name(), Rows :: [parameters()]) ->
                       ok | {error, Reason :: any()}.
cql_write(PoolName, UserJID, Module, QueryName, Rows)  ->
    QueryStr = proplists:get_value(QueryName, Module:prepared_queries()),
    Opts = #{
      retry   => ?WRITE_RETRY_COUNT
     },
    mongoose_cassandra_worker:write(PoolName, UserJID, QueryStr, Rows, Opts).

%% --------------------------------------------------------
%% @doc Execute async batch write query to cassandra (insert, update or delete).
%% Note that Cassandra doesn't like big batches and there's not retry login when query size will
%% be exceeded like in cql_write/5.
%% --------------------------------------------------------
-spec cql_write_async(PoolName :: pool_name(), UserJID :: ejabberd:jid() | undefined, Module :: atom(),
                      QueryName :: query_name(), Rows :: [parameters()]) ->
                             ok | {error, Reason :: any()}.
cql_write_async(PoolName, UserJID, Module, QueryName, Rows)  ->
    QueryStr = proplists:get_value(QueryName, Module:prepared_queries()),
    Opts = #{
      retry   => ?WRITE_RETRY_COUNT
     },
    mongoose_cassandra_worker:write_async(PoolName, UserJID, QueryStr, Rows, Opts).


%% --------------------------------------------------------
%% @doc Execute read query to cassandra (select).
%% Returns all rows at once even if there are several query pages.
%% --------------------------------------------------------
-spec cql_read(PoolName :: pool_name(), UserJID :: ejabberd:jid() | undefined, Module :: atom(),
               QueryName :: query_name(), Params :: parameters()) ->
                      {ok, Rows :: rows()} | {error, Reason :: any()}.
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
-spec cql_foldl(PoolName :: pool_name(), UserJID :: ejabberd:jid() | undefined, Module :: atom(),
                QueryName :: query_name(), Params :: parameters(),
                fold_fun(), AccIn :: fold_accumulator()) ->
                       {ok, AccOut :: fold_accumulator()} | {error, Reason :: any()}.
cql_foldl(PoolName, UserJID, Module, QueryName, Params, Fun, AccIn)  ->
    QueryStr = proplists:get_value(QueryName, Module:prepared_queries()),
    Opts = #{
      retry   => ?READ_RETRY_COUNT
     },
    mongoose_cassandra_worker:read(PoolName, UserJID, QueryStr, Params, Fun, AccIn, Opts).


%% @doc Return timestamp in microseconds
now_timestamp() ->
    now_to_usec(os:timestamp()).

-spec now_to_usec(erlang:timestamp()) -> non_neg_integer().
now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.

%% ====================================================================
%% Internal functions
%% ====================================================================

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
