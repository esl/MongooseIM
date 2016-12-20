-module(mongoose_cassandra).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([start/0, stop/0]).
-export([now_timestamp/0]).
-export([cql_read/5, cql_write/5, cql_write_async/5]).

-export([prepared_queries/0, total_count_query_cql/1, test_query_sql/0]).
-export([test_query/1, test_query/2, total_count_query/2]).

-callback prepared_queries() -> list({term(), string()}).

start() ->
    case ejabberd_config:get_local_option(cassandra_servers) of
        undefined ->
            ignore;
        Pools ->
            cqerl_app:start(normal, []),
            [init_pool(Pool) || Pool <- Pools]
    end.

init_pool({PoolName, PoolConfig}) ->
    init_pool({PoolName, 50, PoolConfig});
init_pool({PoolName, PoolSize, PoolConfig}) ->
    ExtConfig = extend_config(PoolConfig),
    application:set_env(cqerl, num_clients, PoolSize),
    cqerl_cluster:add_nodes(PoolName, proplists:get_value(servers, ExtConfig),
                                       ExtConfig).

extend_config(PoolConfig) ->
    PoolConfig
        ++ [{servers, [{"localhost", 9042}]},
%%            {tcp_opts, [{connect_timeout, 4000}]},
            {keyspace, mongooseim}].

-spec stop() -> _.
stop() ->
    cqerl_app:stop(undefined).

%% @doc Return timestamp in nanoseconds
now_timestamp() ->
    now_to_usec(os:timestamp()).

-spec now_to_usec(erlang:timestamp()) -> non_neg_integer().
now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.

cql_write_async(PoolName, _UserJID, Module, QueryName, Rows)  ->
    QueryStr = proplists:get_value(QueryName, Module:prepared_queries()),
    Query = #cql_query{statement = QueryStr},
    BatchQuery = #cql_query_batch{
        mode = unlogged,
        queries = [Query#cql_query{values = Params} || Params <- Rows]
    },

    {ok, Client} = cqerl:get_client(PoolName),
    cqerl:send_query(Client, BatchQuery).

cql_write(PoolName, _UserJID, Module, QueryName, Rows)  ->
    Tag = cql_write_async(PoolName, _UserJID, Module, QueryName, Rows),
    receive
        {result, Tag, void} -> ok;
        {result, Tag, Reason} ->
            {error, Reason}
    end.

cql_read(PoolName, _UserJID, Module, QueryName, Params)  ->
    QueryStr = proplists:get_value(QueryName, Module:prepared_queries()),
    Query = #cql_query{statement = QueryStr, values = Params},

    {ok, Client} = cqerl:get_client(PoolName),
    case cqerl:run_query(Client, Query) of
        {error, Reason} ->
            {error, Reason};
        {ok, Result} ->
            {ok, cql_read_pages(Result, [])}
    end.


cql_read_pages(Result, Acc0) ->
    Acc = [cqerl:all_rows(Result) | Acc0],
    case cqerl:has_more_pages(Result) of
        true ->
            {ok, NextResult} = cqerl:fetch_more(Result),
            cql_read_pages(NextResult, Acc);
        false ->
            lists:concat(lists:reverse(Acc))
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