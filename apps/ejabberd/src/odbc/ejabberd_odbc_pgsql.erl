-module(ejabberd_odbc_pgsql).
-behaviour(ejabberd_odbc).

-include("ejabberd.hrl").
-include("ejabberd_odbc.hrl").

-define(PGSQL_PORT, 5432).

-export([escape_format/1, connect/1, disconnect/1, query/2]).

-spec escape_format(Host :: ejabberd:server()) -> atom().
escape_format(_Host) ->
    hex.

-spec connect(Args :: any()) ->
    {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Settings) ->
    [Server, Port, DB, Username, Password] = db_opts(Settings),
    Params = [
              {host, Server},
              {database, DB},
              {user, Username},
              {password, Password},
              {port, Port},
              {as_binary, true}],
    case pgsql:connect(Params) of
        {ok, Ref} ->
            {ok, [<<"SET">>]} =
                pgsql:squery(Ref, "SET standard_conforming_strings=off;", ?QUERY_TIMEOUT),
            {ok, Ref};
        Err -> Err
    end.

-spec disconnect(Connection :: term()) -> any().
disconnect(Connection) ->
    pgsql:terminate(Connection).

-spec query(Connection :: term(), Query :: any()) -> term().
query(Connection, Query) ->
    pgsql_to_odbc(pgsql:squery(Connection, Query, ?QUERY_TIMEOUT)).

-spec db_opts(Settings :: term()) -> proplists:proplist().
db_opts({pgsql, Server, DB, User, Pass}) ->
    [Server, ?PGSQL_PORT, DB, User, Pass];
db_opts({pgsql, Server, Port, DB, User, Pass}) when is_integer(Port) ->
    [Server, Port, DB, User, Pass].

%% @doc Convert PostgreSQL query result to Erlang ODBC result formalism
-spec pgsql_to_odbc({'ok', PGSQLResult :: [any()]})
                   -> [{'error', _} | {'updated', 'undefined' | integer()}
                       | {'selected', [any()], [any()]}]
                          | {'error', _}
                          | {'updated', 'undefined' | integer()}
                          | {'selected', [any()], [tuple()]}.
pgsql_to_odbc({ok, PGSQLResult}) ->
    case PGSQLResult of
        [Item] ->
            pgsql_item_to_odbc(Item);
        Items ->
            [pgsql_item_to_odbc(Item) || Item <- Items]
    end.

-spec pgsql_item_to_odbc(tuple() | binary())
                        -> {'error', _}
                               | {'updated', 'undefined' | integer()}
                               | {'selected', [any()], [tuple()]}.
pgsql_item_to_odbc({<<"SELECT", _/binary>>, Rows, Recs}) ->
    {selected,
     [element(1, Row) || Row <- Rows],
     [list_to_tuple(Rec) || Rec <- Recs]};
pgsql_item_to_odbc(<<"INSERT ", OIDN/binary>>) ->
    [_OID, N] = binary:split(OIDN, <<" ">>),
    {updated, list_to_integer(binary_to_list(N))};
pgsql_item_to_odbc(<<"DELETE ", N/binary>>) ->
    {updated, list_to_integer(binary_to_list(N))};
pgsql_item_to_odbc(<<"UPDATE ", N/binary>>) ->
    {updated, list_to_integer(binary_to_list(N))};
pgsql_item_to_odbc({error, Error}) ->
    {error, Error};
pgsql_item_to_odbc(_) ->
    {updated, undefined}.
