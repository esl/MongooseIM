-module(ejabberd_odbc_odbc).
-behaviour(ejabberd_odbc).

-include("ejabberd.hrl").
-include("ejabberd_odbc.hrl").

-export([escape_format/1, connect/1, disconnect/1, query/2]).

-spec escape_format(Host :: ejabberd:server()) -> atom().
escape_format(Host) ->
    Key = {odbc_server_type, Host},
    case ejabberd_config:get_local_option_or_default(Key, odbc) of
        pgsql ->
            hex;
        mssql ->
            mssql_hex;
        _ ->
            simple_escape
    end.

-spec connect(Args :: any()) ->
    {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Settings) ->
    application:start(odbc),
    Opts = [{scrollable_cursors, off},
            {binary_strings, on},
            {timeout, 5000}],
    odbc:connect(db_opts(Settings), Opts).

-spec disconnect(Connection :: term()) -> any().
disconnect(Connection) ->
    odbc:disconnect(Connection).

-spec query(Connection :: term(), Query :: any()) -> term().
query(Connection, Query) ->
    binaryze_odbc(odbc:sql_query(Connection, Query, ?QUERY_TIMEOUT)).

-spec db_opts(Settings :: term()) -> proplists:proplist().
db_opts(SQLServer) when is_list(SQLServer) ->
    SQLServer.

binaryze_odbc(ODBCResults) when is_list(ODBCResults) ->
    lists:map(fun binaryze_odbc/1, ODBCResults);
binaryze_odbc({selected, ColNames, Rows}) ->
    ColNamesB = lists:map(fun ejabberd_binary:string_to_binary/1, ColNames),
    {selected, ColNamesB, Rows};
binaryze_odbc(ODBCResult) ->
    ODBCResult.
