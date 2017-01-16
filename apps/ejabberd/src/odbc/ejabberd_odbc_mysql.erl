-module(ejabberd_odbc_mysql).
-behaviour(ejabberd_odbc).

-include("ejabberd.hrl").
-include("ejabberd_odbc.hrl").

-define(MYSQL_PORT, 3306).

-export([escape_format/1, connect/1, disconnect/1, query/2]).

-spec escape_format(Host :: ejabberd:server()) -> atom().
escape_format(_Host) ->
    simple_escape.

-spec connect(Args :: any()) ->
    {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Settings) ->
    [Server, Port, Database, Username, Password] = db_opts(Settings),
    case p1_mysql_conn:start(Server, Port, Username, Password, Database, fun log/3) of
        {ok, Ref} ->
            query(Ref, [<<"set names 'utf8';">>]),
            query(Ref, [<<"SET SESSION query_cache_type=1;">>]),
            {ok, Ref};

        Err ->
            Err
    end.

-spec disconnect(Connection :: term()) -> any().
disconnect(Connection) ->
    p1_mysql_conn:stop(Connection).

-spec query(Connection :: term(), Query :: any()) -> term().
query(Connection, Query) ->
    mysql_to_odbc(p1_mysql_conn:squery(Connection, Query, self(),
                                       [{timeout, ?QUERY_TIMEOUT}, {result_type, binary}])).

-spec db_opts(Settings :: term()) -> list().
db_opts({mysql, Server, DB, User, Pass}) ->
    [Server, ?MYSQL_PORT, DB, User, Pass];
db_opts({mysql, Server, Port, DB, User, Pass}) when is_integer(Port) ->
    [Server, Port, DB, User, Pass].

-spec log(Level :: 'debug' | 'error' | 'normal', Format :: string(), Args :: list()) -> any().
log(debug, Format, Args)  -> ?DEBUG(Format, Args);
log(normal, Format, Args) -> ?INFO_MSG(Format, Args);
log(error, Format, Args)  -> ?ERROR_MSG(Format, Args).

%% @doc Convert MySQL query result to Erlang ODBC result formalism
-spec mysql_to_odbc({'data', _} | {'error', _} | {'updated', _})
                   -> {'error', _} | {'updated', _} | {'selected', [any()], [tuple()]}.
mysql_to_odbc({updated, MySQLRes}) ->
    {updated, p1_mysql:get_result_affected_rows(MySQLRes)};
mysql_to_odbc({data, MySQLRes}) ->
    mysql_item_to_odbc(p1_mysql:get_result_field_info(MySQLRes),
                       p1_mysql:get_result_rows(MySQLRes));
mysql_to_odbc({error, MySQLRes}) when is_list(MySQLRes) ->
    {error, MySQLRes};
mysql_to_odbc({error, MySQLRes}) ->
    {error, p1_mysql:get_result_reason(MySQLRes)}.

%% @doc When tabular data is returned, convert it to the ODBC formalism
-spec mysql_item_to_odbc(Columns :: [tuple()],
                         Recs :: [[any()]]) -> {'selected',[any()],[tuple()]}.
mysql_item_to_odbc(Columns, Recs) ->
    %% For now, there is a bug and we do not get the correct value from MySQL
    %% module:
    {selected,
     [element(2, Column) || Column <- Columns],
     [list_to_tuple(Rec) || Rec <- Recs]}.
