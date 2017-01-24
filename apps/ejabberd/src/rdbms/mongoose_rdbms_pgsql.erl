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

-module(mongoose_rdbms_pgsql).
-author('konrad.zemek@gmail.com').
-behaviour(mongoose_rdbms).

-define(PGSQL_PORT, 5432).

-export([escape_format/1, connect/1, disconnect/1, query/3, is_error_duplicate/1]).

%% API

-spec escape_format(Host :: ejabberd:server()) -> atom().
escape_format(_Host) ->
    hex.

-spec connect(Args :: any()) ->
    {ok, Connection :: epgsql:connection()} | {error, Reason :: any()}.
connect(Settings) ->
    [Server, Port, DB, Username, Password] = db_opts(Settings),
    case pgsql_connection:start_link([{host, Server}, {port, Port}, {database, DB},
                                      {user, Username}, {password, Password}]) of
        {ok, Pid} ->
            query(Pid, <<"SET standard_conforming_strings=off;">>, 5000),
            {ok, Pid};
        Error ->
            Error
    end.

-spec disconnect(Connection :: epgsql:connection()) -> ok.
disconnect(Connection) ->
    pgsql_connection:close({pgsql_connection, Connection}).

-spec query(Connection :: term(), Query :: any(),
            Timeout :: infinity | non_neg_integer()) -> term().
query(Connection, Query, Timeout) ->
    pgsql_to_odbc(pgsql_connection:simple_query(Query, [], Timeout,
                                                {pgsql_connection, Connection})).

-spec is_error_duplicate(Reason :: string()) -> boolean().
is_error_duplicate("Duplicate" ++ _) -> true;
is_error_duplicate(_Reason) -> false.

%% Helpers

-spec db_opts(Settings :: term()) -> [term()].
db_opts({pgsql, Server, DB, User, Pass}) ->
    db_opts({pgsql, Server, ?PGSQL_PORT, DB, User, Pass});
db_opts({pgsql, Server, Port, DB, User, Pass}) when is_integer(Port) ->
    [Server, Port, DB, User, Pass].

-spec pgsql_to_odbc
    (pgsql_connection:result_tuple() | {error, any()}) ->
        {error, any()} | {selected, [tuple()]} | {updated, undefined | non_neg_integer()};
    ([pgsql_connection:result_tuple() | {error, any()}]) ->
        [{error, any()} | {selected, [tuple()]} | {updated, undefined | non_neg_integer()}].
pgsql_to_odbc(Items) when is_list(Items) ->
    lists:reverse([pgsql_to_odbc(Item) || Item <- Items]);
pgsql_to_odbc({error, Reason}) ->
    {pgsql_error, Details} = Reason,
    Message = proplists:get_value(message, Details, <<"unknown error">>),
    {error, unicode:characters_to_list(Message)};
pgsql_to_odbc({{select, _Count}, Rows}) ->
    {selected, [parse_row(Row) || Row <- Rows]};
pgsql_to_odbc({{insert, _TableOID, Count}, _Rows}) ->
    {updated, Count};
pgsql_to_odbc({{_, Count}, _}) when is_integer(Count) ->
    {updated, Count};
pgsql_to_odbc(_) ->
    {updated, undefined}.

-spec parse_row(Row :: tuple()) -> tuple().
parse_row(Row) ->
    RowList = tuple_to_list(Row),
    Translated = lists:map(fun({_EnumName, Value}) -> Value; (Col) -> Col end, RowList),
    list_to_tuple(Translated).
