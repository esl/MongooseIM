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

-module(mongoose_rdbms_mysql).
-author('konrad.zemek@erlang-solutions.com').
-behaviour(mongoose_rdbms).

-include("ejabberd.hrl").

-define(MYSQL_PORT, 3306).

-export([escape_format/1, connect/1, disconnect/1, query/3, is_error_duplicate/1]).

%% API

-spec escape_format(Host :: ejabberd:server()) -> atom().
escape_format(_Host) ->
    simple_escape.

-spec connect(Args :: any()) -> {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Settings) ->
    [Server, Port, Database, User, Password] = db_opts(Settings),
    case mysql_conn:start_link(Server, Port, User, Password, Database,
                               fun log/4, utf8, undefined, true) of
        {ok, Ref} ->
            query(Ref, <<"SET SESSION query_cache_type=1;">>, 5000),
            {ok, Ref};
        Error ->
            Error
    end.

-spec disconnect(Connection :: term()) -> any().
disconnect(Connection) ->
    Connection ! stop.

-spec query(Connection :: term(), Query :: any(),
            Timeout :: infinity | non_neg_integer()) -> term().
query(Connection, Query, Timeout) ->
    mysql_to_odbc(mysql_conn:fetch(Connection, iolist_to_binary(Query), self(), Timeout)).

-spec is_error_duplicate(Reason :: string()) -> boolean().
is_error_duplicate("duplicate" ++ _) -> true;
is_error_duplicate(_Reason) -> false.

%% Helpers

-spec db_opts(Settings :: term()) -> list().
db_opts({mysql, Server, DB, User, Pass}) ->
    db_opts({mysql, Server, ?MYSQL_PORT, DB, User, Pass});
db_opts({mysql, Server, Port, DB, User, Pass}) when is_integer(Port) ->
    [Server, Port, DB, User, Pass].

%% @doc Convert MySQL query result to Erlang ODBC result formalism
-spec mysql_to_odbc({data, _} | {updated, _} | {error, _}) ->
    {error, string()} | {updated, non_neg_integer()} | {selected, [tuple()]}.
mysql_to_odbc({updated, MySQLRes}) ->
    {updated, mysql:get_result_affected_rows(MySQLRes)};
mysql_to_odbc({data, MySQLRes}) ->
    {selected, [parse_row(Row) || Row <- mysql:get_result_rows(MySQLRes)]};
mysql_to_odbc({error, MySQLRes}) when is_list(MySQLRes) ->
    {error, MySQLRes};
mysql_to_odbc({error, MySQLRes}) ->
    {error, mysql:get_result_reason(MySQLRes)}.

-spec parse_row(Row :: [term()]) -> tuple().
parse_row(Row) ->
    Translated = lists:map(fun(undefined) -> null; (Col) -> Col end, Row),
    list_to_tuple(Translated).

-spec log(module(), Line :: pos_integer(), debug | normal | error,
          fun(() -> {Format :: string(), Args :: [term()]})) -> any().
log(_Module, _Line, Level, FormatFun) ->
    {Format, Args} = FormatFun(),
    case Level of
        debug -> ?DEBUG(Format, Args);
        normal -> ?INFO_MSG(Format, Args);
        error -> ?ERROR_MSG(Format, Args)
    end.
