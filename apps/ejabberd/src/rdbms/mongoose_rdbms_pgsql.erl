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
-author('konrad.zemek@erlang-solutions.com').
-behaviour(mongoose_rdbms).

-include_lib("epgsql/include/epgsql.hrl").

-define(PGSQL_PORT, 5432).

-export([escape_format/1, connect/2, disconnect/1, query/3, prepare/6, execute/4]).

%% API

-spec escape_format(Host :: ejabberd:server()) -> atom().
escape_format(_Host) ->
    hex.

-spec connect(Args :: any(), QueryTimeout :: non_neg_integer()) ->
                     {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Settings, QueryTimeout) ->
    case epgsql:connect(db_opts(Settings)) of
        {ok, Pid} ->
            epgsql:squery(Pid, [<<"SET statement_timeout=">>, integer_to_binary(QueryTimeout)]),
            epgsql:squery(Pid, <<"SET standard_conforming_strings=off">>),
            {ok, Pid};
        Error ->
            Error
    end.

-spec disconnect(Connection :: epgsql:connection()) -> ok.
disconnect(Connection) ->
    epgsql:close(Connection).

-spec query(Connection :: term(), Query :: any(),
            Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
query(Connection, Query, _Timeout) ->
    pgsql_to_odbc(epgsql:squery(Connection, Query)).

-spec prepare(Host :: ejabberd:server(), Connection :: term(), Name :: atom(), Table :: binary(),
              Fields :: [binary()], Statement :: iodata()) ->
                     {ok, term()} | {error, any()}.
prepare(_Host, Connection, Name, _Table, _Fields, Statement) ->
    BinName = atom_to_binary(Name, latin1),
    ReplacedStatement = replace_question_marks(Statement),
    case epgsql:parse(Connection, BinName, ReplacedStatement, []) of
        {ok, _} -> {ok, BinName};
        Error   -> Error
    end.

-spec execute(Connection :: term(), StatementRef :: term(), Params :: [term()],
              Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
execute(Connection, StatementRef, Params, _Timeout) ->
    pgsql_to_odbc(epgsql:prepared_query(Connection, StatementRef, Params)).

%% Helpers

-spec db_opts(Settings :: term()) -> [term()].
db_opts({pgsql, Server, DB, User, Pass}) ->
    db_opts({pgsql, Server, ?PGSQL_PORT, DB, User, Pass});
db_opts({pgsql, Server, Port, DB, User, Pass}) when is_integer(Port) ->
    [
     {host, Server},
     {port, Port},
     {database, DB},
     {username, User},
     {password, Pass}
    ].

-spec pgsql_to_odbc(epgsql:reply(term())) -> mongoose_rdbms:query_result().
pgsql_to_odbc(Items) when is_list(Items) ->
    lists:reverse([pgsql_to_odbc(Item) || Item <- Items]);
pgsql_to_odbc({error, #error{codename = unique_violation}}) ->
    {error, duplicate_key};
pgsql_to_odbc({error, #error{message = Message}}) ->
    {error, unicode:characters_to_list(Message)};
pgsql_to_odbc({ok, Count}) ->
    {updated, Count};
pgsql_to_odbc({ok, _Columns, Rows}) ->
    {selected, Rows}.

-spec replace_question_marks(Statement :: iodata()) -> iodata().
replace_question_marks(Statement) when is_list(Statement) ->
    replace_question_marks(iolist_to_binary(Statement));
replace_question_marks(Statement) when is_binary(Statement) ->
    [Head | Parts] = binary:split(Statement, <<"?">>, [global]),
    Placeholders = [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(Parts))],
PartsWithPlaceholders = lists:zipwith(fun(A, B) -> [A, B] end, Placeholders, Parts),
                      [Head | PartsWithPlaceholders].
