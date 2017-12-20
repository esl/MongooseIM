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

-export([escape_format/1, connect/2, disconnect/1, query/3, prepare/6, execute/4]).

%% API

-spec escape_format(mongoose_rdbms:pool()) -> atom().
escape_format(_Pool) ->
    simple_escape.

-spec connect(Args :: any(), QueryTimeout :: non_neg_integer()) ->
                     {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Settings, QueryTimeout) ->
    case mysql:start_link([{query_timeout, QueryTimeout} | db_opts(Settings)]) of
        {ok, Ref} ->
            mysql:query(Ref, <<"set names 'utf8';">>),
            mysql:query(Ref, <<"SET SESSION query_cache_type=1;">>),
            {ok, Ref};
        Error ->
            Error
    end.

-spec disconnect(Connection :: term()) -> any().
disconnect(Connection) ->
    gen_server:stop(Connection).

-spec query(Connection :: term(), Query :: any(),
            Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
query(Connection, Query, _Timeout) ->
    mysql_to_odbc(mysql:query(Connection, Query), Connection).

-spec prepare(Pool :: mongoose_rdbms:pool(),
              Connection :: term(), Name :: atom(), Table :: binary(),
              Fields :: [binary()], Statement :: iodata()) ->
                     {ok, term()} | {error, any()}.
prepare(_Pool, Connection, Name, _Table, _Fields, Statement) ->
    mysql:prepare(Connection, Name, Statement).

-spec execute(Connection :: term(), StatementRef :: term(), Params :: [term()],
              Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
execute(Connection, StatementRef, Params, _Timeout) ->
    mysql_to_odbc(mysql:execute(Connection, StatementRef, Params), Connection).

%% Helpers

-spec db_opts(Settings :: term()) -> list().
db_opts({mysql, Server, DB, User, Pass}) ->
    db_opts({mysql, Server, ?MYSQL_PORT, DB, User, Pass});
db_opts({mysql, Server, Port, DB, User, Pass}) when is_integer(Port) ->
    get_db_basic_opts({Server, Port, DB, User, Pass});
db_opts({mysql, Server, DB, User, Pass, SSLConnOpts}) ->
    db_opts({mysql, Server, ?MYSQL_PORT, DB, User, Pass, SSLConnOpts});
db_opts({mysql, Server, Port, DB, User, Pass, SSLConnOpts})
  when is_integer(Port) ->
    DBBasicOpts = get_db_basic_opts({Server, Port, DB, User, Pass}),
    extend_db_opts_with_ssl(DBBasicOpts, SSLConnOpts).

-spec get_db_basic_opts(Settings :: term()) -> [term()].
get_db_basic_opts({Server, Port, DB, User, Pass}) ->
    [
     {host, Server},
     {port, Port},
     {user, User},
     {password, Pass},
     {database, DB},
     {found_rows, true}
    ].

-spec extend_db_opts_with_ssl(Opts :: [term()], SSLConnOpts :: [term()]) -> [term()].
extend_db_opts_with_ssl(Opts, SSLConnOpts) ->
    Opts ++ [{ssl, SSLConnOpts}].

%% @doc Convert MySQL query result to Erlang ODBC result formalism
-spec mysql_to_odbc(mysql:query_result(), Conn :: term()) -> mongoose_rdbms:query_result().
mysql_to_odbc(ok, Conn) ->
    {updated, mysql:affected_rows(Conn)};
mysql_to_odbc({ok, _ColumnNames, Rows}, _Conn) ->
    {selected, [list_to_tuple(Row) || Row <- Rows]};
mysql_to_odbc({ok, Results}, Conn) ->
    [mysql_to_odbc({ok, Cols, Rows}, Conn) || {Cols, Rows} <- Results];
mysql_to_odbc({error, {1062, _SQLState, _Message}}, _Conn) ->
    {error, duplicate_key};
mysql_to_odbc({error, {_Code, _SQLState, Message}}, _Conn) ->
    {error, unicode:characters_to_list(Message)}.
