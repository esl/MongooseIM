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

-module(mongoose_rdbms_odbc).
-author('konrad.zemek@erlang-solutions.com').
-behaviour(mongoose_rdbms).

-export([escape_binary/2, unescape_binary/2, connect/2, disconnect/1,
         query/3, prepare/6, execute/4]).

%% API

-spec escape_binary(mongoose_rdbms:pool(), binary()) -> iodata().
escape_binary(Pool, Bin) when is_binary(Bin) ->
    escape_binary(Pool, server_type(Pool), Bin).

-spec unescape_binary(mongoose_rdbms:pool(), binary()) -> binary().
unescape_binary(_Pool, Bin) when is_binary(Bin) ->
    bin_to_hex:hex_to_bin(Bin).

-spec connect(Args :: any(), QueryTimeout :: non_neg_integer()) ->
                     {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Settings, _QueryTimeout) when is_list(Settings) ->
    ok = application:ensure_started(odbc),
    case odbc:connect(Settings, [{scrollable_cursors, off}, {binary_strings, on}]) of
        {ok, Pid} ->
            link(Pid),
            {ok, Pid};
        Error ->
            Error
    end.

-spec disconnect(Connection :: term()) -> any().
disconnect(Connection) ->
    odbc:disconnect(Connection).

-spec query(Connection :: term(), Query :: any(),
            Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
query(Connection, Query, Timeout) when is_binary(Query) ->
    query(Connection, [Query], Timeout);
query(Connection, Query, Timeout) ->
    parse(odbc:sql_query(Connection, Query, Timeout)).

-spec prepare(Pool :: mongoose_rdbms:pool(),
              Connection :: term(), Name :: atom(), Table :: binary(),
              Fields :: [binary()], Statement :: iodata()) ->
                     {ok, {[binary()], [fun((term()) -> tuple())]}}.
prepare(Pool, Connection, _Name, Table, Fields, Statement) ->
    {ok, TableDesc} = odbc:describe_table(Connection, unicode:characters_to_list(Table)),
    SplitQuery = binary:split(iolist_to_binary(Statement), <<"?">>, [global]),
    ServerType = server_type(Pool),
    ParamMappers = [field_name_to_mapper(Pool, ServerType, TableDesc, Field) || Field <- Fields],
    {ok, {SplitQuery, ParamMappers}}.

-spec execute(Connection :: term(), Statement :: {[binary()], [fun((term()) -> tuple())]},
              Params :: [term()], Timeout :: infinity | non_neg_integer()) ->
                     mongoose_rdbms:query_result().
execute(Connection, {SplitQuery, ParamMapper}, Params, Timeout) ->
    {Query, ODBCParams} = unsplit_query(SplitQuery, ParamMapper, Params),
    parse(odbc:param_query(Connection, Query, ODBCParams, Timeout)).

%% Helpers

-spec parse(odbc:result_tuple() | [odbc:result_tuple()] | {error, string()}) ->
                   mongoose_rdbms:query_result().
parse(Items) when is_list(Items) ->
    [parse(Item) || Item <- Items];
parse({selected, _FieldNames, Rows}) ->
    {selected, Rows};
parse({error, "ERROR: duplicate key" ++ _}) ->
    {error, duplicate_key};
parse({error, Reason}) ->
    {error, unicode:characters_to_list(list_to_binary(Reason))};
parse(Other) ->
    Other.

-spec field_name_to_mapper(Pool :: mongoose_rdbms:pool(), ServerType :: atom(),
                           TableDesc :: proplists:proplist(),
                           FieldName :: binary()) -> fun((term()) -> tuple()).
field_name_to_mapper(Pool, ServerType, TableDesc, FieldName) ->
    {_, ODBCType} = lists:keyfind(unicode:characters_to_list(FieldName), 1, TableDesc),
    case ODBCType of
        T when T =:= 'SQL_BINARY'; T =:= 'SQL_VARBINARY'; T =:= 'SQL_LONGVARBINARY' ->
            fun(P) -> {[escape_binary(Pool, ServerType, P)], []} end;
        'SQL_LONGVARCHAR' ->
            fun(P) -> {[<<"'">>, mongoose_rdbms:escape(P), <<"'">>], []} end;
        'SQL_BIGINT' ->
            fun(P) -> {[<<"'">>, integer_to_binary(P), <<"'">>], []} end;
        _ ->
            fun(P) -> {<<"?">>, [{ODBCType, [P]}]} end
    end.

-spec unsplit_query(SplitQuery :: [binary()], ParamMappers :: [fun((term()) -> tuple())],
                    Params :: [term()]) -> {Query :: string(), ODBCParams :: [tuple()]}.
unsplit_query(SplitQuery, ParamMappers, Params) ->
    unsplit_query(SplitQuery, queue:from_list(ParamMappers), Params, [], []).

-spec unsplit_query(SplitQuery :: [binary()], ParamMappers :: queue:queue(fun((term()) -> tuple())),
                    Params :: [term()], QueryAcc :: [binary()], ParamsAcc :: [tuple()]) ->
                           {Query :: string(), ODBCParams :: [tuple()]}.
unsplit_query([QueryHead], _ParamMappers, [], QueryAcc, ParamsAcc) ->
    Query = unicode:characters_to_list(lists:reverse([QueryHead | QueryAcc])),
    Params = lists:reverse(ParamsAcc),
    {Query, Params};
unsplit_query([QueryHead | QueryRest], ParamMappers, [Param | Params], QueryAcc, ParamsAcc) ->
    {{value, Mapper}, ParamMappersTail} = queue:out(ParamMappers),
    NextParamMappers = queue:in(Mapper, ParamMappersTail),
    {InlineQuery, ODBCParam} = Mapper(Param),
    NewQueryAcc = [InlineQuery, QueryHead | QueryAcc],
    NewParamsAcc = ODBCParam ++ ParamsAcc,
    unsplit_query(QueryRest, NextParamMappers, Params, NewQueryAcc, NewParamsAcc).

-spec server_type(mongoose_rdbms:pool()) -> atom().
server_type(Pool) ->
    mongoose_rdbms_sup:get_option(Pool, odbc_server_type).

-spec escape_binary(mongoose_rdbms:pool(), ServerType :: atom(), binary()) -> iodata().
escape_binary(Pool, pgsql, Bin) ->
    mongoose_rdbms_pgsql:escape_binary(Pool, Bin);
escape_binary(Pool, mysql, Bin) ->
    mongoose_rdbms_mysql:escape_binary(Pool, Bin);
escape_binary(_Pool, _ServerType, Bin) ->
    [$', bin_to_hex:bin_to_hex(Bin), $'].
