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

-export([escape_binary/2, escape_string/2,
         unescape_binary/2, connect/2, disconnect/1,
         query/3, prepare/6, execute/4]).

%% API

-spec escape_binary(mongoose_rdbms:pool(), binary()) -> iodata().
escape_binary(Pool, Bin) when is_binary(Bin) ->
    escape_binary(Pool, server_type(Pool), Bin).

escape_string(Pool, Iolist) ->
    ServerType = server_type(Pool),
    escape_text(Pool, ServerType, iolist_to_binary(Iolist)).

-spec unescape_binary(mongoose_rdbms:pool(), binary()) -> binary().
unescape_binary(_Pool, Bin) when is_binary(Bin) ->
    bin_to_hex:hex_to_bin(Bin).

-spec connect(Args :: any(), QueryTimeout :: non_neg_integer()) ->
                     {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Settings, _QueryTimeout) when is_list(Settings) ->
    %% We need binary_strings=off to distinguish between:
    %% - UTF-16 encoded NVARCHARs - encoded as binaries.
    %% - Binaries/regular strings - encoded as list of small integers.
    %%
    %% It's not as efficient, as using binaries everywhere.
    %% But otherwise we should propose one of two patches to OTP's odbc driver:
    %% - Return UTF-16 strings as UTF-8
    %% - Return type information from sql_query
    %%
    %% More info:
    %% http://erlang.org/~raimo/doc-8.0.3/lib/odbc-2.11.2/doc/html/databases.html
    case eodbc:connect(Settings, [{scrollable_cursors, off},
                                  {binary_strings, on},
                                  {return_types, on}]) of
        {ok, Pid} ->
            link(Pid),
            {ok, Pid};
        Error ->
            Error
    end.

-spec disconnect(Connection :: term()) -> any().
disconnect(Connection) ->
    eodbc:disconnect(Connection).

-spec query(Connection :: term(), Query :: any(),
            Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
query(Connection, Query, Timeout) when is_binary(Query) ->
    query(Connection, [Query], Timeout);
query(Connection, Query, Timeout) ->
    parse(eodbc:sql_query(Connection, Query, Timeout)).

-spec prepare(Pool :: mongoose_rdbms:pool(),
              Connection :: term(), Name :: atom(), Table :: binary(),
              Fields :: [binary()], Statement :: iodata()) ->
                     {ok, {[binary()], [fun((term()) -> tuple())]}}.
prepare(Pool, Connection, _Name, Table, Fields, Statement) ->
    {ok, TableDesc} = eodbc:describe_table(Connection, unicode:characters_to_list(Table)),
    SplitQuery = binary:split(iolist_to_binary(Statement), <<"?">>, [global]),
    ServerType = server_type(Pool),
    ParamMappers = [field_name_to_mapper(Pool, ServerType, TableDesc, Field) || Field <- Fields],
    {ok, {SplitQuery, ParamMappers}}.

-spec execute(Connection :: term(), Statement :: {[binary()], [fun((term()) -> tuple())]},
              Params :: [term()], Timeout :: infinity | non_neg_integer()) ->
                     mongoose_rdbms:query_result().
execute(Connection, {SplitQuery, ParamMapper}, Params, Timeout) ->
    {Query, ODBCParams} = unsplit_query(SplitQuery, ParamMapper, Params),
    case eodbc:param_query(Connection, Query, ODBCParams, Timeout) of
        {error, Reason} ->
            Map = #{reason => Reason,
                    odbc_query => Query,
                    odbc_params => ODBCParams},
            {error, Map};
        Result ->
            parse(Result)
    end.


%% Helpers

-spec parse(eodbc:result_tuple() | [eodbc:result_tuple()] | {error, string()}) ->
                   mongoose_rdbms:query_result().
parse(Items) when is_list(Items) ->
    [parse(Item) || Item <- Items];
parse({selected, FieldTypeNames, Rows}) ->
    FieldsInfo = fields_to_parse_info(FieldTypeNames),
    {selected, parse_rows(Rows, FieldsInfo)};
parse({error, "ERROR: duplicate key" ++ _}) ->
    {error, duplicate_key};
parse({error, Reason}) when is_atom(Reason) ->
    {error, atom_to_list(Reason)};
parse({error, Reason}) ->
    {error, unicode:characters_to_list(list_to_binary(Reason))};
parse(Other) ->
    Other.

fields_to_parse_info(FieldTypeNames) ->
    [field_to_parse_info(FieldTypeName) || FieldTypeName <- FieldTypeNames].

field_to_parse_info({{sql_wvarchar,_}, _Name}) ->
    utf16;
field_to_parse_info(_) ->
    generic.

parse_rows(Rows, FieldsInfo) ->
    [list_to_tuple(parse_row(tuple_to_list(Row), FieldsInfo)) || Row <- Rows].

parse_row([null|Row], [_|FieldsInfo]) ->
    [null|parse_row(Row, FieldsInfo)];
parse_row([FieldValue|Row], [utf16|FieldsInfo]) ->
    %% Transorms UTF16 encoded NVARCHAR-s into utf8
    Decoded = unicode_characters_to_binary(FieldValue, {utf16, little}, utf8),
    [Decoded|parse_row(Row, FieldsInfo)];
parse_row([FieldValue|Row], [generic|FieldsInfo]) ->
    [FieldValue|parse_row(Row, FieldsInfo)];
parse_row([], []) ->
    [].

-spec field_name_to_mapper(Pool :: mongoose_rdbms:pool(), ServerType :: atom(),
                           TableDesc :: proplists:proplist(),
                           FieldName :: binary()) -> fun((term()) -> tuple()).
field_name_to_mapper(Pool, ServerType, TableDesc, FieldName) ->
    {_, ODBCType} = lists:keyfind(unicode:characters_to_list(FieldName), 1, TableDesc),
    case simple_type(just_type(ODBCType)) of
        binary ->
            fun(P) -> {[escape_binary(Pool, ServerType, P)], []} end;
        unicode ->
            fun(P) -> {[escape_text_or_integer(Pool, ServerType, P)], []} end;
        bigint ->
            fun(P) -> {[<<"'">>, integer_to_binary(P), <<"'">>], []} end;
        _ ->
            fun(P) -> {<<"?">>, [{ODBCType, [P]}]} end
    end.

simple_type('SQL_BINARY')           -> binary;
simple_type('SQL_VARBINARY')        -> binary;
simple_type('SQL_LONGVARBINARY')    -> binary;
simple_type('SQL_LONGVARCHAR')      -> unicode;
simple_type('sql_wvarchar')         -> unicode; %% nvarchar type in MSSQL
simple_type('sql_varchar')          -> unicode; %% encode ascii as unicode
simple_type('SQL_BIGINT')           -> bigint;
simple_type(_)                      -> generic.

%% Ignore type length
just_type({Type, _Len}) ->
    Type;
just_type(Type) ->
    Type.

-spec unsplit_query(SplitQuery :: [binary()], ParamMappers :: [fun((term()) -> tuple())],
                    Params :: [term()]) -> {Query :: string(), ODBCParams :: [tuple()]}.
unsplit_query(SplitQuery, ParamMappers, Params) ->
    unsplit_query(SplitQuery, queue:from_list(ParamMappers), Params, [], []).

-spec unsplit_query(SplitQuery :: [binary()], ParamMappers :: queue:queue(fun((term()) -> tuple())),
                    Params :: [term()], QueryAcc :: [binary()], ParamsAcc :: [tuple()]) ->
                           {Query :: string(), ODBCParams :: [tuple()]}.
unsplit_query([QueryHead], _ParamMappers, [], QueryAcc, ParamsAcc) ->
    %% Make a list of bytes
    Query = binary_to_list(iolist_to_binary(lists:reverse([QueryHead | QueryAcc]))),
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
escape_binary(_Pool, mssql, Bin) ->
    [<<"0x">>, bin_to_hex:bin_to_hex(Bin)];
escape_binary(_Pool, _ServerType, Bin) ->
    [$', bin_to_hex:bin_to_hex(Bin), $'].

%% boolean are of type {sql_varchar,5} in pgsql.
%% So, we need to handle integers.
%% But converting to integer would cause type check failure.
escape_text_or_integer(Pool, ServerType, P) when is_integer(P) ->
    [$', integer_to_list(P), $'];
escape_text_or_integer(Pool, ServerType, P) ->
    escape_text(Pool, ServerType, P).

-spec escape_text(mongoose_rdbms:pool(), ServerType :: atom(), binary()) -> iodata().
escape_text(_Pool, pgsql, Bin) ->
    escape_pgsql_string(Bin);
escape_text(_Pool, mssql, Bin) ->
    Utf16 = unicode_characters_to_binary(Bin, utf8, {utf16, little}),
    [<<"CAST(0x">>, bin_to_hex:bin_to_hex(Utf16), <<" AS NVARCHAR(max))">>];
escape_text(Pool, ServerType, Bin) ->
    escape_binary(Pool, ServerType, Bin).

unicode_characters_to_binary(Input, FromEncoding, ToEncoding) ->
    case unicode:characters_to_binary(Input, FromEncoding, ToEncoding) of
        Result when is_binary(Result) ->
            Result;
        Other ->
            erlang:error(#{event => parse_value_failed,
                           from_encoding => FromEncoding,
                           to_encoding => ToEncoding,
                           input_binary => Input,
                           output_result => Other})
    end.

escape_pgsql_string(Bin) ->
    [$', escape_pgsql_characters(Bin), $'].

%% Duplicate each single quaote
escape_pgsql_characters(Bin) when is_binary(Bin) ->
    binary:replace(Bin, <<"'">>, <<"''">>, [global]).
