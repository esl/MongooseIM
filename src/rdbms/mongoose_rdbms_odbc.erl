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
-include("mongoose_logger.hrl").

-export([escape_binary/1, escape_string/1,
         unescape_binary/1, connect/2, disconnect/1,
         query/3, prepare/5, execute/4]).

%% API

-spec escape_binary(binary()) -> iodata().
escape_binary(Bin) when is_binary(Bin) ->
    escape_binary(server_type(), Bin).

escape_string(Iolist) ->
    ServerType = server_type(),
    escape_text(ServerType, iolist_to_binary(Iolist)).

-spec unescape_binary(binary()) -> binary().
unescape_binary(Bin) when is_binary(Bin) ->
    base16:decode(Bin).

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

-spec prepare(Connection :: term(), Name :: atom(), Table :: binary(),
              Fields :: [binary()], Statement :: iodata()) ->
                     {ok, {binary(), [fun((term()) -> tuple())]}}.
prepare(Connection, Name, Table, Fields, Statement) ->
    try prepare2(Connection, Table, Fields, Statement)
    catch Class:Reason:Stacktrace ->
              ?LOG_ERROR(#{what => prepare_failed,
                           statement_name => Name, sql_query => Statement,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
              erlang:raise(Class, Reason, Stacktrace)
    end.

prepare2(Connection, Table, Fields, Statement) ->
    {ok, TableDesc} = eodbc:describe_table(Connection, unicode:characters_to_list(Table)),
    ServerType = server_type(),
    ParamMappers = [field_name_to_mapper(ServerType, TableDesc, Field) || Field <- Fields],
    {ok, {iolist_to_binary(Statement), ParamMappers}}.

-spec execute(Connection :: term(), Statement :: {binary(), [fun((term()) -> tuple())]},
              Params :: [term()], Timeout :: infinity | non_neg_integer()) ->
                     mongoose_rdbms:query_result().
execute(Connection, {Query, ParamMapper}, Params, Timeout)
    when length(ParamMapper) =:= length(Params) ->
    ODBCParams = map_params(Params, ParamMapper),
    case eodbc:param_query(Connection, Query, ODBCParams, Timeout) of
        {error, Reason} ->
            Map = #{reason => Reason,
                    odbc_query => Query,
                    odbc_params => ODBCParams},
            {error, Map};
        Result ->
            parse(Result)
    end;
execute(Connection, {Query, ParamMapper}, Params, Timeout) ->
    ?LOG_ERROR(#{what => odbc_execute_failed,
                 params_length => length(Params),
                 mapped_length => length(ParamMapper),
                 connection => Connection,
                 sql_query => Query,
                 query_params => Params,
                 param_mapper => ParamMapper}),
    erlang:error({badarg, [Connection, {Query, ParamMapper}, Params, Timeout]}).

%% Helpers

-spec parse(eodbc:result_tuple() | [eodbc:result_tuple()] | {error, string()}) ->
                   mongoose_rdbms:query_result().
parse(Items) when is_list(Items) ->
    [parse(Item) || Item <- Items];
parse({selected, FieldTypeNames, Rows}) ->
    FieldsInfo = fields_to_parse_info(FieldTypeNames),
    {selected, parse_rows(Rows, FieldsInfo)};
parse({error, Reason}) when is_atom(Reason) ->
    {error, atom_to_list(Reason)};
parse({error, Reason}) ->
    ErrorStr = unicode:characters_to_list(list_to_binary(Reason)),
    case re:run(ErrorStr, "duplicate key") of
        nomatch -> {error, ErrorStr};
        {match, _} -> {error, duplicate_key}
    end;
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

-spec field_name_to_mapper(ServerType :: atom(),
                           TableDesc :: proplists:proplist(),
                           FieldName :: binary()) -> fun((term()) -> tuple()).
field_name_to_mapper(_ServerType, _TableDesc, <<"limit">>) ->
    fun(P) -> {sql_integer, [P]} end;
field_name_to_mapper(_ServerType, _TableDesc, <<"offset">>) ->
    fun(P) -> {sql_integer, [P]} end;
field_name_to_mapper(_ServerType, TableDesc, FieldName) ->
    ODBCType = field_to_odbc_type(unicode:characters_to_list(FieldName), TableDesc),
    case simple_type(just_type(ODBCType)) of
        binary ->
            fun(P) -> binary_mapper(P) end;
        unicode ->
            fun(P) -> unicode_mapper(P) end;
        bigint ->
            fun(P) -> bigint_mapper(P) end;
        _ ->
            fun(P) -> {ODBCType, [P]} end
    end.

field_to_odbc_type(FieldName, TableDesc) ->
    case lists:keyfind(FieldName, 1, TableDesc) of
        false ->
            ?LOG_ERROR(#{what => field_to_odbc_type_failed,
                         field => FieldName, table_desc => TableDesc}),
            error(field_to_odbc_type_failed);
        {_, ODBCType} ->
            ODBCType
    end.

unicode_mapper(P) ->
    Utf16 = unicode_characters_to_binary(iolist_to_binary(P), utf8, {utf16, little}),
    Len = byte_size(Utf16) div 2,
    {{sql_wlongvarchar, Len}, [Utf16]}.

bigint_mapper(P) when is_integer(P) ->
    B = integer_to_binary(P),
    Type = {'sql_varchar', byte_size(B)},
    {Type, [B]}.

binary_mapper(P) ->
    Type = {'sql_longvarbinary', byte_size(P)},
    {Type, [P]}.


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

map_params([Param|Params], [Mapper|Mappers]) ->
    [maybe_null(Param, Mapper)|map_params(Params, Mappers)];
map_params([], []) ->
    [].

maybe_null(undefined, _Mapper) ->
    {sql_integer, [null]}; %% some code uses "undefined" instead of "null"
maybe_null(null, _Mapper) ->
    {sql_integer, [null]}; %% Yeah, just random type for null
maybe_null(Param, Mapper) ->
    Mapper(Param).

-spec server_type() -> atom().
server_type() ->
    ejabberd_config:get_local_option(rdbms_server_type).

-spec escape_binary(ServerType :: atom(), binary()) -> iodata().
escape_binary(pgsql, Bin) ->
    mongoose_rdbms_pgsql:escape_binary(Bin);
escape_binary(mysql, Bin) ->
    mongoose_rdbms_mysql:escape_binary(Bin);
escape_binary(mssql, Bin) ->
    [<<"0x">>, base16:encode(Bin)];
escape_binary(_ServerType, Bin) ->
    [$', base16:encode(Bin), $'].

-spec escape_text(ServerType :: atom(), binary()) -> iodata().
escape_text(pgsql, Bin) ->
    escape_pgsql_string(Bin);
escape_text(mssql, Bin) ->
    Utf16 = unicode_characters_to_binary(Bin, utf8, {utf16, little}),
    [<<"CAST(0x">>, base16:encode(Utf16), <<" AS NVARCHAR(max))">>];
escape_text(ServerType, Bin) ->
    escape_binary(ServerType, Bin).

unicode_characters_to_binary(Input, FromEncoding, ToEncoding) ->
    case unicode:characters_to_binary(Input, FromEncoding, ToEncoding) of
        Result when is_binary(Result) ->
            Result;
        Other ->
            erlang:error(#{what => parse_value_failed,
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
