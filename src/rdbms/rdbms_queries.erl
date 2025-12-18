%%%----------------------------------------------------------------------
%%% File    : rdbms_queries.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : RDBMS queries dependind on back-end
%%% Created : by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(rdbms_queries).
-author("mremond@process-one.net").

-export([begin_trans/0,
         limit/0,
         limit/1,
         limit_offset/0,
         sql_transaction/2,
         count_records_where/3,
         create_bulk_insert_query/3]).

-export([join/2,
         prepare_upsert/6,
         prepare_upsert/7,
         prepare_upsert_many/7,
         execute_upsert/4, execute_upsert/5,
         execute_upsert_many/4, execute_upsert_many/5,
         request_upsert/4]).

-ignore_xref([execute_upsert/5, count_records_where/3, execute_upsert_many/5]).

-include("mongoose.hrl").

%
%% -----------------
%% Common functions

join([], _Sep) ->
    [];
join([H|T], Sep) ->
    [H, [[Sep, X] || X <- T]].

%% -----------------
%% Generic queries

-spec execute_upsert(HostType :: mongooseim:host_type_or_global(),
                     Name :: atom(),
                     InsertParams :: [any()],
                     UpdateParams :: [any()]) -> mongoose_rdbms:query_result().
execute_upsert(HostType, Name, InsertParams, UpdateParams) ->
    execute_upsert(HostType, default, Name, InsertParams, UpdateParams).

-spec execute_upsert(HostType :: mongooseim:host_type_or_global(),
                     PoolTag :: mongoose_wpool:tag(),
                     Name :: atom(),
                     InsertParams :: [any()],
                     UpdateParams :: [any()]) -> mongoose_rdbms:query_result().
execute_upsert(HostType, PoolTag, Name, InsertParams, UpdateParams) ->
    case mongoose_rdbms:db_engine(HostType) of
        Driver when Driver =:= mysql; Driver =:= pgsql; Driver =:= cockroachdb ->
            mongoose_rdbms:execute(HostType, PoolTag, Name, InsertParams ++ UpdateParams);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

-spec execute_upsert_many(HostType :: mongooseim:host_type_or_global(),
                          Name :: atom(),
                          InsertParams :: [any()],
                          UpdateParams :: [any()]) -> mongoose_rdbms:query_result().
execute_upsert_many(HostType, Name, InsertParams, UpdateParams) ->
    execute_upsert_many(HostType, default, Name, InsertParams, UpdateParams).

-spec execute_upsert_many(HostType :: mongooseim:host_type_or_global(),
                          PoolTag :: mongoose_wpool:tag(),
                          Name :: atom(),
                          InsertParams :: [any()],
                          UpdateParams :: [any()]) -> mongoose_rdbms:query_result().
execute_upsert_many(HostType, PoolTag, Name, InsertParams, UpdateParams) ->
    case mongoose_rdbms:db_engine(HostType) of
        mysql ->
            mongoose_rdbms:execute(HostType, PoolTag, Name, InsertParams);
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            mongoose_rdbms:execute(HostType, PoolTag, Name, InsertParams ++ UpdateParams);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

-spec request_upsert(HostType :: mongooseim:host_type_or_global(),
                     Name :: atom(),
                     InsertParams :: [any()],
                     UpdateParams :: [any()]) -> gen_server:request_id().
request_upsert(HostType, Name, InsertParams, UpdateParams) ->
    case mongoose_rdbms:db_engine(HostType) of
        Driver when Driver =:= mysql; Driver =:= pgsql; Driver =:= cockroachdb ->
            mongoose_rdbms:execute_request(HostType, Name, InsertParams ++ UpdateParams);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

%% @doc
%% This functions prepares query for inserting a row or updating it if already exists
%% Updates can be either fields or literal expressions like "column = tab.column + 1"
-spec prepare_upsert(HostType :: mongooseim:host_type_or_global(),
                     QueryName :: mongoose_rdbms:query_name(),
                     TableName :: atom(),
                     InsertFields :: [binary()],
                     Updates :: [binary() | {assignment | expression, binary(), binary()}],
                     UniqueKeyFields :: [binary()]) ->
    {ok, QueryName :: mongoose_rdbms:query_name()} | {error, already_exists}.
prepare_upsert(HostType, Name, Table, InsertFields, Updates, UniqueKeyFields) ->
    prepare_upsert(HostType, Name, Table, InsertFields, Updates, UniqueKeyFields, none).

-spec prepare_upsert(HostType :: mongooseim:host_type_or_global(),
                     QueryName :: mongoose_rdbms:query_name(),
                     TableName :: atom(),
                     InsertFields :: [ColumnName :: binary()],
                     Updates :: [binary() | {assignment | expression, binary(), binary()}],
                     UniqueKeyFields :: [binary()],
                     IncrementalField :: none | binary()) ->
    {ok, QueryName :: mongoose_rdbms:query_name()} | {error, already_exists}.
prepare_upsert(HostType, Name, Table, InsertFields, Updates, UniqueKeyFields, IncrementalField) ->
    InsertFieldsTransformed = format_fields_for_db(HostType, InsertFields),
    UpdatesTransformed = format_fields_for_db(HostType, Updates),
    UniqueKeyFieldsTransformed = format_fields_for_db(HostType, UniqueKeyFields),
    IncrementalFieldTransformed = format_fields_for_db(HostType, IncrementalField),
    SQL = upsert_query(HostType, Table, InsertFieldsTransformed, UpdatesTransformed,
                       UniqueKeyFieldsTransformed, IncrementalFieldTransformed),
    Query = iolist_to_binary(SQL),
    ?LOG_DEBUG(#{what => rdbms_upsert_query, name => Name, query => Query}),
    Fields = prepared_upsert_fields(InsertFieldsTransformed, UpdatesTransformed),
    mongoose_rdbms:prepare(Name, Table, Fields, Query).

prepared_upsert_fields(InsertFields, Updates) ->
    UpdateFields = lists:filtermap(fun get_field_name/1, Updates),
    InsertFields ++ UpdateFields.

prepared_upsert_many_fields(RecordCount, InsertFields, Updates) ->
    InsertFieldsMany = lists:append(lists:duplicate(RecordCount, InsertFields)),
    UpdateFields = lists:filtermap(fun get_field_name/1, Updates),
    InsertFieldsMany ++ UpdateFields.

-spec prepare_upsert_many(HostType :: mongooseim:host_type_or_global(),
                          RecordCount :: pos_integer(),
                          QueryName :: mongoose_rdbms:query_name(),
                          TableName :: atom(),
                          InsertFields :: [ColumnName :: binary()],
                          Updates :: [binary() | {assignment | expression, binary(), binary()}],
                          UniqueKeyFields :: [binary()]) ->
    {ok, QueryName :: mongoose_rdbms:query_name()} | {error, already_exists}.
prepare_upsert_many(HostType, RecordCount, Name, Table, InsertFields, Updates, UniqueKeyFields) ->
    InsertFieldsTransformed = format_fields_for_db(HostType, InsertFields),
    UpdatesTransformed = format_fields_for_db(HostType, Updates),
    UniqueKeyFieldsTransformed = format_fields_for_db(HostType, UniqueKeyFields),
    SQL = upsert_query_many(HostType, RecordCount, Table, InsertFieldsTransformed,
                            UpdatesTransformed, UniqueKeyFieldsTransformed),
    Query = iolist_to_binary(SQL),
    ?LOG_DEBUG(#{what => rdbms_upsert_query, name => Name, query => Query}),
    Fields = prepared_upsert_many_fields(RecordCount, InsertFieldsTransformed, UpdatesTransformed),
    mongoose_rdbms:prepare(Name, Table, Fields, Query).

upsert_query(HostType, Table, InsertFields, Updates, UniqueKeyFields, IncrementalField) ->
    case mongoose_rdbms:db_engine(HostType) of
        mysql ->
            upsert_mysql_query(Table, InsertFields, Updates, UniqueKeyFields, IncrementalField);
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            upsert_pgsql_query(Table, InsertFields, Updates, UniqueKeyFields, IncrementalField);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

upsert_query_many(HostType, RecordCount, Table, InsertFields, Updates, UniqueKeyFields) ->
    case mongoose_rdbms:db_engine(HostType) of
        mysql ->
            upsert_many_mysql_query(RecordCount, Table, InsertFields);
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            upsert_many_pgsql_query(RecordCount, Table, InsertFields, Updates, UniqueKeyFields);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

mysql_and_pgsql_insert_many(RecordCount, Table, Columns) ->
    JoinedFields = join(Columns, <<", ">>),
    Placeholders = lists:duplicate(length(Columns), $?),
    Values = ["(", join(Placeholders, ", "), ")"],
    ManyValues = join(lists:duplicate(RecordCount, Values), ", "),
    ["INSERT INTO ", atom_to_binary(Table, utf8), " (",
     JoinedFields, ") VALUES ", ManyValues].

upsert_mysql_query(Table, InsertFields, Updates, [Key | _], IncrementalField) ->
    Insert = mysql_and_pgsql_insert_many(1, Table, InsertFields),
    OnConflict = mysql_on_conflict(Table, Updates, Key, IncrementalField),
    [Insert, OnConflict].

upsert_pgsql_query(Table, InsertFields, Updates, UniqueKeyFields, IncrementalField) ->
    Insert = mysql_and_pgsql_insert_many(1, Table, InsertFields),
    OnConflict = pgsql_on_conflict(Updates, UniqueKeyFields),
    WhereIncrements = pgsql_ensure_increments(Table, IncrementalField),
    [Insert, OnConflict, WhereIncrements].

upsert_many_mysql_query(RecordCount, Table, InsertFields) ->
    TableName = atom_to_list(Table),
    Columns = join(InsertFields, ", "),
    Placeholders = lists:duplicate(length(InsertFields), $?),
    Values = ["(", join(Placeholders, ", "), ")"],
    ManyValues = join(lists:duplicate(RecordCount, Values), ", "),
    ["REPLACE INTO ", TableName, " (", Columns, ") ",
     " VALUES ", ManyValues].

upsert_many_pgsql_query(RecordCount, Table, InsertFields, Updates, UniqueKeyFields) ->
    Insert = mysql_and_pgsql_insert_many(RecordCount, Table, InsertFields),
    OnConflict = pgsql_on_conflict(Updates, UniqueKeyFields),
    [Insert, OnConflict].

mysql_on_conflict(_Table, [], Key, _) ->
    %% Update field to itself (no-op), there is no 'DO NOTHING' in MySQL
    [" ON DUPLICATE KEY UPDATE ", Key, " = ", Key];
mysql_on_conflict(_Table, UpdateFields, _, none) ->
    [" ON DUPLICATE KEY UPDATE ",
     update_fields_on_conflict(UpdateFields)];
mysql_on_conflict(Table, UpdateFields, _, IncrementalField) ->
    TableName = atom_to_list(Table),
    FieldsWithPlaceHolders = [mysql_fields_with_placeholders(TableName, Update, IncrementalField)
                              || Update <- UpdateFields],
    IncrUpdates = join(FieldsWithPlaceHolders, ", "),
    [" AS alias ON DUPLICATE KEY UPDATE ", IncrUpdates].

mysql_fields_with_placeholders(TableName, UpdateField, IncrementalField) ->
    Alternatives = case UpdateField of
                       {Op, Column, Expression} when Op =:= assignment; Op =:= expression ->
                           [Expression, ", ", TableName, ".", Column, ")"];
                       Column ->
                           ["? , ", TableName, ".", Column, ")"]
                   end,
    [ Column, " = IF(", TableName, ".", IncrementalField, " < alias.", IncrementalField, ", "
      | Alternatives].

pgsql_on_conflict([], UniqueKeyFields) ->
    JoinedKeys = join(UniqueKeyFields, ", "),
    [" ON CONFLICT (", JoinedKeys, ") DO NOTHING"];
pgsql_on_conflict(UpdateFields, UniqueKeyFields) ->
    JoinedKeys = join(UniqueKeyFields, ", "),
    [" ON CONFLICT (", JoinedKeys, ")"
     " DO UPDATE SET ",
     update_fields_on_conflict(UpdateFields)].

update_fields_on_conflict(Updates) ->
    FieldsWithPlaceHolders = [get_field_expression(Update) || Update <- Updates],
    join(FieldsWithPlaceHolders, ", ").

pgsql_ensure_increments(_Table, none) ->
    [];
pgsql_ensure_increments(Table, IncrementalField) ->
    TableName = atom_to_list(Table),
    [" WHERE ", TableName, ".", IncrementalField, " < EXCLUDED.", IncrementalField].

get_field_expression({Op, ColumnName, Expr}) when Op =:= assignment; Op =:= expression ->
    [ColumnName, " = ", Expr];
get_field_expression(Field) when is_binary(Field) ->
    [Field, " = ?"].

get_field_name({assignment, Field, _}) when is_binary(Field) ->
    false;
get_field_name({expression, Field, _}) when is_binary(Field) ->
    {true, Field};
get_field_name(Field) when is_binary(Field) ->
    true.

%% F can be either a fun or a list of queries
%% TODO: We should probably move the list of queries transaction
%% wrapper from the mongoose_rdbms module to this one (rdbms_queries)
sql_transaction(LServer, F) ->
    mongoose_rdbms:sql_transaction(LServer, F).

begin_trans() ->
    [<<"BEGIN;">>].

%% Count number of records in a table given a where clause
count_records_where(LServer, Table, WhereClause) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select count(*) from ">>, Table, " ", WhereClause, ";"]).

-spec create_bulk_insert_query(Table :: iodata() | atom(), Fields :: [iodata() | atom()],
                               RowsNum :: pos_integer()) ->
    {iodata(), [binary()]}.
create_bulk_insert_query(Table, Fields, RowsNum) when is_atom(Table) ->
    create_bulk_insert_query(atom_to_binary(Table, utf8), Fields, RowsNum);
create_bulk_insert_query(Table, [Field | _] = Fields, RowsNum) when is_atom(Field) ->
    create_bulk_insert_query(Table, [atom_to_binary(F, utf8) || F <- Fields], RowsNum);
create_bulk_insert_query(Table, Fields, RowsNum) when RowsNum > 0 ->
    JoinedFields = join(Fields, <<", ">>),
    Placeholders = lists:duplicate(length(Fields), <<"?">>),
    PlaceholderSet = [<<"(">>, join(Placeholders, <<", ">>), <<")">>],
    PlaceholderSets = lists:duplicate(RowsNum, PlaceholderSet),
    JoinedPlaceholderSets = join(PlaceholderSets, <<", ">>),
    Sql = [<<"INSERT INTO ">>, Table, <<" (">>, JoinedFields, <<") "
       "VALUES ">>, JoinedPlaceholderSets, <<";">>],
    Fields2 = lists:append(lists:duplicate(RowsNum, Fields)),
    {Sql, Fields2}.

-spec limit() -> binary().
limit() ->
    <<" LIMIT ?">>.

-spec limit(integer()) -> binary().
limit(Limit) ->
    <<" LIMIT ", (integer_to_binary(Limit))/binary>>.

-spec limit_offset() -> binary().
limit_offset() ->
    <<" LIMIT ? OFFSET ?">>.

format_fields_for_db(_, none) ->
    none;
format_fields_for_db(HostType, Fields) when is_list(Fields) ->
    case mongoose_rdbms:db_engine(HostType) of
        cockroachdb ->
            lists:map(fun(Element) -> transform_field(Element) end, Fields);
        _ ->
            Fields
    end;
format_fields_for_db(HostType, Field) when is_binary(Field) ->
    case mongoose_rdbms:db_engine(HostType) of
        cockroachdb ->
            transform_field(Field);
        _ ->
            Field
    end.

transform_field({_, Field, _} = Element) ->
    erlang:setelement(2, Element, transform_field(Field));
transform_field(Field) when is_binary(Field)->
    <<"\"", Field/binary, "\"">>.
