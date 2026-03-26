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
         create_bulk_insert_query/3,
         add_interval_seconds_expr/1]).

-export([join/2,
         prepare_upsert/6,
         prepare_upsert/7,
         prepare_upsert_many/7,
         execute_upsert/4, execute_upsert/5,
         execute_upsert_many/4, execute_upsert_many/5,
         request_upsert/4]).

-export([prepare_update_returning/2,
         %% execute_update_returning MUST be called within a transaction,
         %% because of the way MySQL emulates UPDATE ... RETURNING
         %% via 2 queries: select-lock and bulk-update.
         execute_update_returning/4, execute_update_returning/5]).

-export([prepare_insert_returning_id/4,
         %% execute_insert_returning_id MUST be called within a transaction on MySQL,
         %% because it uses INSERT + SELECT LAST_INSERT_ID() as two separate queries.
         execute_insert_returning_id/3, execute_insert_returning_id/4]).

-ignore_xref([execute_upsert/5, count_records_where/3, execute_upsert_many/5,
              execute_update_returning/5, execute_insert_returning_id/4]).

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

%% Returns an engine-specific SQL expression for adding a parameterised number
%% of seconds to the current timestamp.
-spec add_interval_seconds_expr(mongooseim:host_type_or_global()) -> binary().
add_interval_seconds_expr(HostType) ->
    case mongoose_rdbms:db_engine(HostType) of
        mysql -> <<"DATE_ADD(NOW(), INTERVAL ? SECOND)">>;
        _ -> <<"now() + (? * interval '1 second')">>
    end.

%% -----------------
%% Update-returning queries
%%
%% Generates engine-specific SQL for UPDATE ... RETURNING on PG/CRDB
%% and an emulated select-lock / bulk-update sequence on MySQL.
%%
%% For PG/CRDB, join ON conditions are automatically merged into the WHERE clause.
%% For MySQL, join ON conditions stay in the JOIN ... ON clause.
%%
%% Returned values are captured BEFORE the update on MySQL. For columns not
%% modified by the update (e.g. primary keys), this is equivalent to RETURNING.
%% Must be called within a transaction on MySQL.

-type update_returning_spec() ::
    #{%% Query name (also base for MySQL derived statements).
      name          := mongoose_rdbms:query_name(),
      %% {TargetTable, TargetAlias}, e.g. {broadcast_jobs_ownership, <<"o">>}.
      table         := {atom(), binary()},
      %% binary() -> "column = ?" (consumes one update param).
      %% {Column, Expr} -> "column = Expr" (consumes one update param iff Expr has '?').
      update_fields := [binary() | {binary(), binary()}],
      %% Placeholder-backed fields used in `filters`.
      filter_fields := [binary()],
      %% Qualified columns to return, e.g. <<"o.broadcast_id">>.
      return_fields := [binary()],
      %% {JoinTable, JoinAlias, JoinOnExpr}.
      joins         := [{atom(), binary(), binary()}],
      %% Non-join WHERE predicates with '?' placeholders.
      filters       := binary()
     }.

-spec prepare_update_returning(mongooseim:host_type_or_global(), update_returning_spec()) ->
    {ok, mongoose_rdbms:query_name()} | {error, already_exists}.
prepare_update_returning(HostType, Spec) ->
    case mongoose_rdbms:db_engine(HostType) of
        mysql ->
            prepare_update_returning_mysql(HostType, Spec);
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            prepare_update_returning_pgsql(HostType, Spec);
        NotSupported ->
            erlang:error({rdbms_not_supported, NotSupported})
    end.

-spec execute_update_returning(mongooseim:host_type_or_global(),
                               Name :: mongoose_rdbms:query_name(),
                               UpdateParams :: [any()],
                               FilterParams :: [any()]) ->
    mongoose_rdbms:query_result().
execute_update_returning(HostType, Name, UpdateParams, FilterParams) ->
    execute_update_returning(HostType, default, Name, UpdateParams, FilterParams).

-spec execute_update_returning(mongooseim:host_type_or_global(),
                               PoolTag :: mongoose_wpool:tag(),
                               Name :: mongoose_rdbms:query_name(),
                               UpdateParams :: [any()],
                               FilterParams :: [any()]) ->
    mongoose_rdbms:query_result().
execute_update_returning(HostType, PoolTag, Name, UpdateParams, FilterParams) ->
    case mongoose_rdbms:db_engine(HostType) of
        mysql ->
            execute_update_returning_mysql(HostType, PoolTag, Name, UpdateParams, FilterParams);
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            execute_update_returning_pgsql(HostType, PoolTag, Name, UpdateParams, FilterParams);
        NotSupported ->
            erlang:error({rdbms_not_supported, NotSupported})
    end.

%% --- PG/CockroachDB: single UPDATE ... FROM ... WHERE ... RETURNING ---

prepare_update_returning_pgsql(HostType, Spec) ->
    #{name := Name, table := {Table, Alias},
      update_fields := Updates, filter_fields := FilterFields,
      return_fields := ReturnFields, joins := Joins, filters := Filters} = Spec,
    UpdatesT = format_fields_for_db(HostType, Updates),
    SetClause = ur_set_clause(Alias, UpdatesT),
    FromClause = pgsql_ur_from_clause(Joins),
    WhereClause = pgsql_ur_where_clause(Joins, Filters),
    ReturnClause = join(ReturnFields, <<", ">>),
    SQL = ["UPDATE ", atom_to_binary(Table, utf8), " ", Alias,
           " SET ", SetClause,
           FromClause,
           " WHERE ", WhereClause,
           " RETURNING ", ReturnClause],
    Query = iolist_to_binary(SQL),
    ?LOG_DEBUG(#{what => rdbms_update_returning_query, name => Name, query => Query}),
    UpdateParamFields = lists:filtermap(fun ur_get_update_param_field/1, UpdatesT),
    Fields = UpdateParamFields ++ FilterFields,
    mongoose_rdbms:prepare(Name, Table, Fields, Query).

ur_get_update_param_field({Field, Expr}) when is_binary(Field), is_binary(Expr) ->
    case binary:match(Expr, <<"?">>) of
        nomatch -> false;
        _ -> {true, Field}
    end;
ur_get_update_param_field(Field) when is_binary(Field) ->
    true.

pgsql_ur_from_clause([]) ->
    [];
pgsql_ur_from_clause(Joins) ->
    JoinParts = [[atom_to_binary(JT, utf8), " ", JA] || {JT, JA, _On} <- Joins],
    [" FROM ", join(JoinParts, ", ")].

pgsql_ur_where_clause([], Filters) ->
    Filters;
pgsql_ur_where_clause(Joins, Filters) ->
    JoinConditions = [On || {_, _, On} <- Joins],
    join(JoinConditions ++ [Filters], <<" AND ">>).

execute_update_returning_pgsql(HostType, PoolTag, Name, UpdateParams, FilterParams) ->
    mongoose_rdbms:execute_successfully(HostType, PoolTag, Name, UpdateParams ++ FilterParams).

%% --- MySQL: emulated via select-lock / bulk-update in caller's transaction ---

mysql_derived_name(BaseName, Suffix) ->
    list_to_atom(atom_to_list(BaseName) ++ Suffix).

prepare_update_returning_mysql(HostType, Spec) ->
    #{name := Name, table := {Table, Alias},
      update_fields := Updates, filter_fields := FilterFields,
      return_fields := ReturnFields, joins := Joins, filters := Filters} = Spec,
    UpdatesT = format_fields_for_db(HostType, Updates),
    JoinClause = mysql_ur_join_clause(Joins),

    %% 1. SELECT return_fields ... FOR UPDATE (lock rows and capture return values)
    LockName = mysql_derived_name(Name, "_lock"),
    ReturnSelect = join(ReturnFields, <<", ">>),
    LockSQL = iolist_to_binary(
        ["SELECT ", ReturnSelect,
         " FROM ", atom_to_binary(Table, utf8), " ", Alias,
         JoinClause,
         " WHERE ", Filters,
         " FOR UPDATE"]),
    ?LOG_DEBUG(#{what => rdbms_update_returning_mysql_lock, name => LockName, query => LockSQL}),
    mongoose_rdbms:prepare(LockName, Table, FilterFields, LockSQL),

    %% 2. UPDATE ... JOIN ... SET ... WHERE filters (bulk update the same rows)
    UpdateName = mysql_derived_name(Name, "_update"),
    SetClause = ur_set_clause(Alias, UpdatesT),
    UpdateSQL = iolist_to_binary(
        ["UPDATE ", atom_to_binary(Table, utf8), " ", Alias,
         JoinClause,
         " SET ", SetClause,
         " WHERE ", Filters]),
    ?LOG_DEBUG(#{what => rdbms_update_returning_mysql_update, name => UpdateName, query => UpdateSQL}),
    UpdateParamFields = lists:filtermap(fun ur_get_update_param_field/1, UpdatesT),
    mongoose_rdbms:prepare(UpdateName, Table, UpdateParamFields ++ FilterFields, UpdateSQL),

    {ok, Name}.

mysql_ur_join_clause([]) ->
    [];
mysql_ur_join_clause(Joins) ->
    [[" JOIN ", atom_to_binary(JT, utf8), " ", JA, " ON ", On]
     || {JT, JA, On} <- Joins].

ur_set_clause(Alias, Updates) ->
    Exprs = [ur_set_field(Alias, U) || U <- Updates],
    join(Exprs, <<", ">>).

ur_set_field(Alias, {Col, Expr}) when is_binary(Col), is_binary(Expr) ->
    [Alias, ".", Col, " = ", Expr];
ur_set_field(Alias, Col) when is_binary(Col) ->
    [Alias, ".", Col, " = ?"].

execute_update_returning_mysql(HostType, PoolTag, Name, UpdateParams, FilterParams) ->
    LockName = mysql_derived_name(Name, "_lock"),
    UpdateName = mysql_derived_name(Name, "_update"),
    %% Step 1: lock rows and capture return values
    {selected, LockedRows} = mongoose_rdbms:execute_successfully(HostType, PoolTag, LockName, FilterParams),
    case LockedRows of
        [] ->
            {updated, 0, []};
        _ ->
            %% Step 2: bulk update the same rows (same filter, same params)
            mongoose_rdbms:execute_successfully(HostType, PoolTag, UpdateName,
                                   UpdateParams ++ FilterParams),
            {updated, length(LockedRows), LockedRows}
    end.

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
transform_field({Field, Expr}) when is_binary(Field), is_binary(Expr) ->
    {transform_field(Field), Expr};
transform_field(Field) when is_binary(Field)->
    <<"\"", Field/binary, "\"">>.

%% -----------------
%% Insert-returning-id queries
%%
%% Helper for single-row INSERT into a table with an auto-generated `id`
%% column (SERIAL / AUTO_INCREMENT). Returns {ok, Id}.
%%
%% PG/CockroachDB: INSERT ... RETURNING id  (single prepared statement).
%% MySQL: INSERT + SELECT id WHERE id = LAST_INSERT_ID()  (two statements,
%%        must be called within a transaction for correct results).

-spec prepare_insert_returning_id(mongooseim:host_type_or_global(),
                                  QueryName :: mongoose_rdbms:query_name(),
                                  TableName :: atom(),
                                  InsertFields :: [binary()]) ->
    {ok, mongoose_rdbms:query_name()} | {error, already_exists}.
prepare_insert_returning_id(HostType, Name, Table, InsertFields) ->
    case mongoose_rdbms:db_engine(HostType) of
        mysql ->
            prepare_insert_returning_id_mysql(Name, Table, InsertFields);
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            prepare_insert_returning_id_pgsql(HostType, Name, Table, InsertFields);
        NotSupported ->
            erlang:error({rdbms_not_supported, NotSupported})
    end.

-spec execute_insert_returning_id(mongooseim:host_type_or_global(),
                                  Name :: mongoose_rdbms:query_name(),
                                  InsertParams :: [any()]) ->
    {ok, integer()}.
execute_insert_returning_id(HostType, Name, InsertParams) ->
    execute_insert_returning_id(HostType, default, Name, InsertParams).

-spec execute_insert_returning_id(mongooseim:host_type_or_global(),
                                  PoolTag :: mongoose_wpool:tag(),
                                  Name :: mongoose_rdbms:query_name(),
                                  InsertParams :: [any()]) ->
    {ok, integer()}.
execute_insert_returning_id(HostType, PoolTag, Name, InsertParams) ->
    case mongoose_rdbms:db_engine(HostType) of
        mysql ->
            execute_insert_returning_id_mysql(HostType, PoolTag, Name, InsertParams);
        Driver when Driver =:= pgsql; Driver =:= cockroachdb ->
            execute_insert_returning_id_pgsql(HostType, PoolTag, Name, InsertParams);
        NotSupported ->
            erlang:error({rdbms_not_supported, NotSupported})
    end.

%% --- PG/CockroachDB: INSERT ... RETURNING id ---

prepare_insert_returning_id_pgsql(HostType, Name, Table, InsertFields) ->
    FieldsT = format_fields_for_db(HostType, InsertFields),
    JoinedFields = join(FieldsT, <<", ">>),
    Placeholders = join(lists:duplicate(length(FieldsT), <<"?">>), <<", ">>),
    TableBin = atom_to_binary(Table, utf8),
    SQL = iolist_to_binary(
        ["INSERT INTO ", TableBin, " (", JoinedFields, ") VALUES (",
         Placeholders, ") RETURNING id"]),
    ?LOG_DEBUG(#{what => rdbms_insert_returning_id_query, name => Name, query => SQL}),
    mongoose_rdbms:prepare(Name, Table, FieldsT, SQL).

execute_insert_returning_id_pgsql(HostType, PoolTag, Name, InsertParams) ->
    {updated, _Count, [{Id}]} = mongoose_rdbms:execute_successfully(HostType, PoolTag, Name, InsertParams),
    {ok, mongoose_rdbms:result_to_integer(Id)}.

%% --- MySQL: INSERT then SELECT LAST_INSERT_ID() ---

prepare_insert_returning_id_mysql(Name, Table, InsertFields) ->
    TableBin = atom_to_binary(Table, utf8),
    JoinedFields = join(InsertFields, <<", ">>),
    Placeholders = join(lists:duplicate(length(InsertFields), <<"?">>), <<", ">>),

    %% 1. Plain INSERT
    InsertName = mysql_derived_name(Name, "_insert"),
    InsertSQL = iolist_to_binary(
        ["INSERT INTO ", TableBin, " (", JoinedFields, ") VALUES (",
         Placeholders, ")"]),
    ?LOG_DEBUG(#{what => rdbms_insert_returning_id_mysql_insert,
                 name => InsertName, query => InsertSQL}),
    mongoose_rdbms:prepare(InsertName, Table, InsertFields, InsertSQL),

    %% 2. SELECT by LAST_INSERT_ID()
    SelectName = mysql_derived_name(Name, "_last_id"),
    SelectSQL = iolist_to_binary(
        ["SELECT id FROM ", TableBin, " WHERE id = LAST_INSERT_ID()"]),
    ?LOG_DEBUG(#{what => rdbms_insert_returning_id_mysql_select,
                 name => SelectName, query => SelectSQL}),
    mongoose_rdbms:prepare(SelectName, Table, [], SelectSQL),
    {ok, Name}.

execute_insert_returning_id_mysql(HostType, PoolTag, Name, InsertParams) ->
    InsertName = mysql_derived_name(Name, "_insert"),
    SelectName = mysql_derived_name(Name, "_last_id"),
    {updated, 1} = mongoose_rdbms:execute_successfully(HostType, PoolTag, InsertName, InsertParams),
    {selected, [{Id}]} = mongoose_rdbms:execute_successfully(HostType, PoolTag, SelectName, []),
    {ok, mongoose_rdbms:result_to_integer(Id)}.