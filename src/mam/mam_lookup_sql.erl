%% Makes a SELECT SQL query
-module(mam_lookup_sql).
-export([lookup_query/5]).

-include("mongoose_logger.hrl").
-include("mongoose_mam.hrl").

-type offset_limit() :: all | {Offset :: non_neg_integer(), Limit :: non_neg_integer()}.
-type sql_part() :: iolist() | binary().
-type env_vars() :: mod_mam_rdbms_arch:env_vars().
-type query_type() :: atom().
-type column() :: atom().
-type lookup_query_fn() :: fun((QueryType :: atom(), Env :: map(), Filters :: list(),
                                Order :: atom(), OffsetLimit :: offset_limit()) -> term()).

-export_type([sql_part/0]).
-export_type([query_type/0]).
-export_type([column/0]).
-export_type([lookup_query_fn/0]).

%% The ONLY usage of Env is in these functions:
%% The rest of code should treat Env as opaque (i.e. the code just passes Env around).
-spec host_type(env_vars()) -> mongooseim:host_type().
host_type(#{host_type := HostType}) -> HostType.

-spec table(env_vars()) -> atom().
table(#{table := Table}) -> Table.

-spec index_hint_sql(env_vars()) -> sql_part().
index_hint_sql(Env = #{index_hint_fn := F}) -> F(Env).

-spec columns_sql(env_vars(), query_type()) -> sql_part().
columns_sql(#{columns_sql_fn := F}, QueryType) -> F(QueryType).

-spec column_to_id(env_vars(), column()) -> string().
column_to_id(#{column_to_id_fn := F}, Col) -> F(Col).


%% This function uses some fields from Env:
%% - host_type
%% - table
%% - index_hint_fn
%% - columns_sql_fn
%% - column_to_id_fn
%%
%% Filters are in format {Op, Column, Value}
%% QueryType should be an atom, that we pass into the columns_sql_fn function.
-spec lookup_query(QueryType :: atom(), Env :: map(), Filters :: list(),
                   Order :: atom(), OffsetLimit :: offset_limit()) -> term().
lookup_query(QueryType, Env, Filters, Order, OffsetLimit) ->
    Table = table(Env),
    HostType = host_type(Env),
    StmtName = filters_to_statement_name(Env, QueryType, Table, Filters, Order, OffsetLimit),
    case mongoose_rdbms:prepared(StmtName) of
        false ->
            %% Create a new type of a query
            SQL = lookup_sql_binary(QueryType, Table, Env, Filters, Order, OffsetLimit),
            Columns = filters_to_columns(Filters, OffsetLimit),
            mongoose_rdbms:prepare(StmtName, Table, Columns, SQL);
        true ->
            ok
    end,
    Args = filters_to_args(Filters, OffsetLimit),
    mongoose_rdbms:execute_successfully(HostType, StmtName, Args).

lookup_sql_binary(QueryType, Table, Env, Filters, Order, OffsetLimit) ->
    iolist_to_binary(lookup_sql(QueryType, Table, Env, Filters, Order, OffsetLimit)).

lookup_sql(QueryType, Table, Env, Filters, Order, OffsetLimit) ->
    IndexHintSQL = index_hint_sql(Env),
    FilterSQL = filters_to_sql(Filters),
    OrderSQL = order_to_sql(Order),
    LimitSQL = limit_sql(OffsetLimit),
    ["SELECT ", columns_sql(Env, QueryType),
     " FROM ", atom_to_list(Table), " ",
     IndexHintSQL, FilterSQL, OrderSQL, LimitSQL].

limit_sql(all) -> <<>>;
limit_sql({0, _Limit}) -> rdbms_queries:limit();
limit_sql({_Offset, _Limit}) -> rdbms_queries:limit_offset().

filters_to_columns(Filters, OffsetLimit) ->
    offset_limit_to_columns(OffsetLimit, [Column || {_Op, Column, _Value} <- Filters]).

filters_to_args(Filters, OffsetLimit) ->
    offset_limit_to_args(OffsetLimit, [Value || {_Op, _Column, Value} <- Filters]).

offset_limit_to_args(all, Args) ->
    Args;
offset_limit_to_args({0, Limit}, Args) ->
    Args ++ [Limit];
offset_limit_to_args({Offset, Limit}, Args) ->
    Args ++ [Limit, Offset].

offset_limit_to_columns(all, Columns) ->
    Columns;
offset_limit_to_columns({0, _Limit}, Columns) ->
    Columns ++ [limit];
offset_limit_to_columns({_Offset, _Limit}, Columns) ->
    Columns ++ [limit, offset].

filters_to_statement_name(Env, QueryType, Table, Filters, Order, OffsetLimit) ->
    QueryId = query_type_to_id(QueryType),
    Ids = [op_to_id(Op) ++ column_to_id(Env, Col) || {Op, Col, _Val} <- Filters],
    OrderId = order_type_to_id(Order),
    LimitId = offset_limit_to_id(OffsetLimit),
    list_to_atom(atom_to_list(Table) ++ "_" ++ QueryId ++ "_" ++ OrderId ++ "_" ++ lists:append(Ids) ++ "_" ++ LimitId).

query_type_to_id(QueryType) -> atom_to_list(QueryType).

order_type_to_id(desc) -> "d";
order_type_to_id(asc) -> "a";
order_type_to_id(unordered) -> "u".

order_to_sql(asc) -> " ORDER BY id";
order_to_sql(desc) -> " ORDER BY id DESC";
order_to_sql(unordered) -> "".

offset_limit_to_id({0, _Limit}) -> "limit";
offset_limit_to_id({_Offset, _Limit}) -> "offlim";
offset_limit_to_id(all) -> "all".

filters_to_sql(Filters) ->
    SQLs = [filter_to_sql(Filter) || Filter <- Filters],
    case SQLs of
        [] -> "";
        Defined -> [" WHERE ", rdbms_queries:join(Defined, " AND ")]
    end.

-spec filter_to_sql(mam_filter:filter_field()) -> sql_part().
filter_to_sql({Op, Column, _Value}) -> filter_to_sql(atom_to_list(Column), Op).

op_to_id(equal)   -> "eq";
op_to_id(less)    -> "lt"; %% less than
op_to_id(greater) -> "gt"; %% greater than
op_to_id(le)      -> "le"; %% less or equal
op_to_id(ge)      -> "ge"; %% greater or equal
op_to_id(like)    -> "lk".

filter_to_sql(Column, equal)    -> Column ++ " = ?";
filter_to_sql(Column, less)     -> Column ++ " < ?";
filter_to_sql(Column, greater)  -> Column ++ " > ?";
filter_to_sql(Column, le)       -> Column ++ " <= ?";
filter_to_sql(Column, ge)       -> Column ++ " >= ?";
filter_to_sql(Column, like)     -> Column ++ " LIKE ?".
