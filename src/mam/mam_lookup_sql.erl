%% Makes a SELECT SQL query
-module(mam_lookup_sql).
-export([lookup_query/4]).

-include("mongoose_logger.hrl").
-include("mongoose_mam.hrl").


%% This function uses some fields from Env:
%% - host
%% - table
%% - index_hint_fn
%% - column_to_id_fn
%% - columns_sql_fn
%%
%% Filters are in format {Op, Column, Value}
%% QueryType should be an atom, that we pass into the columns_sql_fn function.
-spec lookup_query(QueryType :: atom(), Env :: map(), Filters :: list(), Order :: atom()) -> term().
lookup_query(QueryType, #{host := Host, table := Table} = Env, Filters, Order) ->
    StmtName = filters_to_statement_name(Env, QueryType, Filters, Order),
    case mongoose_rdbms:prepared(StmtName) of
        false ->
            %% Create a new type of a query
            SQL = lookup_sql_binary(QueryType, Env, Filters, Order),
            Columns = filters_to_columns(Filters),
            mongoose_rdbms:prepare(StmtName, Table, Columns, SQL);
        true ->
            ok
    end,
    Args = filters_to_args(Filters),
    try mongoose_rdbms:execute(Host, StmtName, Args) of
        {selected, Rs} when is_list(Rs) ->
            {selected, Rs};
        Error ->
            What = #{what => mam_lookup_failed, statement => StmtName,
                     sql_query => lookup_sql_binary(QueryType, Env, Filters, Order),
                     reason => Error, host => Host},
            ?LOG_ERROR(What),
            error(What)
    catch Class:Error:Stacktrace ->
            What = #{what => mam_lookup_failed, statement => StmtName,
                     sql_query => lookup_sql_binary(QueryType, Env, Filters, Order),
                     class => Class, stacktrace => Stacktrace,
                     reason => Error, host => Host},
            ?LOG_ERROR(What),
            erlang:raise(Class, Error, Stacktrace)
    end.

lookup_sql_binary(QueryType, Env, Filters, Order) ->
    iolist_to_binary(lookup_sql(QueryType, Env, Filters, Order)).

lookup_sql(QueryType, Env = #{index_hint_fn := IndexHintF, table := Table}, Filters, Order) ->
    IndexHintSQL = IndexHintF(Env),
    FilterSQL = filters_to_sql(Filters),
    OrderSQL = order_to_sql(Order),
    LimitSQL = limit_sql(Filters),
    ["SELECT ", columns_sql(Env, QueryType), " FROM ", atom_to_list(Table), " ",
     IndexHintSQL, FilterSQL, OrderSQL, LimitSQL].

%% Caller should provide both limit and offset fields in the correct order.
%% See limit_offset_filters.
%% No limits option is fine too (it is used with count and GDPR).
limit_sql(Filters) ->
    HasLimit = lists:keymember(limit, 1, Filters), %% and offset
    case HasLimit of
        true -> rdbms_queries:limit_offset_sql();
        false -> ""
    end.

filters_to_columns(Filters) ->
   [Column || {_Op, Column, _Value} <- Filters].

filters_to_args(Filters) ->
   [Value || {_Op, _Column, Value} <- Filters].

filters_to_statement_name(#{table := Table, column_to_id_fn := ColFn}, QueryType, Filters, Order) ->
    QueryId = query_type_to_id(QueryType),
    Ids = [op_to_id(Op) ++ ColFn(Col) || {Op, Col, _Val} <- Filters],
    OrderId = order_type_to_id(Order),
    list_to_atom(atom_to_list(Table) ++ "_" ++ QueryId ++ "_" ++ OrderId ++ "_" ++ lists:append(Ids)).

columns_sql(#{columns_sql_fn := F}, QueryType) -> F(QueryType).

query_type_to_id(QueryType) -> atom_to_list(QueryType).

order_type_to_id(desc) -> "d";
order_type_to_id(asc) -> "a";
order_type_to_id(unordered) -> "u".

order_to_sql(asc) -> " ORDER BY id ";
order_to_sql(desc) -> " ORDER BY id DESC ";
order_to_sql(unordered) -> " ".

filters_to_sql(Filters) ->
    SQLs = [filter_to_sql(Filter) || Filter <- Filters],
    case skip_undefined(SQLs) of
        [] -> "";
        Defined -> [" WHERE ", rdbms_queries:join(Defined, " AND ")]
    end.

skip_undefined(List) -> [X || X <- List, X =/= undefined].

filter_to_sql({Op, Column, _Value}) -> filter_to_sql(atom_to_list(Column), Op).

op_to_id(equal)   -> "eq";
op_to_id(lower)   -> "lt"; %% lower than
op_to_id(greater) -> "gt"; %% greater than
op_to_id(le)      -> "le"; %% lower or equal
op_to_id(ge)      -> "ge"; %% greater or equal
op_to_id(like)    -> "lk";
op_to_id(limit)   -> "li";
op_to_id(offset)  -> "of".

filter_to_sql(Column, equal)    -> Column ++ " = ?";
filter_to_sql(Column, lower)    -> Column ++ " < ?";
filter_to_sql(Column, greater)  -> Column ++ " > ?";
filter_to_sql(Column, le)       -> Column ++ " <= ?";
filter_to_sql(Column, ge)       -> Column ++ " >= ?";
filter_to_sql(Column, like)     -> Column ++ " LIKE ?";
filter_to_sql(_Column, limit)   -> undefined;
filter_to_sql(_Column, offset)  -> undefined.
