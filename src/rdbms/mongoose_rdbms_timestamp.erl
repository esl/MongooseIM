-module(mongoose_rdbms_timestamp).
-export([prepare/0,
         select/0]).

-spec prepare() -> ok.
prepare() ->
    mongoose_rdbms:prepare(mim_timestamp, users, [], select_query()),
    ok.

select_query() ->
   case {mongoose_rdbms:db_engine(global), mongoose_rdbms:db_type()} of
       {mysql, _} ->
           <<"SELECT UNIX_TIMESTAMP()">>;
       {pgsql, _} ->
           <<"SELECT ROUND(extract(epoch from now()))">>;
       {odbc, mssql} ->
           <<"SELECT DATEDIFF_BIG(second, '1970-01-01 00:00:00', GETUTCDATE())">>;
       Other ->
           error({prepare_timestamp_query_failed, Other})
   end.

-spec select() -> integer().
select() ->
    Res = mongoose_rdbms:execute_successfully(global, mim_timestamp, []),
    mongoose_rdbms:selected_to_integer(Res). %% ensure it is an integer
