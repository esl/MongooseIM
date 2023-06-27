-module(mongoose_mnesia).
-export([create_table/2,
         create_schema/0,
         wait_for_tables/0]).

-include("mongoose.hrl").

-spec create_table(atom(), list()) -> ok | exists.
create_table(Table, Opts) ->
    case mnesia:system_info(is_running) of
        no ->
            report_mnesia_table_error(Table, Opts, mnesia_not_running);
        yes ->
            Res = mnesia:create_table(Table, Opts),
            check_create_table_result(Table, Opts, Res)
    end.

check_create_table_result(_Table, _Opts, {atomic, ok}) ->
    ok;
check_create_table_result(Table, Opts, {aborted, {already_exists, Table}}) ->
    maybe_add_copies(Table, Opts, disc_copies),
    maybe_add_copies(Table, Opts, disc_only_copies),
    maybe_add_copies(Table, Opts, ram_copies),
    exists;
check_create_table_result(Table, Opts, Res) ->
    report_mnesia_table_error(Table, Opts, Res).

report_mnesia_table_error(Table, Opts, Res) ->
    ?LOG_CRITICAL(#{what => mnesia_create_table_failed,
                    table => Table, create_opts => Opts, reason => Res,
                    schema_nodes => catch mnesia:table_info(schema, disc_copies)}),
    error({mnesia_create_table_failed, Table, Res}).

maybe_add_copies(Table, Opts, Type) ->
    case proplists:get_value(Type, Opts) of
        [_|_] ->
            mnesia:add_table_copy(Table, node(), Type);
        _ ->
            ok
    end.

create_schema() ->
    Res = mnesia:create_schema([node()]),
    check_schema_result(Res).

check_schema_result(ok) ->
    ok;
check_schema_result({error, {_, {already_exists, _}}}) ->
    ok;
check_schema_result(Res) ->
    ?LOG_CRITICAL(#{what => mnesia_create_schema_failed, reason => Res,
                    mnesia_info => mnesia:system_info(all)}),
    error({mnesia_create_schema_failed, Res}).

%% This function is wait_for_tables which also reports the loading progress
wait_for_tables() ->
    Tables = mnesia:system_info(local_tables),
    wait_for_mnesia_tables(Tables, 1).

wait_for_mnesia_tables(Tables, N) ->
    case mnesia:wait_for_tables(Tables, 5000) of
        {timeout, NotReadyTables} ->
            ?LOG_WARNING(#{what => mnesia_waiting_for_tables,
                           tables => NotReadyTables, iteration => N}),
            wait_for_mnesia_tables(NotReadyTables, N + 1);
        ok ->
            ok;
        {error, Reason} ->
             ?LOG_CRITICAL(#{what => mnesia_wait_for_tables_failed, reason => Reason}),
             error({mnesia_wait_for_tables_failed, Reason})
    end.
