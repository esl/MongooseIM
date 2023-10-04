-module(mongoose_mnesia).
-export([create_table/2]).

-include("mongoose.hrl").

%% @doc A wrapper around `mnesia:create_table/2':
%% - Automatically adds table copies.
%% - Checks that mnesia is running or fail with an error.
%% - Checks result of create_table
-spec create_table(atom(), list()) ->
    {atomic, ok} | {aborted, {already_exists, atom()}}.
create_table(Table, Opts) ->
    case mnesia:system_info(is_running) of
        no ->
            report_mnesia_table_error(Table, Opts, mnesia_not_running);
        yes ->
            Res = mnesia:create_table(Table, Opts),
            check_create_table_result(Table, Opts, Res),
            Res
    end.

check_create_table_result(_Table, _Opts, {atomic, ok}) ->
    ok;
check_create_table_result(Table, Opts, {aborted, {already_exists, Table}}) ->
    [maybe_add_copies(Table, Opts, Type) || Type <- table_types()],
    ok;
check_create_table_result(Table, Opts, Res) ->
    report_mnesia_table_error(Table, Opts, Res).

table_types() ->
    [disc_copies, disc_only_copies, ram_copies].

report_mnesia_table_error(Table, Opts, Res) ->
    ?LOG_CRITICAL(#{what => mnesia_create_table_failed,
                    table => Table, create_opts => Opts, reason => Res,
                    schema_nodes => catch mnesia:table_info(schema, disc_copies)}),
    error({mnesia_create_table_failed, Table, Res}).

maybe_add_copies(Table, Opts, Type) ->
    case proplists:get_value(Type, Opts) of
        [_|_] ->
            mnesia:add_table_copy(Table, node(), Type),
            ok;
        _ ->
            ok
    end.
