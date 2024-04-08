-module(mongoose_internal_databases).
-export([init/0]).

init() ->
    case mongoose_config:lookup_opt([internal_databases, mnesia]) of
        {ok, _} ->
            init_mnesia(),
            mongoose_node_num_mnesia:init();
        {error, _} ->
            %% Ensure mnesia is stopped when applying the test presets from the big tests.
            %% So, we accidentually do not test with mnesia enabled, when starting the
            %% test cases from the clean test build.
            mnesia:stop(),
            ok
    end.

init_mnesia() ->
    %% Mnesia should not be running at this point, unless it is started by tests.
    %% Ensure Mnesia is stopped
    mnesia:stop(),
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            mnesia:create_schema([node()]);
        _ ->
            ok
    end,
    application:start(mnesia, permanent),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).
