-module(mongoose_internal_databases).
-export([init/0,
         configured_internal_databases/0,
         running_internal_databases/0]).

-ignore_xref([configured_internal_databases/0,
              running_internal_databases/0]).

init() ->
    case mongoose_config:lookup_opt([internal_databases, mnesia]) of
        {ok, _} ->
            init_mnesia(),
            mongoose_node_num_mnesia:init();
        {error, _} ->
            %% Ensure mnesia is stopped when applying the test presets from the big tests.
            %% So, we accidentually do not test with mnesia enabled, when starting the
            %% test cases from the clean test build.
            %% TODO Stopping here would break a lot of tests, stop here once tests are fixed.
            % mnesia:stop(),
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

configured_internal_databases() ->
    case mongoose_config:lookup_opt([internal_databases, mnesia]) of
        {ok, _} ->
            [mnesia];
        {error, _} ->
            []
    end ++
    case mongoose_config:lookup_opt([internal_databases, cets]) of
        {ok, _} ->
            [cets];
        {error, _} ->
            []
    end.

running_internal_databases() ->
    case mnesia:system_info(is_running) of
        yes ->
            [mnesia];
        no ->
            []
    end ++
    case is_pid(whereis(mongoose_cets_discovery)) of
        true ->
            [cets];
        false ->
            []
    end.
