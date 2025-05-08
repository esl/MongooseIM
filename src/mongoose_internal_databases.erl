-module(mongoose_internal_databases).

%% API
-export([init/0]).

-behaviour(mongoose_instrument_probe).
-export([probe/2]).

%% For tests
-export([instrumentation/0, wait_for_mnesia/0]).
-ignore_xref([instrumentation/0, wait_for_mnesia/0]).

-include("mongoose_logger.hrl").

-type db() :: mnesia | cets.

-spec init() -> ok.
init() ->
    %% Mnesia should not be running at this point, unless it is started by tests.
    %% Ensure Mnesia is stopped
    mnesia:stop(),
    maps:foreach(fun init/2, mongoose_config:get_opt(internal_databases)).

-spec init(db(), map()) -> ok.
init(mnesia, #{}) ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            mnesia:create_schema([node()]);
        _ ->
            ok
    end,
    application:ensure_all_started([mnesia], permanent),
    wait_for_mnesia(),
    mongoose_node_num_mnesia:init();
init(cets, #{}) ->
    ok.

wait_for_mnesia() ->
    wait_for_tables_loop(mnesia:system_info(local_tables), 10000, 0).

%% Sometimes mnesia:wait_for_tables/2 could hang on startup.
%% This function logs which tables are not ready and their status.
wait_for_tables_loop(Tables, Interval, Total) ->
    case mnesia:wait_for_tables(Tables, Interval) of
        ok ->
            ok;
        {timeout, WaitingTables} ->
            ?LOG_WARNING(#{what => mnesia_wait_for_tables_progress,
                           waiting_for_tables => WaitingTables,
                           waiting_time => Total + Interval}),
            [log_detailed_table_info(Tab) || Tab <- WaitingTables],
            wait_for_tables_loop(WaitingTables, Interval, Total + Interval);
        {error, Reason} ->
            error({mnesia_wait_for_tables_failed, Reason})
    end.

log_detailed_table_info(Tab) ->
    ?LOG_WARNING(#{what => log_detailed_table_info,
                   table => Tab,
                   details => mnesia:table_info(Tab, all)}).

-spec instrumentation() -> [mongoose_instrument:spec()].
instrumentation() ->
    lists:flatmap(fun instrumentation/1, maps:keys(mongoose_config:get_opt(internal_databases))).

-spec instrumentation(db()) -> [mongoose_instrument:spec()].
instrumentation(mnesia) ->
    [{mnesia_info, #{},
      #{metrics => #{db_nodes => gauge, running_db_nodes => gauge},
        probe => #{module => ?MODULE}}}];
instrumentation(cets) ->
    mongoose_instrument_probe_cets:instrumentation().

-spec probe(mongoose_instrument:event_name(), mongoose_instrument:labels()) ->
          mongoose_instrument:measurements().
probe(mnesia_info, #{}) ->
    DbNodes = mnesia:system_info(db_nodes),
    RunningDbNodes = mnesia:system_info(running_db_nodes),
    #{db_nodes => length(DbNodes), running_db_nodes => length(RunningDbNodes)}.
