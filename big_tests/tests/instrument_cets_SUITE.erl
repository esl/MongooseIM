-module(instrument_cets_SUITE).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, mim2/0, rpc/4]).
-import(domain_helper, [host_type/1]).
-import(mongooseimctl_helper, [rpc_call/3]).

all() ->
    [{group, instrument}].

groups() ->
    [{instrument, [], cases()}].

cases() ->
    [check_instrumentation].

init_per_suite(Config) ->
    case rpc_call(mongoose_config, lookup_opt, [[internal_databases, cets]]) of
        {error, not_found} ->
            {skip, "CETS is not configured"};
        {ok, _} ->
            instrument_helper:start([{cets_info, #{}}]),
            Config2 = mongoose_helper:backup_and_set_config_option(Config, [instrumentation, probe_interval], 1),
            %% We have to restart probes after setting probe_interval
            restart_cets_probe(),
            Config2
    end.

end_per_suite(Config) ->
    instrument_helper:stop(),
    mongoose_helper:restore_config_option(Config, [instrumentation, probe_interval]),
    restart_cets_probe().

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

check_instrumentation(Config) ->
    instrument_helper:wait_for_new(cets_info, #{}),
    instrument_helper:assert(cets_info, #{}, fun(Res) ->
            %% Values are integers
            lists:all(fun(Name) -> is_integer(maps:get(Name, Res)) end, instrumentation_metrics_names())
            andalso
            %% Check that there are no unknown fields
            [] =:= maps:keys(maps:without(instrumentation_metrics_names(), Res))
        end).

instrumentation_metrics_names() ->
    [available_nodes,
     unavailable_nodes,
     joined_nodes,
     discovered_nodes,
     discovery_works,
     remote_nodes_without_disco,
     remote_nodes_with_unknown_tables,
     remote_unknown_tables,
     remote_nodes_with_missing_tables,
     remote_missing_tables,
     conflict_nodes,
     conflict_tables].

restart_cets_probe() ->
    rpc_call(mongoose_instrument_probe_cets, stop, []),
    rpc_call(mongoose_instrument_probe_cets, start, []).
