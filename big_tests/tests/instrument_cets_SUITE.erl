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
            instrument_helper:start(instrument_helper:declared_events(mongoose_metrics_probe_cets)),
            mongoose_helper:backup_and_set_config_option(Config, [instrumentation, probe_interval], 1)
    end.

end_per_suite(Config) ->
    instrument_helper:stop(),
    mongoose_helper:restore_config_option(Config, [instrumentation, probe_interval]).

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

check_instrumentation(Config) ->
    timer:sleep(5000),
    Rpc = distributed_helper:rpc(mim(), mongoose_instrument_event_table, all_keys, []),
    ct:fail("RPC ~p", [Rpc]),
    Res = instrument_helper:wait_for_new(cets_info, #{}),
    ct:fail(Res).
