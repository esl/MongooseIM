-module(mongooseim_metrics_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [
     {group, ordinary_mode},
     {group, all_metrics_are_global}
    ].

groups() ->
    [
     {ordinary_mode, [], all_metrics_list()},
     {all_metrics_are_global, [], all_metrics_list()}
    ].

all_metrics_list() ->
    [
     subscriptions_initialised
    ].

init_per_suite(C) ->
    application:load(exometer_core),
    application:set_env(exometer_core, mongooseim_report_interval, 1000),
    {Port, Socket} = carbon_cache_server:start(),
    Reporters = get_reporters_cfg(Port),
    application:set_env(exometer_core, report, Reporters),
    PortServer = carbon_cache_server:wait_for_accepting(),
    gen_tcp:controlling_process(Socket, PortServer),
    {ok, _Apps} = application:ensure_all_started(exometer_core),
    exometer:new([carbon, packets], spiral),
    [{carbon_port, Port}, {carbon_server, PortServer}, {carbon_socket, Socket} | C].

end_per_suite(C) ->
    CarbonServer = ?config(carbon_server, C),
    erlang:exit(CarbonServer, kill),
    CarbonSocket = ?config(carbon_socket, C),
    gen_tcp:close(CarbonSocket),
    application:stop(exometer_core),
    C.

init_per_group(Group, C) ->
    mongoose_config:set_opts(opts(Group)),
    C1 = async_helper:start(C, mongoose_instrument, start_link, []),
    mongoose_system_probes:start(), % required to have some metrics to report
    mongoose_metrics:init_mongooseim_metrics(),
    C1.

end_per_group(_Group, C) ->
    mongoose_system_probes:stop(),
    async_helper:stop_all(C),
    mongoose_config:erase_opts().

subscriptions_initialised(_C) ->
    true = wait_for_update(exometer:get_value([carbon, packets], count), 60).

wait_for_update({ok, [{count,X}]}, 0) ->
    X > 0;
wait_for_update({ok, [{count,X}]}, _N) when X > 0 ->
    true;
wait_for_update({ok, [{count,0}]}, N) ->
    timer:sleep(1000),
    wait_for_update(exometer:get_value([carbon, packets], count), N-1).

opts(Group) ->
    AllGlobal = Group =:= all_metrics_are_global,
    InstrConfig = #{exometer => #{all_metrics_are_global => AllGlobal}},
    #{all_metrics_are_global => AllGlobal, % legacy, remove with mongoose_metrics
      hosts => [<<"localhost">>],
      host_types => [],
      internal_databases => #{},
      instrumentation => config_parser_helper:config([instrumentation], InstrConfig)}.

get_reporters_cfg(Port) ->
    [{reporters, [
                 {exometer_report_graphite, [
                                             {prefix, "mongooseim"},
                                             {connect_timeout, 10000},
                                             {host, "127.0.0.1"},
                                             {port, Port},
                                             {api_key, ""}
                                            ]}
                ]}].
