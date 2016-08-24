-module(mongooseim_metrics_SUITE).

-include_lib("exml/include/exml.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() -> [no_skip_metric, subscriptions_initialised].

init_per_suite(C) ->
    application:load(exometer),
    application:set_env(exometer, mongooseim_report_interval, 1000),
    {Port, Socket} = carbon_cache_server:start(1, 0),
    Sup = spawn(fun() ->
                        mim_ct_sup:start_link(ejabberd_sup),
                        Hooks =
                        {ejabberd_hooks,
                         {ejabberd_hooks, start_link, []},
                         permanent,
                         brutal_kill,
                         worker,
                         [ejabberd_hooks]},
                        C2SSupervisor =
                        {ejabberd_c2s_sup,
                         {ejabberd_tmp_sup, start_link, [ejabberd_c2s_sup, ejabberd_c2s]},
                         permanent,
                         infinity,
                         supervisor,
                         [ejabberd_tmp_sup]},
                        supervisor:start_child(ejabberd_sup, Hooks),
                        supervisor:start_child(ejabberd_sup, C2SSupervisor),
                        receive
                            stop ->
                                ok
                        end
                end),
    Reporters = get_reporters_cfg(Port),
    application:set_env(exometer, report, Reporters),
    PortServer = carbon_cache_server:wait_for_accepting(),
    {ok, _Apps} = application:ensure_all_started(exometer),
    setup_meck(),
    exometer:new([carbon, packets], spiral),
    [{carbon_port, Port}, {test_sup, Sup}, {carbon_server, PortServer}, {carbon_socket, Socket} | C].

end_per_suite(C) ->
    Sup = ?config(test_sup, C),
    Sup ! stop,
    CarbonServer = ?config(carbon_server, C),
    erlang:exit(CarbonServer, kill),
    CarbonSocket = ?config(carbon_socket, C),
    gen_tcp:close(CarbonSocket),
    meck:unload(),
    application:stop(exometer),
    application:stop(exometer_core),
    C.

no_skip_metric(_C) ->
    ok = mongoose_metrics:create_generic_hook_metric(<<"localhost">>, sm_register_connection_hook),
    undefined = exometer:info([<<"localhost">>, sm_register_connection_hook]).


subscriptions_initialised(_C) ->
    mongoose_metrics:init(),
    true = wait_for_update(exometer:get_value([carbon, packets], count), 60).

wait_for_update({ok, [{count,X}]}, 0) ->
    X > 0;
wait_for_update({ok, [{count,X}]}, _N) when X > 0 ->
    true;
wait_for_update({ok, [{count,0}]}, N) ->
    timer:sleep(1000),
    wait_for_update(exometer:get_value([carbon, packets], count), N-1).

setup_meck() ->
    meck:new(ejabberd_config, [no_link]),
    meck:expect(ejabberd_config, get_global_option, fun(hosts) -> [<<"localhost">>] end),
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),
    ok.

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

