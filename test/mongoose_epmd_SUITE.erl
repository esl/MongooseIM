-module(mongoose_epmd_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [lookup_ip_when_config_not_loaded,
     lookup_ip_when_cets_disabled,
     lookup_ip_when_cets_enabled].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(lookup_ip_when_config_not_loaded, Config) ->
    %% Ensure config is not loaded
    mongoose_config:erase_opts(),
    meck:new(cets_discovery, [no_link]),
    Config;
init_per_testcase(_TestCase, Config) ->
    %% Start with empty config
    mongoose_config:set_opts(#{}),
    meck:new(cets_discovery, [no_link]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(cets_discovery),
    mongoose_config:erase_opts(),
    ok.

%% When config is not yet loaded (during node startup), lookup_ip should return an error
%% without crashing. This happens when mongoose_epmd:address_please is called by OTP's
%% net_kernel before MongooseIM's config is loaded.
lookup_ip_when_config_not_loaded(_Config) ->
    Result = mongoose_epmd:lookup_ip(<<"node@host">>),
    ?assertEqual({error, config_not_loaded}, Result),
    %% Verify cets_discovery was never called
    ?assertEqual(0, meck:num_calls(cets_discovery, wait_for_get_nodes, '_')),
    ?assertEqual(0, meck:num_calls(cets_discovery, system_info, '_')).

%% When CETS is disabled, lookup_ip should return an error without calling cets_discovery.
%% This avoids the noisy error log from cets_long:run_tracked when mongoose_epmd is
%% configured but CETS is not.
lookup_ip_when_cets_disabled(_Config) ->
    %% Config is loaded but internal_databases.cets is not set (defaults to disabled)
    Result = mongoose_epmd:lookup_ip(<<"node@host">>),
    ?assertEqual({error, cets_not_configured}, Result),
    %% Verify cets_discovery was never called
    ?assertEqual(0, meck:num_calls(cets_discovery, wait_for_get_nodes, '_')),
    ?assertEqual(0, meck:num_calls(cets_discovery, system_info, '_')).

%% When CETS is enabled, lookup_ip should call cets_discovery
lookup_ip_when_cets_enabled(_Config) ->
    CetsConfig = #{backend => rdbms, cluster_name => test},
    mongoose_config:set_opt(internal_databases, #{cets => CetsConfig}),
    meck:expect(cets_discovery, wait_for_get_nodes,
                fun(mongoose_cets_discovery, 3000) -> ok end),
    meck:expect(cets_discovery, system_info,
                fun(mongoose_cets_discovery) ->
                    #{backend_state => #{address_pairs => #{<<"node@host">> => <<"192.0.2.1">>}}}
                end),
    Result = mongoose_epmd:lookup_ip(<<"node@host">>),
    ?assertEqual({ok, {192, 0, 2, 1}}, Result),
    %% Verify cets_discovery was called
    ?assertEqual(1, meck:num_calls(cets_discovery, wait_for_get_nodes, '_')),
    ?assertEqual(1, meck:num_calls(cets_discovery, system_info, '_')).
