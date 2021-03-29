-module(dynamic_domains_pm_SUITE).

%% API
-compile(export_all).
-import(distributed_helper, [mim/0, mim2/0, require_rpc_nodes/1, rpc/4]).

suite() ->
    require_rpc_nodes([mim, mim2]).


all() ->
    [can_authenticate].

init_per_suite(Config) ->
    Domain = <<"example.com">>,
    HostType = <<"test type">>,
    Source = dummy_source,
    ok = rpc(mim(), mongoose_domain_core, insert, [Domain, HostType, Source]),
    ok = rpc(mim2(), mongoose_domain_core, insert, [Domain, HostType, Source]),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    ok = rpc(mim(), mongoose_domain_core, delete, [<<"example.com">>]),
    ok = rpc(mim2(), mongoose_domain_core, delete, [<<"example.com">>]),
    escalus:end_per_suite(Config).

init_per_testcase(CN, Config) ->
    escalus:init_per_testcase(CN, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

can_authenticate(Config) ->
    UserSpecA = escalus_users:get_userspec(Config, alice3),
    {ok, ClientA, _} = escalus_connection:start(UserSpecA),
    UserSpecB = escalus_users:get_userspec(Config, bob3),
    {ok, ClientB, _} = escalus_connection:start(UserSpecB),
    escalus_connection:stop(ClientA),
    escalus_connection:stop(ClientB).
