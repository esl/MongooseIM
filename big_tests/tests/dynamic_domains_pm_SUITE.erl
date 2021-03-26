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
    rpc(mim(), mongoose_domain_core, insert, [Domain, HostType, Source]),
    rpc(mim2(), mongoose_domain_core, insert, [Domain, HostType, Source]),
    Config.

end_per_suite(Config) ->
    rpc(mim(), mongoose_domain_core, delete, [<<"example.com">>]),
    rpc(mim2(), mongoose_domain_core, delete, [<<"example.com">>]),
    Config.

can_authenticate(Config) ->
    %% TODO: implement later
    ok.
