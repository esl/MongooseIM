-module(dynamic_domains_pm_SUITE).

%% API
-compile(export_all).
-import(distributed_helper, [mim/0, mim2/0, require_rpc_nodes/1, rpc/4]).

-define(TEST_NODES, [mim(), mim2()]).
-define(DOMAINS, [<<"example.com">>, <<"example.org">>]).

suite() ->
    require_rpc_nodes([mim, mim2]).

all() ->
    [can_authenticate].

init_per_suite(Config) ->
    insert_domains(?TEST_NODES, ?DOMAINS),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    remove_domains(?TEST_NODES, ?DOMAINS),
    escalus:end_per_suite(Config).

init_per_testcase(CN, Config) ->
    escalus:init_per_testcase(CN, Config).

end_per_testcase(CN, Config) ->
    escalus:end_per_testcase(CN, Config).

can_authenticate(Config) ->
    UserSpecA = escalus_users:get_userspec(Config, alice3),
    {ok, ClientA, _} = escalus_connection:start(UserSpecA),
    UserSpecB = escalus_users:get_userspec(Config, bob3),
    {ok, ClientB, _} = escalus_connection:start(UserSpecB),
    escalus_connection:stop(ClientA),
    escalus_connection:stop(ClientB).

%% helper functions
insert_domains(Nodes, Domains) ->
    Source = dummy_source, %% can be anything, we don't care about it
    HostType = <<"test type">>, %% preconfigured in the toml file
    [ok = rpc(Node, mongoose_domain_core, insert, [Domain, HostType, Source]) ||
        Node <- Nodes, Domain <- Domains].

remove_domains(Nodes, Domains) ->
    [ok = rpc(Node, mongoose_domain_core, delete, [Domain]) ||
        Node <- Nodes, Domain <- Domains].
