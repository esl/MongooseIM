-module(dynamic_domains_pm_SUITE).

%% API
-compile(export_all).
-import(distributed_helper, [mim/0, mim2/0, require_rpc_nodes/1, rpc/4]).

-define(TEST_NODES, [mim() | ?CLUSTER_NODES]).
-define(CLUSTER_NODES, [mim2()]).
-define(DOMAINS, [<<"example.com">>, <<"example.org">>]).
-define(HOST_TYPE, <<"test type">>). %% preconfigured in the toml file

suite() ->
    require_rpc_nodes([mim, mim2]).

all() ->
    [can_authenticate,
     pm_messages,
     disconnected_on_domain_disabling,
     auth_domain_removal_is_triggered_on_hook].

init_per_suite(Config0) ->
    Config = cluster_nodes(?CLUSTER_NODES, Config0),
    insert_domains(?TEST_NODES, ?DOMAINS),
    escalus:init_per_suite(Config).

end_per_suite(Config0) ->
    Config = escalus:end_per_suite(Config0),
    remove_domains(?TEST_NODES, ?DOMAINS),
    uncluster_nodes(?CLUSTER_NODES, Config).

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

pm_messages(Config) ->
    StoryFn =
        fun(Alice, Bob) ->
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
            escalus:assert(is_chat_message, [<<"OH, HAI!">>], escalus:wait_for_stanza(Bob)),
            escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Hello there!">>)),
            escalus:assert(is_chat_message, [<<"Hello there!">>], escalus:wait_for_stanza(Alice))
        end,
    escalus:story(Config, [{alice3, 1}, {bob3, 1}], StoryFn).

disconnected_on_domain_disabling(Config) ->
    StoryFn =
        fun(Alice, Bob) ->
            remove_domains(?TEST_NODES, ?DOMAINS),
            escalus_connection:wait_for_close(Alice, timer:seconds(5)),
            escalus_connection:wait_for_close(Bob, timer:seconds(5)),
            insert_domains(?TEST_NODES, ?DOMAINS)
        end,
    escalus:story(Config, [{alice3, 1}, {bob3, 1}], StoryFn).

auth_domain_removal_is_triggered_on_hook(_Config) ->
    ok = rpc(mim(), meck, new, [ejabberd_auth_dummy, [passthrough, no_link]]),
    Params = [?HOST_TYPE, <<"dummy.domain.name">>],
    rpc(mim(), mongoose_hooks, remove_domain, Params),
    1 = rpc(mim(), meck, num_calls, [ejabberd_auth_dummy, remove_domain, Params]),
    rpc(mim(), meck, unload, [ejabberd_auth_dummy]).

%% helper functions
insert_domains(Nodes, Domains) ->
    [domain_helper:insert_domain(Node, Domain) || Node <- Nodes, Domain <- Domains].

remove_domains(Nodes, Domains) ->
    [domain_helper:delete_domain(Node, Domain) || Node <- Nodes, Domain <- Domains].

cluster_nodes([], Config) -> Config;
cluster_nodes([Node | T], Config) ->
    NewConfig = distributed_helper:add_node_to_cluster(Node, Config),
    cluster_nodes(T, NewConfig).

uncluster_nodes([], Config) -> Config;
uncluster_nodes([Node | T], Config) ->
    NewConfig = distributed_helper:remove_node_from_cluster(Node, Config),
    cluster_nodes(T, NewConfig).
