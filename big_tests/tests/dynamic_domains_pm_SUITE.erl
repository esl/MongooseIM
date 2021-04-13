-module(dynamic_domains_pm_SUITE).

%% API
-compile(export_all).
-import(distributed_helper, [mim/0, mim2/0, require_rpc_nodes/1, rpc/4]).

-define(TEST_NODES, [mim() | ?CLUSTER_NODES]).
-define(CLUSTER_NODES, [mim2()]).
-define(DOMAINS, [<<"example.com">>, <<"example.org">>]).

suite() ->
    require_rpc_nodes([mim, mim2]).

all() ->
    [can_authenticate,
     pm_messages,
     disconnected_on_domain_disabling].

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

%% helper functions
insert_domains(Nodes, Domains) ->
    Source = dummy_source, %% can be anything, we don't care about it
    HostType = <<"test type">>, %% preconfigured in the toml file
    [ok = rpc(Node, mongoose_domain_core, insert, [Domain, HostType, Source]) ||
        Node <- Nodes, Domain <- Domains].

remove_domains(Nodes, Domains) ->
    [ok = rpc(Node, mongoose_domain_core, delete, [Domain]) ||
        Node <- Nodes, Domain <- Domains].

cluster_nodes([], Config) -> Config;
cluster_nodes([Node | T], Config) ->
    NewConfig = distributed_helper:add_node_to_cluster(Node, Config),
    cluster_nodes(T, NewConfig).

uncluster_nodes([], Config) -> Config;
uncluster_nodes([Node | T], Config) ->
    NewConfig = distributed_helper:remove_node_from_cluster(Node, Config),
    cluster_nodes(T, NewConfig).
