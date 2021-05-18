-module(dynamic_domains_pm_SUITE).

-include_lib("exml/include/exml.hrl").

%% API
-compile(export_all).
-import(distributed_helper, [mim/0, mim2/0, rpc/4,
require_rpc_nodes/1,
subhost_pattern/1]).

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
     auth_domain_removal_is_triggered_on_hook,
     test_routing].

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

test_routing(Config) ->
    StoryFn =
        fun(Alice) ->
            dynamic_modules:start(?HOST_TYPE, mod_dynamic_domains_test,
                                  [{host1, subhost_pattern("subdomain1.@HOST@")},
                                   {host2, subhost_pattern("subdomain2.@HOST@")},
                                   {namespace, <<"dummy.namespace">>}]),
            rpc(mim(), meck, unload, []),
            ok = rpc(mim(), meck, new, [mod_dynamic_domains_test,
                                        [passthrough, no_link]]),
            Subdomains1 = [<<"subdomain1.", Domain/binary>> || Domain <- ?DOMAINS],
            [escalus:send(Alice, escalus_stanza:chat_to(Subdomain, <<"OH, HAI!">>))
             || Subdomain <- Subdomains1],
            rpc(mim(), meck, wait, [2, mod_dynamic_domains_test, process_packet, 5, 200]),
            rpc(mim(), meck, reset, [mod_dynamic_domains_test]),

            QueryEl = escalus_stanza:query_el(<<"dummy.namespace">>, []),
            [begin
                 IQ = escalus_stanza:iq(Subdomain, <<"get">>, [QueryEl]),
                 escalus:send(Alice, IQ)
             end || Subdomain <- Subdomains1],
            %% check that all the IQs to any of Subdomains1 landed at process_packet/5
            %% and no stanzas received in response
            rpc(mim(), meck, wait, [2, mod_dynamic_domains_test, process_packet, 5, 200]),
            0 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_iq, 5]),
            rpc(mim(), meck, reset, [mod_dynamic_domains_test]),
            [] = escalus:wait_for_stanzas(Alice, 5, 1000),

            [begin
                 IQ = escalus_stanza:iq(Domain, <<"get">>, [QueryEl]),
                 escalus:send_iq_and_wait_for_result(Alice, IQ)
             end || Domain <- ?DOMAINS],
            2 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_iq, 5]),
            0 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_packet, 5]),
            %% check that process_iq/5 is called from one and the same worker process
            History = rpc(mim(), meck, history, [mod_dynamic_domains_test]),
            rpc(mim(), meck, reset, [mod_dynamic_domains_test]),
            Pids = [Pid||{Pid,{_,process_iq,_},_}<-History],
            [_] = (lists:usort(Pids)),

            Subdomains2 = [<<"subdomain2.", Domain/binary>> || Domain <- ?DOMAINS],
            [begin
                 IQ = escalus_stanza:iq(Subomain, <<"get">>, [QueryEl]),
                 escalus:send_iq_and_wait_for_result(Alice, IQ)
             end || Subomain <- Subdomains2],
            2 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_iq, 5]),
            0 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_packet, 5]),

            rpc(mim(), meck, unload, [mod_dynamic_domains_test]),
            dynamic_modules:stop(?HOST_TYPE, mod_dynamic_domains_test)
        end,
    escalus:story(Config, [{alice3, 1}], StoryFn).

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

dump_ets_tables(Tables) ->
    [ct:pal("!!! ~p = ~p~n", [T, rpc(mim(), ets, tab2list, [T])]) || T <- Tables].
