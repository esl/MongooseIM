-module(dynamic_domains_SUITE).

-include_lib("exml/include/exml.hrl").

%% API
-compile([export_all, nowarn_export_all]).
-import(distributed_helper, [mim/0, mim2/0, rpc/4,
                             require_rpc_nodes/1,
                             subhost_pattern/1]).

-define(TEST_NODES, [mim() | ?CLUSTER_NODES]).
-define(CLUSTER_NODES, [mim2()]).
-define(DOMAINS, [<<"example.com">>, <<"example.org">>]).
-define(HOST_TYPE, <<"dummy auth">>). %% preconfigured in the toml file

suite() ->
    require_rpc_nodes([mim, mim2]).

all() ->
    [can_authenticate,
     pm_messages,
     disconnected_on_domain_disabling,
     auth_domain_removal_is_triggered_on_hook,
     {group, with_mod_dynamic_domains_test}].

groups() ->
    [{with_mod_dynamic_domains_test, [], [packet_handling_for_subdomain,
                                          iq_handling_for_subdomain,
                                          iq_handling_for_domain]}].

init_per_suite(Config0) ->
    Config = cluster_nodes(?CLUSTER_NODES, Config0),
    insert_domains(?TEST_NODES, ?DOMAINS),
    escalus:init_per_suite(Config).

end_per_suite(Config0) ->
    Config = escalus:end_per_suite(Config0),
    remove_domains(?TEST_NODES, ?DOMAINS),
    uncluster_nodes(?CLUSTER_NODES, Config).

init_per_group(with_mod_dynamic_domains_test, Config) ->
    MockedModules = [mod_dynamic_domains_test, mongoose_router],
    [ok = rpc(mim(), meck, new, [Module, [passthrough, no_link]])
     || Module <- MockedModules],
    dynamic_modules:start(?HOST_TYPE, mod_dynamic_domains_test,
                          #{host1 => subhost_pattern("subdomain1.@HOST@"),
                            host2 => subhost_pattern("subdomain2.@HOST@"),
                            namespace => <<"dummy.namespace">>}),
    [{reset_meck, MockedModules} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(with_mod_dynamic_domains_test, Config) ->
    dynamic_modules:stop(?HOST_TYPE, mod_dynamic_domains_test),
    rpc(mim(), meck, unload, []),
    Config;
end_per_group(_, Config) ->
    Config.

init_per_testcase(CN, Config) ->
    Modules = proplists:get_value(reset_meck, Config, []),
    [rpc(mim(), meck, reset, [M]) || M <- Modules],
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

packet_handling_for_subdomain(Config) ->
    StoryFn =
        fun(Alice) ->
            NewDomain = <<"example.test">>,
            insert_domains(?TEST_NODES, [NewDomain]),
            Domains = [NewDomain | ?DOMAINS],
            Subdomains = [<<"subdomain1.", Domain/binary>> || Domain <- Domains],
            [NewSubdomain | _] = Subdomains,
            [escalus:send(Alice, escalus_stanza:chat_to(Subdomain, <<"OH, HAI!">>))
             || Subdomain <- Subdomains],
            rpc(mim(), meck, wait, [3, mod_dynamic_domains_test, process_packet, 5, 500]),
            rpc(mim(), meck, reset, [mod_dynamic_domains_test]),

            QueryEl = escalus_stanza:query_el(<<"dummy.namespace">>, []),
            [begin
                 IQ = escalus_stanza:iq(Subdomain, <<"get">>, [QueryEl]),
                 escalus:send(Alice, IQ)
             end || Subdomain <- Subdomains],
            %% check that all the IQs to any of Subdomains1 landed at process_packet/5
            %% and no stanzas received in response
            rpc(mim(), meck, wait, [3, mod_dynamic_domains_test, process_packet, 5, 500]),
            0 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_iq, 5]),
            [] = escalus:wait_for_stanzas(Alice, 5, 500),
            rpc(mim(), meck, reset, [mod_dynamic_domains_test]),

            %% check that subdomain is not served after the parent domain removal
            remove_domains(?TEST_NODES, [NewDomain]),
            rpc(mim(), meck, wait, [mongoose_router, unregister_route, [NewSubdomain], 500]),
            IQ = escalus_stanza:iq(NewSubdomain, <<"get">>, [QueryEl]),
            escalus:send(Alice, IQ),
            Stanza = escalus:wait_for_stanza(Alice, 10000),
            escalus:assert(is_error, [<<"cancel">>, <<"remote-server-not-found">>], Stanza),
            0 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_packet, 5])
        end,
    escalus:story(Config, [{alice3, 1}], StoryFn).

iq_handling_for_domain(Config) ->
    StoryFn =
        fun(Alice) ->
            NewDomain = <<"example.test">>,
            insert_domains(?TEST_NODES, [NewDomain]),
            Domains = [NewDomain | ?DOMAINS],
            QueryEl = escalus_stanza:query_el(<<"dummy.namespace">>, []),
            [begin
                 IQ = escalus_stanza:iq(Domain, <<"get">>, [QueryEl]),
                 escalus:send_iq_and_wait_for_result(Alice, IQ)
             end || Domain <- Domains],
            3 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_iq, 5]),
            0 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_packet, 5]),
            %% check that process_iq/5 is called from one and the same worker process
            History = rpc(mim(), meck, history, [mod_dynamic_domains_test]),
            rpc(mim(), meck, reset, [mod_dynamic_domains_test]),
            Pids = [Pid || {Pid, {_, process_iq, _}, _} <- History],
            [_] = (lists:usort(Pids)),

            %% check that domain is not served removal
            remove_domains(?TEST_NODES, [NewDomain]),
            rpc(mim(), meck, wait, [mongoose_router, unregister_route, [NewDomain], 500]),
            IQ = escalus_stanza:iq(NewDomain, <<"get">>, [QueryEl]),
            escalus:send(Alice, IQ),
            Stanza = escalus:wait_for_stanza(Alice, 10000),
            escalus:assert(is_error, [<<"cancel">>, <<"remote-server-not-found">>], Stanza),
            0 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_iq, 5])
        end,
    escalus:story(Config, [{alice3, 1}], StoryFn).

iq_handling_for_subdomain(Config) ->
    StoryFn =
        fun(Alice) ->
            NewDomain = <<"example.test">>,
            insert_domains(?TEST_NODES, [NewDomain]),
            Domains = [NewDomain | ?DOMAINS],
            Subdomains = [<<"subdomain2.", Domain/binary>> || Domain <- Domains],
            [NewSubdomain | _] = Subdomains,
            QueryEl = escalus_stanza:query_el(<<"dummy.namespace">>, []),
            [begin
                 IQ = escalus_stanza:iq(Subdomain, <<"get">>, [QueryEl]),
                 escalus:send_iq_and_wait_for_result(Alice, IQ)
             end || Subdomain <- Subdomains],
            3 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_iq, 5]),
            0 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_packet, 5]),
            %% check that process_iq/5 is called from one and the same worker process
            History = rpc(mim(), meck, history, [mod_dynamic_domains_test]),
            rpc(mim(), meck, reset, [mod_dynamic_domains_test]),
            Pids = [Pid || {Pid, {_, process_iq, _}, _} <- History],
            [_] = (lists:usort(Pids)),

            %% check that subdomain is not served after the parent domain removal
            remove_domains(?TEST_NODES, [NewDomain]),
            rpc(mim(), meck, wait, [mongoose_router, unregister_route, [NewSubdomain], 500]),
            IQ = escalus_stanza:iq(NewSubdomain, <<"get">>, [QueryEl]),
            escalus:send(Alice, IQ),
            Stanza = escalus:wait_for_stanza(Alice, 10000),
            escalus:assert(is_error, [<<"cancel">>, <<"remote-server-not-found">>], Stanza),
            0 = rpc(mim(), meck, num_calls, [mod_dynamic_domains_test, process_iq, 5])
        end,
    escalus:story(Config, [{alice3, 1}], StoryFn).

%% helper functions
insert_domains(Nodes, Domains) ->
    [domain_helper:insert_domain(Node, Domain, ?HOST_TYPE) || Node <- Nodes, Domain <- Domains].

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
