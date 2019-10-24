%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mod_global_distrib_SUITE).

-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(HOSTS_REFRESH_INTERVAL, 200). %% in ms

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, mod_global_distrib},
     {group, cluster_restart},
     {group, start_checks},
     {group, invalidation},
     {group, multi_connection},
     {group, rebalancing},
     {group, advertised_endpoints},
     {group, hosts_refresher}
    ].

groups() ->
    G = [{mod_global_distrib, [],
          [
           test_pm_between_users_at_different_locations,
           test_pm_between_users_before_available_presence,
           test_component_disconnect,
           test_component_on_one_host,
           test_components_in_different_regions,
           test_hidden_component_disco_in_different_region,
           test_pm_with_disconnection_on_other_server,
           test_pm_with_graceful_reconnection_to_different_server,
           test_pm_with_ungraceful_reconnection_to_different_server,
           test_pm_with_ungraceful_reconnection_to_different_server_with_asia_refreshes_first,
           test_pm_with_ungraceful_reconnection_to_different_server_with_europe_refreshes_first,
           test_component_unregister,
           test_update_senders_host,
           test_update_senders_host_by_ejd_service,

           %% with node 2 disabled
           test_muc_conversation_on_one_host,
           test_global_disco
           %% TODO: Add test case fo global_distrib_addr option
          ]},
         {hosts_refresher, [],
          [test_host_refreshing]},
         {cluster_restart, [],
          [
           test_location_disconnect
          ]},
         {start_checks, [],
          [
           test_error_on_wrong_hosts
          ]},
         {invalidation, [],
          [
           % TODO: Add checks for other mapping refreshes
           refresh_nodes
          ]},
         {multi_connection, [],
          [
           test_in_order_messages_on_multiple_connections,
           test_in_order_messages_on_multiple_connections_with_bounce,
           test_messages_bounced_in_order,

           %% with node 2 disabled
           test_muc_conversation_history
          ]},
         {rebalancing, [],
          [
           enable_new_endpoint_on_refresh,
           disable_endpoint_on_refresh,
           wait_for_connection,
           closed_connection_is_removed_from_disabled
          ]},
         {advertised_endpoints, [],
          [
           test_advertised_endpoints_override_endpoints,
           test_pm_between_users_at_different_locations
          ]}
        ],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    [{require, europe_node1, {hosts, mim, node}},
     {require, europe_node2, {hosts, mim2, node}},
     {require, asia_node, {hosts, reg, node}},
     {require, c2s_port, {hosts, mim, c2s_port}} |
     escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    case {rpc(europe_node1, mongoose_wpool, get_worker, [redis, global, global_distrib]),
          rpc(asia_node, mongoose_wpool, get_worker, [redis, global, global_distrib])} of
        {{ok, _}, {ok, _}} ->
            ok = rpc(europe_node2, mongoose_cluster, join, [ct:get_config(europe_node1)]),

            enable_logging(),
            % We have to pass [no_opts] because [] is treated as string and converted
            % automatically to <<>>
            escalus:init_per_suite([{add_advertised_endpoints, []},
                                    {extra_config, []}, {redis_extra_config, [no_opts]} | Config]);
        Result ->
            ct:pal("Redis check result: ~p", [Result]),
            {skip, "GD Redis default pool not available"}
    end.

end_per_suite(Config) ->
    disable_logging(),
    escalus_fresh:clean(),
    rpc(europe_node2, mongoose_cluster, leave, []),
    escalus:end_per_suite(Config).

init_per_group(start_checks, Config) ->
    Config;
init_per_group(multi_connection, Config) ->
    ExtraConfig = [{resend_after_ms, 20000},
                   %% Disable unused feature to avoid interferance
                   {disabled_gc_interval, 10000},
                   {connections_per_endpoint, 100}],
    init_per_group_generic([{extra_config, ExtraConfig} | Config]);
init_per_group(invalidation, Config) ->
    Config1 = init_per_group(invalidation_generic, Config),
    NodeBin = <<"fake_node@localhost">>,
    [{node_to_expire, NodeBin} | Config1];
init_per_group(rebalancing, Config) ->
    %% We need to prevent automatic refreshes, because they may interfere with tests
    %% and we need early disabled garbage collection to check its validity
    ExtraConfig = [{endpoint_refresh_interval, 3600},
                   {endpoint_refresh_interval_when_empty, 3600},
                   {disabled_gc_interval, 1}],
    RedisExtraConfig = [{refresh_after, 3600}],
    init_per_group_generic([{extra_config, ExtraConfig},
                            {redis_extra_config, RedisExtraConfig} | Config]);
init_per_group(advertised_endpoints, Config) ->
    lists:foreach(fun({NodeName, _, _}) ->
                          Node = ct:get_config(NodeName),
                          mongoose_helper:inject_module(Node, ?MODULE, reload)
                  end, get_hosts()),
    mock_inet_on_each_node(),
    init_per_group_generic(
               [{add_advertised_endpoints,
                 [{asia_node, advertised_endpoints()}]} | Config]);
init_per_group(mod_global_distrib, Config) ->
    %% Disable mod_global_distrib_mapping_redis refresher
    RedisExtraConfig = [{refresh_after, 3600}],
    init_per_group_generic([{redis_extra_config, RedisExtraConfig} | Config]);
init_per_group(_, Config) ->
    init_per_group_generic(Config).

init_per_group_generic(Config0) ->
    Config2 =
        lists:foldl(
          fun({NodeName, LocalHost, ReceiverPort}, Config1) ->
                  Opts0 = (?config(extra_config, Config1) ++
                           [{local_host, LocalHost},
                            {hosts_refresh_interval, ?HOSTS_REFRESH_INTERVAL},
                            {global_host, "localhost"},
                            {endpoints, [listen_endpoint(ReceiverPort)]},
                            {tls_opts, [
                                        {certfile, "priv/ssl/fake_server.pem"},
                                        {cafile, "priv/ssl/ca/cacert.pem"}
                                       ]},
                            {redis, ?config(redis_extra_config, Config1)},
                            {resend_after_ms, 500}]),
                  Opts = maybe_add_advertised_endpoints(NodeName, Opts0, Config1),

                  %% To reduce load when sending many messages
                  VirtHosts = virtual_hosts(),
                  ModulesToStop = [mod_offline, mod_privacy, mod_roster, mod_last],

                  OldMods = save_modules(NodeName, VirtHosts),

                  rpc(NodeName, gen_mod_deps, start_modules,
                      [<<"localhost">>, [{mod_global_distrib, Opts}]]),

                  [rpc(NodeName, gen_mod, stop_module, [VirtHost, Mod])
                   || Mod <- ModulesToStop, VirtHost <- VirtHosts],

                  ResumeTimeout = rpc(NodeName, mod_stream_management, get_resume_timeout, [1]),
                  true = rpc(NodeName, mod_stream_management, set_resume_timeout, [1]),

                  OldMods ++
                  [
                   {{resume_timeout, NodeName}, ResumeTimeout} |
                   Config1
                  ]
          end,
          Config0,
          get_hosts()),

    wait_for_listeners_to_appear(),

    {SomeNode, _, _} = hd(get_hosts()),
    NodesKey = rpc(SomeNode, mod_global_distrib_mapping_redis, nodes_key, []),
    [{nodes_key, NodesKey}, {escalus_user_db, xmpp} | Config2].

end_per_group(advertised_endpoints, Config) ->
    Pids = ?config(meck_handlers, Config),
    unmock_inet(Pids),
    escalus_fresh:clean(),
    end_per_group_generic(Config);
end_per_group(start_checks, Config) ->
    escalus_fresh:clean(),
    Config;
end_per_group(invalidation, Config) ->
    redis_query(europe_node1, [<<"HDEL">>, ?config(nodes_key, Config),
                            ?config(node_to_expire, Config)]),
    end_per_group_generic(Config);
end_per_group(_, Config) ->
    end_per_group_generic(Config).

end_per_group_generic(Config) ->
    lists:foreach(
      fun({NodeName, _, _}) ->
              VirtHosts = virtual_hosts(),
              [restore_modules(NodeName, VirtHost, Config) || VirtHost <- VirtHosts],

              rpc(NodeName, mod_stream_management, set_resume_timeout,
                  [?config({resume_timeout, NodeName}, Config)])
      end,
      get_hosts()).

init_per_testcase(CaseName, Config)
  when CaseName == test_muc_conversation_on_one_host; CaseName == test_global_disco;
       CaseName == test_muc_conversation_history ->
    %% There is no helper to load MUC on node2
    %% For now it's easier to hide node2
    %% TODO: Do it right at some point!
    hide_node(europe_node2, Config),
    %% There would be no new connections to europe_node2, but there can be some old ones.
    %% We need to disconnect previous connections.
    {_, EuropeHost, _} = lists:keyfind(europe_node1, 1, get_hosts()),
    trigger_rebalance(asia_node, list_to_binary(EuropeHost)),
    %% Load muc on mim node
    muc_helper:load_muc(<<"muc.localhost">>),
    RegNode = ct:get_config({hosts, reg, node}),
    %% Wait for muc.localhost to become visible from reg node
    wait_for_domain(RegNode, <<"muc.localhost">>),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CN, Config) when CN == test_pm_with_graceful_reconnection_to_different_server;
                                   CN == test_pm_with_ungraceful_reconnection_to_different_server;
                                   CN == test_pm_with_ungraceful_reconnection_to_different_server_with_asia_refreshes_first;
                                   CN == test_pm_with_ungraceful_reconnection_to_different_server_with_europe_refreshes_first ->
    escalus:init_per_testcase(CN, init_user_eve(Config));
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

init_user_eve(Config) ->
    %% Register Eve in reg cluster
    EveSpec = escalus_fresh:create_fresh_user(Config, eve),
    MimPort = ct:get_config({hosts, mim, c2s_port}),
    EveSpec2 = lists:keystore(port, 1, EveSpec, {port, MimPort}),
    %% Register Eve in mim cluster
    escalus:create_users(Config, [{eve, EveSpec2}]),
    [{evespec_reg, EveSpec}, {evespec_mim, EveSpec2} | Config].

end_per_testcase(CN, Config) when CN == test_pm_with_graceful_reconnection_to_different_server;
                                  CN == test_pm_with_ungraceful_reconnection_to_different_server;
                                  CN == test_pm_with_ungraceful_reconnection_to_different_server_with_asia_refreshes_first;
                                  CN == test_pm_with_ungraceful_reconnection_to_different_server_with_europe_refreshes_first ->
    MimEveSpec = ?config(evespec_mim, Config),
    %% Clean Eve from reg cluster
    escalus_fresh:clean(),
    %% Clean Eve from mim cluster
    %% For shared databases (i.e. mysql, pgsql...),
    %% removing from one cluster would remove from all clusters.
    %% For mnesia auth backend we need to call removal from each cluster.
    %% That's why there is a catch here.
    catch escalus_users:delete_users(Config, [{mim_eve, MimEveSpec}]),
    generic_end_per_testcase(CN, Config);
end_per_testcase(CaseName, Config)
  when CaseName == test_muc_conversation_on_one_host; CaseName == test_global_disco;
       CaseName == test_muc_conversation_history ->
    refresh_mappings(europe_node2, "by_end_per_testcase,testcase=" ++ atom_to_list(CaseName)),
    muc_helper:unload_muc(),
    generic_end_per_testcase(CaseName, Config);
end_per_testcase(test_update_senders_host_by_ejd_service = CN, Config) ->
    refresh_mappings(europe_node1, "by_end_per_testcase,testcase=" ++ atom_to_list(CN)),
    generic_end_per_testcase(CN, Config);
end_per_testcase(CN, Config) when CN == enable_new_endpoint_on_refresh;
                                  CN == disable_endpoint_on_refresh;
                                  CN == wait_for_connection;
                                  CN == closed_connection_is_removed_from_disabled ->
    restart_receiver(asia_node),
    refresh_mappings(asia_node, "by_end_per_testcase,testcase=" ++ atom_to_list(CN)),
    generic_end_per_testcase(CN, Config);
end_per_testcase(CaseName, Config) ->
    generic_end_per_testcase(CaseName, Config).

generic_end_per_testcase(CaseName, Config) ->
    lists:foreach(
      fun({NodeName, _, _}) ->
              %% TODO: Enable refresher only for specific test cases,
              %% as some of them are based on assumption that node(s)
              %% must open new connections during tests.
              pause_refresher(NodeName, CaseName),
              Node = ct:get_config(NodeName),
              SupRef = {mod_global_distrib_outgoing_conns_sup, Node},
              try
                  OutgoingConns = supervisor:which_children(SupRef),
                  lists:foreach(fun ({mod_global_distrib_hosts_refresher, _, _, _}) ->
                                        skip;
                                    ({Id, _, _, _}) ->
                                        supervisor:terminate_child(SupRef, Id)
                                end, OutgoingConns),
                  [{mod_global_distrib_hosts_refresher, _, worker, _Modules}] =
                    supervisor:which_children(SupRef)
              catch
                  _:{noproc, _} ->
                      ct:pal("Sender supervisor not found in ~p", [NodeName])
              end,
              unpause_refresher(NodeName, CaseName)
      end,
      get_hosts()),
    escalus:end_per_testcase(CaseName, Config).

virtual_hosts() ->
    [ct:get_config({hosts, mim, domain}), ct:get_config({hosts, mim, secondary_domain})].

%% Refresher is not started at all or stopped for some test cases
-spec pause_refresher(NodeName :: atom(), CaseName :: atom()) -> ok.
pause_refresher(_, test_error_on_wrong_hosts) ->
    ok;
pause_refresher(asia_node, test_location_disconnect) ->
    ok;
pause_refresher(NodeName, _) ->
    ok = rpc(NodeName, mod_global_distrib_hosts_refresher, pause, []).

-spec unpause_refresher(NodeName :: atom(), CaseName :: atom()) -> ok.
unpause_refresher(_, test_error_on_wrong_hosts) ->
    ok;
unpause_refresher(asia_node, test_location_disconnect) ->
    ok;
unpause_refresher(NodeName, _) ->
    ok = rpc(NodeName, mod_global_distrib_hosts_refresher, unpause, []).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------

%% Requires module mod_global_distrib to be started with argument advertised_endpoints
%% for each host in get_hosts().
%% Reads Redis to confirm that endpoints (in Redis) are overwritten
%% with `advertised_endpoints` option value
test_advertised_endpoints_override_endpoints(_Config) ->
    Endps = execute_on_each_node(mod_global_distrib_mapping_redis,
                                 get_endpoints,
                                 [<<"reg1">>]),
    true = lists:all(fun({ok, E}) ->
                             lists:sort(iptuples_to_string(E)) =:=
                                 lists:sort(advertised_endpoints()) end, Endps).

%% @doc Verifies that hosts refresher will restart the outgoing connection pool if
%% it goes down for some reason (crash or domain unavailability).
%% Also actually verifies that refresher properly reads host list
%% from backend and starts appropriate pool.
test_host_refreshing(_Config) ->
    mongoose_helper:wait_until(fun() -> trees_for_connections_present() end, true,
                               #{name => trees_for_connections_present}),
    ConnectionSups = out_connection_sups(asia_node),
    {europe_node1, EuropeHost, _} = lists:keyfind(europe_node1, 1, get_hosts()),
    EuropeSup = rpc(asia_node, mod_global_distrib_utils, server_to_sup_name, [list_to_binary(EuropeHost)]),
    {_, EuropePid, supervisor, _} = lists:keyfind(EuropeSup, 1, ConnectionSups),
    erlang:exit(EuropePid, kill), % it's ok to kill temporary process
    mongoose_helper:wait_until(fun() -> tree_for_sup_present(asia_node, EuropeSup) end, true,
                               #{name => tree_for_sup_present}).

%% When run in mod_global_distrib group - tests simple case of connection
%% between two users connected to different clusters.
%% When run in advertised_endpoints group it tests whether it is possible
%% to connect to a node that is advertising itself with a domain name.
test_pm_between_users_at_different_locations(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {eve, 1}], fun test_two_way_pm/2).

test_pm_between_users_before_available_presence(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {eve, 1}]),
    {ok, Alice} = escalus_client:start(Config1, alice, <<"res1">>),
    {ok, Eve} = escalus_client:start(Config1, eve, <<"res1">>),

    test_two_way_pm(Alice, Eve),

    escalus_client:stop(Config1, Alice),
    escalus_client:stop(Config1, Eve).

test_two_way_pm(Alice, Eve) ->
    %% Ensure that users are properly registered
    %% Otherwise you can get "Unable to route global message... user not found in the routing table"
    %% error, because "escalus_client:start" can return before SM registration is completed.
    wait_for_registration(Alice, ct:get_config({hosts, mim, node})),
    wait_for_registration(Eve, ct:get_config({hosts, reg, node})),

    escalus_client:send(Alice, escalus_stanza:chat_to(Eve, <<"Hi to Eve from Europe1!">>)),
    escalus_client:send(Eve, escalus_stanza:chat_to(Alice, <<"Hi to Alice from Asia!">>)),

    FromAlice = escalus_client:wait_for_stanza(Eve, timer:seconds(15)),
    FromEve = escalus_client:wait_for_stanza(Alice, timer:seconds(15)),

    AliceJid = escalus_client:full_jid(Alice),
    EveJid = escalus_client:full_jid(Eve),

    escalus:assert(is_chat_message_from_to, [AliceJid, EveJid, <<"Hi to Eve from Europe1!">>],
                   FromAlice),
    escalus:assert(is_chat_message_from_to, [EveJid, AliceJid, <<"Hi to Alice from Asia!">>],
                   FromEve).

test_muc_conversation_on_one_host(Config0) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config0, alice),
    Config = muc_helper:given_fresh_room(Config0, AliceSpec, []),
    escalus:fresh_story(
      Config, [{eve, 1}],
      fun(Eve) ->
              Alice = connect_from_spec(AliceSpec, Config),

              RoomJid = ?config(room, Config),
              AliceUsername = escalus_utils:get_username(Alice),
              EveUsername = escalus_utils:get_username(Eve),
              RoomAddr = muc_helper:room_address(RoomJid),

              escalus:send(Alice, muc_helper:stanza_muc_enter_room(RoomJid, AliceUsername)),
              escalus:wait_for_stanza(Alice),

              escalus:send(Eve, muc_helper:stanza_muc_enter_room(RoomJid, EveUsername)),
              [_, _, _] = escalus:wait_for_stanzas(Eve, 3),

              Msg= <<"Hi, Eve!">>,
              escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Msg)),
              escalus:assert(is_groupchat_message, [Msg], escalus:wait_for_stanza(Alice)),
              escalus:assert(is_groupchat_message, [Msg], escalus:wait_for_stanza(Eve)),

              Msg2= <<"Hi, Alice!">>,
              escalus:send(Eve, escalus_stanza:groupchat_to(RoomAddr, Msg2)),
              escalus:assert(is_groupchat_message, [Msg2], escalus:wait_for_stanza(Eve)),
              escalus:assert(is_groupchat_message, [Msg2], escalus:wait_for_stanza(Alice))
      end),
    muc_helper:destroy_room(Config).

test_muc_conversation_history(Config0) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config0, alice),
    Config = muc_helper:given_fresh_room(Config0, AliceSpec, []),
    escalus:fresh_story(
      Config, [{eve, 1}],
      fun(Eve) ->
              Alice = connect_from_spec(AliceSpec, Config),

              RoomJid = ?config(room, Config),
              AliceUsername = escalus_utils:get_username(Alice),
              RoomAddr = muc_helper:room_address(RoomJid),

              escalus:send(Alice, muc_helper:stanza_muc_enter_room(RoomJid, AliceUsername)),
              %% We don't care about presences from Alice, escalus would filter them out
              wait_for_subject(Alice),

              send_n_muc_messages(Alice, RoomAddr, 3),

              %% Ensure that the messages are received by the room
              %% before trying to login Eve.
              %% Otherwise, Eve would receive some messages from history and
              %% some as regular groupchat messages.
              receive_n_muc_messages(Alice, 3),

              EveUsername = escalus_utils:get_username(Eve),
              escalus:send(Eve, muc_helper:stanza_muc_enter_room(RoomJid, EveUsername)),

              wait_for_muc_presence(Eve, RoomJid, AliceUsername),
              wait_for_muc_presence(Eve, RoomJid, EveUsername),

              %% XEP-0045: After sending the presence broadcast (and only after doing so),
              %% the service MAY then send discussion history, the room subject,
              %% live messages, presence updates, and other in-room traffic.
              receive_n_muc_messages(Eve, 3),
              wait_for_subject(Eve)
      end),
    muc_helper:destroy_room(Config).

wait_for_muc_presence(User, RoomJid, FromNickname) ->
    Presence = escalus:wait_for_stanza(User),
    escalus:assert(is_presence, Presence),
    escalus:assert(is_stanza_from, [muc_helper:room_address(RoomJid, FromNickname)], Presence),
    ok.

wait_for_subject(User) ->
    Subject = escalus:wait_for_stanza(User),
    escalus:assert(is_groupchat_message, Subject),
    ?assertNotEqual(undefined, exml_query:subelement(Subject, <<"subject">>)),
    ok.

send_n_muc_messages(User, RoomAddr, N) ->
    lists:foreach(fun(I) ->
                          Msg = <<"test-", (integer_to_binary(I))/binary>>,
                          escalus:send(User, escalus_stanza:groupchat_to(RoomAddr, Msg))
                  end, lists:seq(1, N)).

receive_n_muc_messages(User, N) ->
    lists:foreach(fun(J) ->
                          Msg = <<"test-", (integer_to_binary(J))/binary>>,
                          Stanza = escalus:wait_for_stanza(User),
                          escalus:assert(is_groupchat_message, [Msg], Stanza)
                            end, lists:seq(1, N)).

test_component_on_one_host(Config) ->
    ComponentConfig = [{server, <<"localhost">>}, {host, <<"localhost">>}, {password, <<"secret">>},
                       {port, service_port()}, {component, <<"test_service">>}],

    {Comp, Addr, _Name} = component_helper:connect_component(ComponentConfig),

    Story = fun(User) ->
                    Msg1 = escalus_stanza:chat_to(Addr, <<"Hi2!">>),
                    escalus:send(User, Msg1),
                    %% Then component receives it
                    Reply1 = escalus:wait_for_stanza(Comp),
                    escalus:assert(is_chat_message, [<<"Hi2!">>], Reply1),

                    %% When components sends a reply
                    Msg2 = escalus_stanza:chat_to(User, <<"Oh hi!">>),
                    escalus:send(Comp, escalus_stanza:from(Msg2, Addr)),

                    %% Then Alice receives it
                    Reply2 = escalus:wait_for_stanza(User),
                    escalus:assert(is_chat_message, [<<"Oh hi!">>], Reply2),
                    escalus:assert(is_stanza_from, [Addr], Reply2)
            end,

    [escalus:fresh_story(Config, [{User, 1}], Story) || User <- [alice, eve]].

%% Ensures that 2 components in distinct data centers can communicate.
test_components_in_different_regions(_Config) ->
    ComponentCommonConfig = [{host, <<"localhost">>}, {password, <<"secret">>},
                             {server, <<"localhost">>}, {component, <<"test_service">>}],
    Comp1Port = ct:get_config({hosts, mim, service_port}),
    Comp2Port = ct:get_config({hosts, reg, service_port}),
    Component1Config = [{port, Comp1Port}, {component, <<"service1">>} | ComponentCommonConfig],
    Component2Config = [{port, Comp2Port}, {component, <<"service2">>} | ComponentCommonConfig],

    {Comp1, Addr1, _Name1} = component_helper:connect_component(Component1Config),
    {Comp2, Addr2, _Name2} = component_helper:connect_component(Component2Config),

    Msg1 = escalus_stanza:from(escalus_stanza:chat_to(Addr2, <<"Hi from 1!">>), Addr1),
    escalus:send(Comp1, Msg1),
    GotMsg1 = escalus:wait_for_stanza(Comp2),
    escalus:assert(is_chat_message, [<<"Hi from 1!">>], GotMsg1),

    Msg2 = escalus_stanza:from(escalus_stanza:chat_to(Addr1, <<"Hi from 2!">>), Addr2),
    escalus:send(Comp2, Msg2),
    GotMsg2 = escalus:wait_for_stanza(Comp1),
    escalus:assert(is_chat_message, [<<"Hi from 2!">>], GotMsg2).

%% Ordinary user is not able to discover the hidden component from GD
test_hidden_component_disco_in_different_region(Config) ->
    %% Hidden component from component_SUITE connects to mim1/europe_node1
    HiddenComponentConfig = component_helper:spec(hidden_component, Config),
    {_HiddenComp, HiddenAddr, _} = component_helper:connect_component(HiddenComponentConfig),

    escalus:fresh_story(
      Config, [{eve, 1}],
      fun(Eve) ->
              EveServer = escalus_client:server(Eve),
              escalus:send(Eve, escalus_stanza:service_discovery(EveServer)),
              DiscoReply = escalus:wait_for_stanza(Eve),
              escalus:assert(is_iq_result, DiscoReply),
              escalus:assert(fun(Stanza) ->
                                     not escalus_pred:has_service(HiddenAddr, Stanza)
                             end, DiscoReply)
      end).

test_component_disconnect(Config) ->
    ComponentConfig = [{server, <<"localhost">>}, {host, <<"localhost">>}, {password, <<"secret">>},
                       {port, service_port()}, {component, <<"test_service">>}],

    {Comp, Addr, _Name} = component_helper:connect_component(ComponentConfig),
    component_helper:disconnect_component(Comp, Addr),

    Story = fun(User) ->
                    escalus:send(User, escalus_stanza:chat_to(Addr, <<"Hi!">>)),
                    Error = escalus:wait_for_stanza(User, 5000),
                    escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Error)
            end,

    [escalus:fresh_story(Config, [{User, 1}], Story) || User <- [alice, eve]].

test_location_disconnect(Config) ->
    try
        escalus:fresh_story(
          Config, [{alice, 1}, {eve, 1}],
          fun(Alice, Eve) ->
                  escalus_client:send(Alice, escalus_stanza:chat_to(Eve, <<"Hi from Europe1!">>)),

                  escalus_client:wait_for_stanza(Eve),

                  ok = rpc(asia_node, application, stop, [mongooseim]),
                  %% TODO: Stopping mongooseim alone should probably stop connections too
                  ok = rpc(asia_node, application, stop, [ranch]),

                  escalus_client:send(Alice, escalus_stanza:chat_to(Eve, <<"Hi again!">>)),
                  Error = escalus:wait_for_stanza(Alice),
                  escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Error)
          end)
    after
        rpc(asia_node, application, start, [ranch]),
        rpc(asia_node, application, start, [mongooseim])
    end.

test_pm_with_disconnection_on_other_server(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {eve, 1}],
      fun(Alice, Eve) ->
              escalus_connection:stop(Eve),
              escalus_client:send(Alice, escalus_stanza:chat_to(Eve, <<"Hi from Europe1!">>)),
              FromAliceBounce = escalus_client:wait_for_stanza(Alice, 15000),
              escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], FromAliceBounce)
      end).

test_pm_with_graceful_reconnection_to_different_server(Config) ->
    EveSpec = ?config(evespec_reg, Config),
    EveSpec2 = ?config(evespec_mim, Config),
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              Eve = connect_from_spec(EveSpec, Config),

              escalus_client:send(Eve, escalus_stanza:chat_to(Alice, <<"Hi from Asia!">>)),

              %% Stop connection and wait for process to die
              EveNode = ct:get_config({hosts, reg, node}),
              mongoose_helper:logout_user(Config, Eve, EveNode),

              FromEve = escalus_client:wait_for_stanza(Alice),

              %% Pause Alice until Eve is reconnected
              AliceNode = ct:get_config({hosts, mim, node}),
              C2sPid = mongoose_helper:get_session_pid(Alice, AliceNode),
              ok = rpc(asia_node, sys, suspend, [C2sPid]),

              escalus_client:send(Alice, chat_with_seqnum(Eve, <<"Hi from Europe1!">>)),

              NewEve = connect_from_spec(EveSpec2, Config),
              EveNode2 = ct:get_config({hosts, mim, node}),
              wait_for_registration(NewEve, EveNode2),

              ok = rpc(asia_node, sys, resume, [C2sPid]),


              escalus_client:send(Alice, chat_with_seqnum(Eve, <<"Hi again from Europe1!">>)),
              escalus_client:send(NewEve, escalus_stanza:chat_to(Alice, <<"Hi again from Asia!">>)),

              FirstFromAlice = escalus_client:wait_for_stanza(NewEve),
              AgainFromEve = escalus_client:wait_for_stanza(Alice),
              SecondFromAlice = escalus_client:wait_for_stanza(NewEve),

              [FromAlice, AgainFromAlice] = order_by_seqnum([FirstFromAlice, SecondFromAlice]),

              escalus:assert(is_chat_message, [<<"Hi from Europe1!">>], FromAlice),
              escalus:assert(is_chat_message, [<<"Hi from Asia!">>], FromEve),
              escalus:assert(is_chat_message, [<<"Hi again from Europe1!">>], AgainFromAlice),
              escalus:assert(is_chat_message, [<<"Hi again from Asia!">>], AgainFromEve)
          end).

%% Refresh logic can cause two possible behaviours.
%% We test both behaviours here (plus no refresh case)
%% See PR #2392
test_pm_with_ungraceful_reconnection_to_different_server(Config) ->
    %% No refresh
    BeforeResume = fun() -> ok end,
    AfterCheck = fun(Alice, NewEve) ->
            user_receives(NewEve, [<<"Hi from Europe1!">>, <<"Hi again from Europe1!">>]),
            user_receives(Alice, [<<"Hi from Europe!">>])
         end,
    do_test_pm_with_ungraceful_reconnection_to_different_server(Config, BeforeResume, AfterCheck).

test_pm_with_ungraceful_reconnection_to_different_server_with_asia_refreshes_first(Config) ->
    %% Same as no refresh
    RefreshReason = "by_test_pm_with_ungraceful_reconnection_to_different_server_with_asia_refreshes_first",
    % Order of nodes is important here in refresh_hosts!
    BeforeResume = fun() -> refresh_hosts([asia_node, europe_node1], RefreshReason) end,
    AfterCheck = fun(Alice, NewEve) ->
            user_receives(NewEve, [<<"Hi from Europe1!">>, <<"Hi again from Europe1!">>]),
            user_receives(Alice, [<<"Hi from Europe!">>])
         end,
    do_test_pm_with_ungraceful_reconnection_to_different_server(Config, BeforeResume, AfterCheck).

test_pm_with_ungraceful_reconnection_to_different_server_with_europe_refreshes_first(Config) ->
    %% Asia node overrides Europe value with the older ones,
    %% so we loose some messages during rerouting :(
    RefreshReason = "by_test_pm_with_ungraceful_reconnection_to_different_server_with_europe_refreshes_first",
    BeforeResume = fun() -> refresh_hosts([europe_node1, asia_node], RefreshReason) end,
    AfterCheck = fun(Alice, NewEve) ->
            user_receives(NewEve, [<<"Hi again from Europe1!">>]),
            user_receives(Alice, [<<"Hi from Europe!">>])
         end,
    do_test_pm_with_ungraceful_reconnection_to_different_server(Config, BeforeResume, AfterCheck).

%% Reconnect Eve from asia (reg cluster) to europe (mim)
do_test_pm_with_ungraceful_reconnection_to_different_server(Config0, BeforeResume, AfterCheck) ->
    Config = escalus_users:update_userspec(Config0, eve, stream_management, true),
    EveSpec = ?config(evespec_reg, Config),
    EveSpec2 = ?config(evespec_mim, Config),
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              {ok, Eve, _} = escalus_connection:start(EveSpec, connect_steps_with_sm()),
              escalus_story:send_initial_presence(Eve),
              escalus_client:wait_for_stanza(Eve),

              %% Stop connection and wait for process to die
              EveNode = ct:get_config({hosts, reg, node}),
              C2sPid = mongoose_helper:get_session_pid(Eve, EveNode),
              ok = rpc(asia_node, sys, suspend, [C2sPid]),

              escalus_client:send(Alice, chat_with_seqnum(bare_client(Eve), <<"Hi from Europe1!">>)),

              %% Wait for route message to be queued in c2s message queue
              mongoose_helper:wait_for_route_message_count(C2sPid, 1),

              %% Time to do bad nasty things with our socket, so once our process wakes up,
              %% it SHOULD detect a dead socket
              escalus_connection:kill(Eve),

              %% Connect another one, we hope the message would be rerouted
              NewEve = connect_from_spec(EveSpec2, Config),
              EveNode2 = ct:get_config({hosts, mim, node}),
              wait_for_registration(NewEve, EveNode2),

              BeforeResume(),

              %% Trigger rerouting
              ok = rpc(asia_node, sys, resume, [C2sPid]),
              C2sPid ! resume_timeout,

              %% Let C2sPid to process the message and reroute (and die finally, poor little thing)
              mongoose_helper:wait_for_pid_to_die(C2sPid),

              escalus_client:send(Alice, chat_with_seqnum(bare_client(Eve), <<"Hi again from Europe1!">>)),
              escalus_client:send(NewEve, escalus_stanza:chat_to(Alice, <<"Hi from Europe!">>)),

              AfterCheck(Alice, NewEve)
          end).

test_global_disco(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {eve, 1}],
      fun(Alice, Eve) ->
              AliceServer = escalus_client:server(Alice),
              escalus:send(Alice, escalus_stanza:service_discovery(AliceServer)),
              _AliceStanza = escalus:wait_for_stanza(Alice),
              %% TODO: test for duplicate components
              %%escalus:assert(fun has_exactly_one_service/2, [muc_helper:muc_host()], AliceStanza),

              EveServer = escalus_client:server(Eve),
              escalus:send(Eve, escalus_stanza:service_discovery(EveServer)),
              EveStanza = escalus:wait_for_stanza(Eve),
              escalus:assert(has_service, [muc_helper:muc_host()], EveStanza)
      end).

test_component_unregister(_Config) ->
    ComponentConfig = [{server, <<"localhost">>}, {host, <<"localhost">>}, {password, <<"secret">>},
                       {port, service_port()}, {component, <<"test_service">>}],

    {Comp, Addr, _Name} = component_helper:connect_component(ComponentConfig),
    ?assertMatch({ok, _}, rpc(europe_node1, mod_global_distrib_mapping, for_domain,
                              [<<"test_service.localhost">>])),

    component_helper:disconnect_component(Comp, Addr),

    ?assertEqual(error, rpc(europe_node1, mod_global_distrib_mapping, for_domain,
                            [<<"test_service.localhost">>])).

test_error_on_wrong_hosts(_Config) ->
    Opts = [{cookie, "cookie"}, {local_host, "no_such_host"}, {global_host, "localhost"}],
    ?assertException(error, {badrpc, {'EXIT', {"no_such_host is not a member of the host list", _}}},
                     rpc(europe_node1, gen_mod, start_module,
                         [<<"localhost">>, mod_global_distrib, Opts])).

refresh_nodes(Config) ->
    NodesKey = ?config(nodes_key, Config),
    NodeBin = ?config(node_to_expire, Config),
    redis_query(europe_node1, [<<"HSET">>, NodesKey, NodeBin, <<"0">>]),
    refresh_mappings(europe_node1, "by_refresh_nodes"),
    {ok, undefined} = redis_query(europe_node1, [<<"HGET">>, NodesKey, NodeBin]).

test_in_order_messages_on_multiple_connections(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {eve, 1}],
      fun(Alice, Eve) ->
              Seq = lists:seq(1, 100),
              lists:foreach(
                fun(I) ->
                        Stanza = escalus_stanza:chat_to(Eve, integer_to_binary(I)),
                        escalus_client:send(Alice, Stanza)
                end,
                Seq),
              lists:foreach(
                fun(I) ->
                        Stanza = escalus_client:wait_for_stanza(Eve, 5000),
                        escalus:assert(is_chat_message, [integer_to_binary(I)], Stanza)
                end,
                Seq)
      end).

test_in_order_messages_on_multiple_connections_with_bounce(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {eve, 1}],
      fun(Alice, Eve) ->
              %% Send 99 messages, some while server knows the mapping and some when it doesn't
              send_steps(Alice, Eve, 99, <<"reg1">>),
              %% Make sure that the last message is sent when the mapping is known
              set_mapping(europe_node1, Eve, <<"reg1">>),
              escalus_client:send(Alice, escalus_stanza:chat_to(Eve, <<"100">>)),

              %% Check that all stanzas were received in order
              lists:foreach(
                fun(I) ->
                        Stanza = escalus_client:wait_for_stanza(Eve, 5000),
                        escalus:assert(is_chat_message, [integer_to_binary(I)], Stanza)
                end,
                lists:seq(1, 100))
      end).

test_messages_bounced_in_order(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {eve, 1}],
      fun(Alice, Eve) ->
              %% Make sure all messages land in bounce storage
              delete_mapping(europe_node1, Eve),

              Seq = lists:seq(1, 100),
              lists:foreach(
                fun(I) ->
                        Stanza = escalus_stanza:chat_to(Eve, integer_to_binary(I)),
                        escalus_client:send(Alice, Stanza)
                end,
                Seq),

              %% Restore the mapping so that bounce eventually succeeds
              ?assertEqual(undefined, get_mapping(europe_node1, Eve)),
              set_mapping(europe_node1, Eve, <<"reg1">>),

              lists:foreach(
                fun(I) ->
                        Stanza = escalus_client:wait_for_stanza(Eve, 5000),
                        escalus:assert(is_chat_message, [integer_to_binary(I)], Stanza)
                end,
                Seq)
      end).

test_update_senders_host(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {eve, 1}],
      fun(Alice, Eve) ->
              AliceJid = rpc(asia_node, jid, from_binary, [escalus_client:full_jid(Alice)]),
              {ok, <<"localhost.bis">>}
              = rpc(asia_node, mod_global_distrib_mapping, for_jid, [AliceJid]),
              ok = rpc(europe_node1, mod_global_distrib_mapping, delete_for_jid, [AliceJid]),
              wait_for_node(asia_node, AliceJid),

              %% TODO: Should prevent Redis refresher from executing for a moment,
              %%       as it may collide with this test.

              escalus:send(Alice, escalus_stanza:chat_to(Eve, <<"test_update_senders_host">>)),
              escalus:wait_for_stanza(Eve),

              {ok, <<"localhost.bis">>}
              = rpc(asia_node, mod_global_distrib_mapping, for_jid, [AliceJid])
      end).
wait_for_node(Node,Jid) ->
    mongoose_helper:wait_until(fun() -> rpc(Node, mod_global_distrib_mapping, for_jid, [Jid]) end,
                               error,
                               #{time_left => timer:seconds(10),
                                 sleep_time => timer:seconds(1),
                                 name => rpc}).

test_update_senders_host_by_ejd_service(Config) ->
    refresh_hosts([europe_node1, europe_node2, asia_node], "by_test_update_senders_host_by_ejd_service"),
    %% Connects to europe_node1
    ComponentConfig = [{server, <<"localhost">>}, {host, <<"localhost">>}, {password, <<"secret">>},
                       {port, service_port()}, {component, <<"test_service">>}],

    {Comp, Addr, _Name} = component_helper:connect_component(ComponentConfig),

    escalus:fresh_story(
      Config, [{eve, 1}],
      fun(Eve) ->
              %% Eve is connected to asia_node
              EveJid = rpc(asia_node, jid, from_binary, [escalus_client:full_jid(Eve)]),
              {ok, <<"reg1">>} = rpc(europe_node1, mod_global_distrib_mapping, for_jid, [EveJid]),
              {ok, <<"reg1">>} = rpc(europe_node2, mod_global_distrib_mapping, for_jid, [EveJid]),

              ok = rpc(asia_node, mod_global_distrib_mapping, delete_for_jid, [EveJid]),
              wait_for_node(europe_node1, EveJid),
              wait_for_node(europe_node2, EveJid),

              %% Component is connected to europe_node1
              %% but we force asia_node to connect to europe_node2 by hiding europe_node1
              %% and forcing rebalance (effectively disabling connections to europe_node1)
              %% to verify routing cache update on both nodes

              %% TODO: Should prevent Redis refresher from executing for a moment,
              %%       as it may collide with this test.

              hide_node(europe_node1, Config),
              {_, EuropeHost, _} = lists:keyfind(europe_node1, 1, get_hosts()),
              trigger_rebalance(asia_node, list_to_binary(EuropeHost)),

              escalus:send(Eve, escalus_stanza:chat_to(Addr, <<"hi">>)),
              escalus:wait_for_stanza(Comp),

              {ok, <<"reg1">>} = rpc(europe_node1, mod_global_distrib_mapping, for_jid, [EveJid]),
              {ok, <<"reg1">>} = rpc(europe_node2, mod_global_distrib_mapping, for_jid, [EveJid])
      end).

%% -------------------------------- Rebalancing --------------------------------

enable_new_endpoint_on_refresh(Config) ->
    get_connection(europe_node1, <<"reg1">>),

    {Enabled1, _Disabled1, Pools1} = get_outgoing_connections(europe_node1, <<"reg1">>),

    ExtraPort = get_port(reg, gd_extra_endpoint_port),
    NewEndpoint = enable_extra_endpoint(asia_node, europe_node1, ExtraPort, Config),

    {Enabled2, _Disabled2, Pools2} = get_outgoing_connections(europe_node1, <<"reg1">>),

    %% One new pool and one new endpoint
    [NewEndpoint] = Pools2 -- Pools1,
    [] = Pools1 -- Pools2,
    [NewEndpoint] = Enabled2 -- Enabled1,
    [] = Enabled1 -- Enabled2.

disable_endpoint_on_refresh(Config) ->
    ExtraPort = get_port(reg, gd_extra_endpoint_port),
    enable_extra_endpoint(asia_node, europe_node1, ExtraPort, Config),

    get_connection(europe_node1, <<"reg1">>),

    {Enabled1, Disabled1, Pools1} = get_outgoing_connections(europe_node1, <<"reg1">>),
    [_, _] = Enabled1,
    [] = Disabled1,

    hide_extra_endpoint(asia_node),
    trigger_rebalance(europe_node1, <<"reg1">>),

    {Enabled2, Disabled2, Pools2} = get_outgoing_connections(europe_node1, <<"reg1">>),

    %% 2 pools open even after disable
    [] = Pools1 -- Pools2,
    [] = Pools2 -- Pools1,
    %% NewEndpoint is no longer enabled
    [] = Enabled2 -- Enabled1,
    [NewEndpoint] = Enabled1 -- Enabled2,
    %% NewEndpoint is now disabled
    [] = Disabled1,
    [NewEndpoint] = Disabled2.

wait_for_connection(Config) ->
    set_endpoints(asia_node, []),
    %% Because of hosts refresher, a pool of connections to asia_node
    %% may already be present here
    mongoose_helper:wait_until(
                                fun () ->
                                    try trigger_rebalance(europe_node1, <<"reg1">>), true
                                    catch _:_ -> false end
                                end,
                                true,
                                #{name => rebalance, time_left => timer:seconds(5)}),

    spawn_connection_getter(europe_node1),

    receive
        Unexpected1 -> error({unexpected, Unexpected1})
    after
        2000 -> ok
    end,

    refresh_mappings(asia_node, "by_wait_for_connection"),
    trigger_rebalance(europe_node1, <<"reg1">>),

    receive
        Conn when is_pid(Conn) -> ok;
        Unexpected2 -> error({unexpected, Unexpected2})
    after
        5000 -> error(timeout)
    end.

closed_connection_is_removed_from_disabled(_Config) ->
    get_connection(europe_node1, <<"reg1">>),
    set_endpoints(asia_node, []),
    trigger_rebalance(europe_node1, <<"reg1">>),

    {[], [_], [_]} = get_outgoing_connections(europe_node1, <<"reg1">>),

    % Will drop connections and prevent them from reconnecting
    restart_receiver(asia_node, [listen_endpoint(get_port(reg, gd_supplementary_endpoint_port))]),

    mongoose_helper:wait_until(fun() -> get_outgoing_connections(europe_node1, <<"reg1">>) end,
                               {[], [], []},
                              #{name => get_outgoing_connections}).


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

get_port(Host, Param) ->
    case ct:get_config({hosts, Host, Param}) of
        Port when is_integer(Port) ->
            Port;
        Other ->
            ct:fail({get_port_failed, Host, Param, Other})
    end.

get_hosts() ->
    [
     {europe_node1, "localhost.bis", get_port(mim, gd_endpoint_port)},
     {europe_node2, "localhost.bis", get_port(mim2, gd_endpoint_port)},
     {asia_node, "reg1", get_port(reg, gd_endpoint_port)}
    ].

listen_endpoint(NodeName) when is_atom(NodeName) ->
    {_, _, Port} = lists:keyfind(NodeName, 1, get_hosts()),
    listen_endpoint(Port);
listen_endpoint(Port) when is_integer(Port) ->
    {{127, 0, 0, 1}, Port}.

rpc(NodeName, M, F, A) ->
    Node = ct:get_config(NodeName),
    mongoose_helper:successful_rpc(Node, M, F, A, timer:seconds(30)).

hide_node(NodeName, Config) ->
    NodesKey = ?config(nodes_key, Config),
    NodeBin = atom_to_binary(ct:get_config(NodeName), latin1),
    {ok, <<"1">>} = redis_query(europe_node1, [<<"HDEL">>, NodesKey, NodeBin]).

connect_from_spec(UserSpec, Config) ->
    {ok, User} = escalus_client:start(Config, UserSpec, <<"res1">>),
    escalus_connection:set_filter_predicate(User, fun(S) -> not escalus_pred:is_presence(S) end),
    escalus_story:send_initial_presence(User),
    User.

chat_with_seqnum(To, Text) ->
    escalus_stanza:set_id(escalus_stanza:chat_to(To, Text),
                          integer_to_binary(erlang:monotonic_time())).

order_by_seqnum(Stanzas) ->
    lists:sort(fun(A, B) -> exml_query:attr(B, <<"id">>) < exml_query:attr(A, <<"id">>) end,
               Stanzas).

has_exactly_one_service(Service, #xmlel{children = [#xmlel{children = Services}]}) ->
    Pred = fun(Item) ->
                   exml_query:attr(Item, <<"jid">>) =:= Service
           end,
    case lists:filter(Pred, Services) of
        [_] -> true;
        _ -> false
    end.

send_steps(From, To, Max, ToHost) ->
    next_send_step(From, To, 1, Max, Max div 10, true, ToHost).

next_send_step(_From, _To, I, Max, _ToReset, _KnowsMapping, _ToHost) when I > Max -> ok;
next_send_step(From, To, I, Max, 0, KnowsMapping, ToHost) ->
    ct:log("Reset: I: ~B", [I]),
    case KnowsMapping of
        true -> delete_mapping(europe_node1, To);
        false -> set_mapping(europe_node1, To, ToHost)
    end,
    next_send_step(From, To, I, Max, Max div 10, not KnowsMapping, ToHost);
next_send_step(From, To, I, Max, ToReset, KnowsMapping, ToHost) ->
    ct:log("I: ~B ~B ~B", [I, Max, ToReset]),
    Stanza = escalus_stanza:chat_to(To, integer_to_binary(I)),
    escalus_client:send(From, Stanza),
    next_send_step(From, To, I + 1, Max, ToReset - 1, KnowsMapping, ToHost).

get_mapping(Node, Client) ->
    {FullJid, _BareJid} = jids(Client),
    {ok, What} = redis_query(Node, [<<"GET">>, FullJid]),
    What.

%% Warning! May not work properly with alice or any other user whose
%% stringprepped JID is different than original one
delete_mapping(Node, Client) ->
    {FullJid, BareJid} = jids(Client),
    redis_query(Node, [<<"DEL">>, FullJid, BareJid]),
    Jid = rpc(Node, jid, from_binary, [FullJid]),
    rpc(Node, mod_global_distrib_mapping, clear_cache, [Jid]).

set_mapping(Node, Client, Mapping) ->
    {FullJid, BareJid} = jids(Client),
    redis_query(Node, [<<"MSET">>, FullJid, Mapping, BareJid, Mapping]),
    Jid = rpc(Node, jid, from_binary, [FullJid]),
    rpc(Node, mod_global_distrib_mapping, clear_cache, [Jid]).

jids(Client) ->
    FullJid = escalus_client:full_jid(Client),
    BareJid = escalus_client:short_jid(Client),
    {FullJid, BareJid}.

redis_query(Node, Query) ->
    {ok, RedisWorker} = rpc(Node, mongoose_wpool, get_worker, [redis, global, global_distrib]),
    rpc(Node, eredis, q, [RedisWorker, Query]).

%% A fake address we don't try to connect to.
%% Used in test_advertised_endpoints_override_endpoints testcase.
advertised_endpoints() ->
    [
     {fake_domain(), get_port(reg, gd_endpoint_port)}
    ].

fake_domain() ->
    "somefakedomain.com".

iptuples_to_string([]) ->
    [];
iptuples_to_string([{Addr, Port} | Endps]) when is_tuple(Addr) ->
    [{inet_parse:ntoa(Addr), Port} | iptuples_to_string(Endps)];
iptuples_to_string([E | Endps]) ->
    [E | iptuples_to_string(Endps)].

maybe_add_advertised_endpoints(NodeName, Opts, Config) ->
    Endpoints = proplists:get_value(NodeName, ?config(add_advertised_endpoints, Config), []),
    case Endpoints of
        [] ->
            Opts;
        E ->
            Connections = case lists:keyfind(connections, 1, Opts) of
                              false -> [];
                              C -> C
                          end,
            NewConnections = {connections, [{advertised_endpoints, E} | Connections]},
            [NewConnections | Opts]
    end.

mock_inet_on_each_node() ->
    Nodes = lists:map(fun({NodeName, _, _}) -> ct:get_config(NodeName) end, get_hosts()),
    Results = lists:map(fun(Node) -> rpc:block_call(Node, ?MODULE, mock_inet, []) end, Nodes),
    true = lists:all(fun(Result) -> Result =:= ok end, Results).

execute_on_each_node(M, F, A) ->
    lists:map(fun({NodeName, _, _}) -> rpc(NodeName, M, F, A) end, get_hosts()).

mock_inet() ->
    %% We don't want to mock inet module itself to avoid strange networking issues
    meck:new(mod_global_distrib_utils, [non_strict, passthrough, unstick]),
    meck:expect(mod_global_distrib_utils, getaddrs, fun(_, inet) -> {ok, [{127, 0, 0, 1}]};
                                                       (_, inet6) -> {error, "No ipv6 address"} end).

unmock_inet(_Pids) ->
    execute_on_each_node(meck, unload, [mod_global_distrib_utils]).

out_connection_sups(Node) ->
    Children = rpc(Node, supervisor, which_children, [mod_global_distrib_outgoing_conns_sup]),
    lists:filter(fun({Sup, _, _, _}) -> Sup =/= mod_global_distrib_hosts_refresher end, Children).

trees_for_connections_present() ->
    AsiaChildren = out_connection_sups(asia_node),
    Europe1Children = out_connection_sups(europe_node1),
    Europe2Children = out_connection_sups(europe_node2),
    lists:all(fun(Host) -> length(Host) > 0 end, [AsiaChildren, Europe1Children, Europe2Children]).

tree_for_sup_present(Node, ExpectedSup) ->
    Children = out_connection_sups(Node),
    lists:keyfind(ExpectedSup, 1, Children) =/= false.


%% ------------------------------- rebalancing helpers -----------------------------------

spawn_connection_getter(SenderNode) ->
    TestPid = self(),
    spawn(fun() ->
                  Conn = get_connection(SenderNode, <<"reg1">>),
                  TestPid ! Conn
          end).

enable_extra_endpoint(ListenNode, SenderNode, Port, Config) ->
    OriginalEndpoint = listen_endpoint(ListenNode),
    NewEndpoint = {{127, 0, 0, 1}, Port},

    restart_receiver(ListenNode, [NewEndpoint, OriginalEndpoint]),
    refresh_mappings(ListenNode, "by_enable_extra_endpoint,port=" ++ integer_to_list(Port)),
    trigger_rebalance(SenderNode, <<"reg1">>),

    NewEndpoint.

get_connection(SenderNode, ToDomain) ->
    rpc(SenderNode, mod_global_distrib_outgoing_conns_sup, get_connection, [ToDomain]).

hide_extra_endpoint(ListenNode) ->
    set_endpoints(ListenNode, [listen_endpoint(ListenNode)]).

set_endpoints(ListenNode, Endpoints) ->
    {ok, _} = rpc(ListenNode, mod_global_distrib_mapping_redis, set_endpoints, [Endpoints]).

get_outgoing_connections(NodeName, DestinationDomain) ->
    Supervisor = rpc(NodeName, mod_global_distrib_utils, server_to_sup_name, [DestinationDomain]),
    Manager = rpc(NodeName, mod_global_distrib_utils, server_to_mgr_name, [DestinationDomain]),
    Enabled = rpc(NodeName, mod_global_distrib_server_mgr,
                  get_enabled_endpoints, [DestinationDomain]),
    Disabled = rpc(NodeName, mod_global_distrib_server_mgr,
                   get_disabled_endpoints, [DestinationDomain]),
    PoolsChildren = rpc(NodeName, supervisor, which_children, [Supervisor]),
    Pools = [ Id || {Id, _Child, _Type, _Modules} <- PoolsChildren, Id /= Manager ],
    {Enabled, Disabled, Pools}.

restart_receiver(NodeName) ->
    restart_receiver(NodeName, [listen_endpoint(NodeName)]).

restart_receiver(NodeName, NewEndpoints) ->
    OldOpts = rpc(NodeName, gen_mod, get_module_opts,
                  [<<"localhost">>, mod_global_distrib_receiver]),
    NewOpts = lists:keyreplace(endpoints, 1, OldOpts, {endpoints, NewEndpoints}),
    {ok, _} = rpc(NodeName, gen_mod, reload_module,
             [<<"localhost">>, mod_global_distrib_receiver, NewOpts]).

trigger_rebalance(NodeName, DestinationDomain) when is_binary(DestinationDomain) ->
    %% To ensure that the manager exists,
    %% otherwise we can get noproc error in the force_refresh call
    ok = rpc(NodeName, mod_global_distrib_outgoing_conns_sup,
             ensure_server_started, [DestinationDomain]),
    rpc(NodeName, mod_global_distrib_server_mgr, force_refresh, [DestinationDomain]),
    StateInfo = rpc(NodeName, mod_global_distrib_server_mgr, get_state_info, [DestinationDomain]),
    ct:log("mgr_state_info_after_rebalance nodename=~p state_info=~p", [NodeName, StateInfo]),
    timer:sleep(1000).

%% -----------------------------------------------------------------------
%% Escalus-related helpers

user_receives(User, Bodies) ->
    ExpectedLength = length(Bodies),
    Messages = escalus_client:wait_for_stanzas(User, ExpectedLength),
    SortedMessages = order_by_seqnum(Messages),
    case length(Messages) of
        ExpectedLength ->
            Checks = [escalus_pred:is_chat_message(Body, Stanza) || {Body, Stanza} <- lists:zip(Bodies, SortedMessages)],
            case lists:all(fun(Check) -> Check end, Checks) of
                true ->
                    ok;
                false ->
                    ct:fail({user_receives_failed, {wanted, Bodies}, {received, SortedMessages}, {check, Checks}})
            end;
        _ ->
            ct:fail({user_receives_not_enough, {wanted, Bodies}, {received, SortedMessages}})
    end.


%% -----------------------------------------------------------------------
%% Refreshing helpers

%% Reason is a string
%% NodeName is asia_node, europe_node2, ... in a format used by this suite.
refresh_mappings(NodeName, Reason) when is_list(Reason) ->
    rpc(NodeName, mod_global_distrib_mapping_redis, refresh, [Reason]).

refresh_hosts(NodeNames, Reason) ->
   [refresh_mappings(NodeName, Reason) || NodeName <- NodeNames].


%% -----------------------------------------------------------------------
%% Other helpers

connect_steps_with_sm() ->
    [start_stream, stream_features, maybe_use_ssl,
     authenticate, bind, session, stream_resumption].

bare_client(Client) ->
    Client#client{jid = escalus_utils:get_short_jid(Client)}.

service_port() ->
    ct:get_config({hosts, mim, service_port}).


%% -----------------------------------------------------------------------
%% Waiting helpers

wait_for_domain(Node, Domain) ->
    F = fun() ->
        {ok, Domains} = rpc:call(Node, mod_global_distrib_mapping, all_domains, []),
        lists:member(Domain, Domains)
        end,
    mongoose_helper:wait_until(F, true, #{name => {wait_for_domain, Node, Domain}}).

%% We receive presence BEFORE session is registered in ejabberd_sm.
%% So, to ensure that we processed do_open_session completely, let's send a "ping".
%% by calling the c2s process.
%% That call would only return, when all messages in erlang message queue
%% are processed.
wait_for_registration(Client, Node) ->
    mongoose_helper:wait_until(fun() -> is_pid(mongoose_helper:get_session_pid(Client, Node)) end, true,
                               #{name => wait_for_session}),
    C2sPid = mongoose_helper:get_session_pid(Client, Node),
    rpc:call(node(C2sPid), ejabberd_c2s, get_info, [C2sPid]),
    ok.


%% -----------------------------------------------------------------------
%% Ensure, that endpoints are up

wait_for_listeners_to_appear() ->
    [wait_for_can_connect_to_port(Port) || Port <- receiver_ports(get_hosts())].

receiver_ports(Hosts) ->
    lists:map(fun({_NodeName, _LocalHost, ReceiverPort}) -> ReceiverPort end, Hosts).

wait_for_can_connect_to_port(Port) ->
    Opts = #{time_left => timer:seconds(30), sleep_time => 1000, name => {can_connect_to_port, Port}},
    mongoose_helper:wait_until(fun() -> can_connect_to_port(Port) end, true, Opts).

can_connect_to_port(Port) ->
    case gen_tcp:connect("127.0.0.1", Port, []) of
        {ok, Sock} ->
            gen_tcp:close(Sock),
            true;
        Other ->
            ct:pal("can_connect_to_port port=~p result=~p", [Port, Other]),
            false
    end.

%% -----------------------------------------------------------------------
%% Custom log levels for GD modules during the tests

enable_logging() ->
    mim_loglevel:enable_logging(test_hosts(), custom_loglevels()).

disable_logging() ->
    mim_loglevel:disable_logging(test_hosts(), custom_loglevels()).

custom_loglevels() ->
    %% for "s2s connection to muc.localhost not found" debugging
    [{ejabberd_s2s, debug},
    %% for debugging event=refreshing_own_data_done
     {mod_global_distrib_mapping_redis, info},
    %% to know if connection is already started or would be started
    %% event=outgoing_conn_start_progress
     {mod_global_distrib_outgoing_conns_sup, info},
    %% to debug bound connection issues
     {mod_global_distrib, debug},
    %% to know all new connections pids
     {mod_global_distrib_connection, debug},
    %% to check if gc or refresh is triggered
     {mod_global_distrib_server_mgr, info},
   %% To debug incoming connections
%    {mod_global_distrib_receiver, info},
   %% to debug global session set/delete
     {mod_global_distrib_mapping, debug}
    ].

test_hosts() -> [mim, mim2, reg].


%% -----------------------------------------------------------------------
%% Module loading/restoration with multi node support

loaded_modules_with_opts(NodeName, VirtHost) ->
    rpc(NodeName, gen_mod, loaded_modules_with_opts, [VirtHost]).

save_modules(NodeName, VirtHosts) ->
    [{{old_mods, NodeName, VirtHost}, loaded_modules_with_opts(NodeName, VirtHost)}
     || VirtHost <- VirtHosts].

restore_modules(NodeName, VirtHost, Config) ->
    CurrentMods = loaded_modules_with_opts(NodeName, VirtHost),
    case ?config({old_mods, NodeName, VirtHost}, Config) of
        OldMods when is_list(OldMods) ->
            rpc(NodeName, gen_mod_deps, replace_modules,
                [VirtHost, CurrentMods, OldMods]);
        Other ->
            ct:fail({replace_modules_failed, NodeName, VirtHost, Other})
    end.

%% -----------------------------------------------------------------------
