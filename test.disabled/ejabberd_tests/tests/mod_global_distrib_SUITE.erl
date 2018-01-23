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
     {group, rebalancing}
    ].

groups() ->
    [{mod_global_distrib, [shuffle],
      [
       test_pm_between_users_at_different_locations,
       test_pm_between_users_before_available_presence,
       test_muc_conversation_on_one_host,
       test_component_disconnect,
       test_component_on_one_host,
       test_pm_with_disconnection_on_other_server,
       test_pm_with_graceful_reconnection_to_different_server,
       test_pm_with_ungraceful_reconnection_to_different_server,
       test_global_disco,
       test_component_unregister,
       test_update_senders_host,
       test_update_senders_host_by_ejd_service
       %% TODO: Add test case fo global_distrib_addr option
      ]},
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
       % TODO: Add checks for other cache updates
       refresh_nodes
      ]},
     {multi_connection, [shuffle],
      [
       test_in_order_messages_on_multiple_connections,
       test_muc_conversation_history,
       test_in_order_messages_on_multiple_connections_with_bounce,
       test_messages_bounced_in_order
      ]},
     {rebalancing, [shuffle],
      [
       enable_new_endpoint_on_refresh,
       disable_endpoint_on_refresh,
       wait_for_connection,
       closed_connection_is_removed_from_disabled
      ]}
    ].

suite() ->
    [{require, europe_node1, {hosts, mim, node}},
     {require, europe_node2, {hosts, mim2, node}},
     {require, asia_node, {hosts, fed, node}} |
     escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    case {rpc(europe_node1, eredis, start_link, []), rpc(asia_node, eredis, start_link, [])} of
        {{ok, _}, {ok, _}} ->
            ok = rpc(europe_node2, mongoose_cluster, join, [ct:get_config(europe_node1)]),

            CertDir = filename:join(?config(data_dir, Config), "../../priv/ssl"),
            CertPath = canonicalize_path(filename:join(CertDir, "fake_cert.pem")),
            CACertPath = canonicalize_path(filename:join(CertDir, "cacert.pem")),
            escalus:init_per_suite([{certfile, CertPath}, {cafile, CACertPath},
                                    {extra_config, []}, {redis_extra_config, []} | Config]);
        _ ->
            {skip, "Cannot connect to Redis server on 127.0.0.1 6379"}
    end.

end_per_suite(Config) ->
    rpc(europe_node2, mongoose_cluster, leave, [ct:get_config(europe_node1)]),
    escalus:end_per_suite(Config).

init_per_group(start_checks, Config) ->
    Config;
init_per_group(multi_connection, Config) ->
    ExtraConfig = [{resend_after_ms, 20000}, {connections_per_endpoint, 100}],
    init_per_group(multi_connection_generic, [{extra_config, ExtraConfig} | Config]);
init_per_group(invalidation, Config) ->
    Config1 = init_per_group(invalidation_generic, Config),
    NodeBin = <<"fake_node@localhost">>,
    [{node_to_expire, NodeBin} | Config1];
init_per_group(rebalancing, Config) ->
    %% We need to prevent automatic refreshes, because they may interfere with tests
    %% and we need early disabled garbage collection to check its validity
    ExtraConfig = [{endpoint_refresh_interval, 3600},
                   {disabled_gc_interval, 1}],
    RedisExtraConfig = [{refresh_after, 3600}],
    init_per_group(rebalancing_generic, [{extra_config, ExtraConfig},
                                         {redis_extra_config, RedisExtraConfig} | Config]);
init_per_group(_, Config0) ->
    Config2 =
        lists:foldl(
          fun({NodeName, LocalHost, ReceiverPort}, Config1) ->
                  Opts = ?config(extra_config, Config1) ++
		         [{local_host, LocalHost},
                          {global_host, "localhost"},
                          {endpoints, [listen_endpoint(ReceiverPort)]},
                          {tls_opts, [
                                      {certfile, ?config(certfile, Config1)},
                                      {cafile, ?config(cafile, Config1)}
                                     ]},
                          {redis, [{port, 6379} | ?config(redis_extra_config, Config1)]},
                          {resend_after_ms, 500}],

                  OldMods = rpc(NodeName, gen_mod, loaded_modules_with_opts, [<<"localhost">>]),
                  rpc(NodeName, gen_mod_deps, start_modules,
                      [<<"localhost">>, [{mod_global_distrib, Opts}]]),
                  rpc(NodeName, gen_mod, stop_module, [<<"localhost">>, mod_offline]),
                  ResumeTimeout = rpc(NodeName, mod_stream_management, get_resume_timeout, [1]),
                  true = rpc(NodeName, mod_stream_management, set_resume_timeout, [1]),

                  EjdSupChildren = rpc(NodeName, supervisor, which_children, [ejabberd_sup]),
                  {_, MapperPid, _ , _} = lists:keyfind(
                                            mod_global_distrib_redis_refresher, 1, EjdSupChildren),

                  [
                   {{mapper_pid, NodeName}, MapperPid},
                   {{old_mods, NodeName}, OldMods},
                   {{resume_timeout, NodeName}, ResumeTimeout} |
                   Config1
                  ]
          end,
          Config0,
          get_hosts()),

    {SomeNode, _, _} = hd(get_hosts()),
    NodesKey = rpc(SomeNode, mod_global_distrib_mapping_redis, nodes_key, []),
    [{nodes_key, NodesKey}, {escalus_user_db, xmpp} | Config2].

end_per_group(start_checks, Config) ->
    Config;
end_per_group(invalidation, Config) ->
    redis_query(europe_node1, [<<"HDEL">>, ?config(nodes_key, Config),
                            ?config(node_to_expire, Config)]),
    end_per_group(invalidation_generic, Config);
end_per_group(_, Config) ->
    lists:foreach(
      fun({NodeName, _, _}) ->
              CurrentMods = rpc(NodeName, gen_mod, loaded_modules_with_opts, [<<"localhost">>]),
              rpc(NodeName, gen_mod_deps, replace_modules,
                  [<<"localhost">>, CurrentMods, ?config({old_mods, NodeName}, Config)]),

              rpc(NodeName, mod_stream_management, set_resume_timeout,
                  [?config({resume_timeout, NodeName}, Config)])
      end,
      get_hosts()),
    escalus_fresh:clean().

init_per_testcase(CaseName, Config)
  when CaseName == test_muc_conversation_on_one_host; CaseName == test_global_disco;
       CaseName == test_muc_conversation_history ->
    %% There is no helper to load MUC on node2
    %% For now it's easier to hide node2
    %% TODO: Do it right at some point!
    hide_node(europe_node2, Config),
    muc_helper:load_muc(<<"muc.localhost">>),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config)
  when CaseName == test_muc_conversation_on_one_host; CaseName == test_global_disco;
       CaseName == test_muc_conversation_history ->
    refresh_node(europe_node2, Config),
    muc_helper:unload_muc(),
    generic_end_per_testcase(CaseName, Config);
end_per_testcase(test_update_senders_host_by_ejd_service = CN, Config) ->
    refresh_node(europe_node1, Config),
    generic_end_per_testcase(CN, Config);
end_per_testcase(CN, Config) when CN == enable_new_endpoint_on_refresh;
                                  CN == disable_endpoint_on_refresh;
                                  CN == wait_for_connection;
                                  CN == closed_connection_is_removed_from_disabled ->
    restart_receiver(asia_node),
    refresh_mappings(asia_node, Config),
    generic_end_per_testcase(CN, Config);
end_per_testcase(CaseName, Config) ->
    generic_end_per_testcase(CaseName, Config).

generic_end_per_testcase(CaseName, Config) ->
    lists:foreach(
      fun({NodeName, _, _}) ->
              Node = ct:get_config(NodeName),
              SupRef = {mod_global_distrib_outgoing_conns_sup, Node},
              try
                  OutgoingConns = supervisor:which_children(SupRef),
                  lists:foreach(fun({Id, _, _, _}) ->
                                        supervisor:terminate_child(SupRef, Id)
                                end, OutgoingConns),
                  [] = supervisor:which_children(SupRef)
              catch
                  _:{noproc, _} ->
                      ct:pal("Sender supervisor not found in ~p", [NodeName])
              end
      end,
      get_hosts()),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------

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
    escalus_client:send(Alice, escalus_stanza:chat_to(Eve, <<"Hi from Europe1!">>)),
    escalus_client:send(Eve, escalus_stanza:chat_to(Alice, <<"Hi from Asia!">>)),

    FromAlice = escalus_client:wait_for_stanza(Eve),
    FromEve = escalus_client:wait_for_stanza(Alice),

    AliceJid = escalus_client:full_jid(Alice),
    EveJid = escalus_client:full_jid(Eve),

    escalus:assert(is_chat_message_from_to, [AliceJid, EveJid, <<"Hi from Europe1!">>],
                   FromAlice),
    escalus:assert(is_chat_message_from_to, [EveJid, AliceJid, <<"Hi from Asia!">>],
                   FromEve).

test_muc_conversation_on_one_host(Config0) ->
    AliceSpec = muc_SUITE:given_fresh_spec(Config0, alice),
    Config = muc_SUITE:given_fresh_room(Config0, AliceSpec, []),
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
    AliceSpec = muc_SUITE:given_fresh_spec(Config0, alice),
    Config = muc_SUITE:given_fresh_room(Config0, AliceSpec, []),
    escalus:fresh_story(
      Config, [{eve, 1}],
      fun(Eve) ->
              Alice = connect_from_spec(AliceSpec, Config),

              RoomJid = ?config(room, Config),
              AliceUsername = escalus_utils:get_username(Alice),
              RoomAddr = muc_helper:room_address(RoomJid),

              escalus:send(Alice, muc_helper:stanza_muc_enter_room(RoomJid, AliceUsername)),
              escalus:wait_for_stanza(Alice),

              lists:foreach(fun(I) ->
                                    Msg = <<"test-", (integer_to_binary(I))/binary>>,
                                    escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, Msg))
                            end, lists:seq(1, 3)),

              EveUsername = escalus_utils:get_username(Eve),
              escalus:send(Eve, muc_helper:stanza_muc_enter_room(RoomJid, EveUsername)),

              AlicePresence = escalus:wait_for_stanza(Eve),
              escalus:assert(is_presence, AlicePresence),
              ?assertEqual(muc_helper:room_address(RoomJid, AliceUsername),
                           exml_query:attr(AlicePresence, <<"from">>)),

              MyRoomPresence = escalus:wait_for_stanza(Eve),
              escalus:assert(is_presence, MyRoomPresence),
              ?assertEqual(muc_helper:room_address(RoomJid, EveUsername),
                           exml_query:attr(MyRoomPresence, <<"from">>)),

              lists:foreach(fun(J) ->
                                    Msg = <<"test-", (integer_to_binary(J))/binary>>,
                                    Stanza = escalus:wait_for_stanza(Eve),
                                    escalus:assert(is_groupchat_message, [Msg], Stanza)
                            end, lists:seq(1, 3)),

              Subject = escalus:wait_for_stanza(Eve),
              escalus:assert(is_groupchat_message, Subject),
              ?assertNotEqual(undefined, exml_query:subelement(Subject, <<"subject">>))
      end),
    muc_helper:destroy_room(Config).

test_component_on_one_host(Config) ->
    ComponentConfig = [{server, <<"localhost">>}, {host, <<"localhost">>}, {password, <<"secret">>},
                       {port, 8888}, {component, <<"test_service">>}],

    {Comp, Addr, _Name} = component_SUITE:connect_component(ComponentConfig),

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

test_component_disconnect(Config) ->
    ComponentConfig = [{server, <<"localhost">>}, {host, <<"localhost">>}, {password, <<"secret">>},
                       {port, 8888}, {component, <<"test_service">>}],

    {Comp, Addr, _Name} = component_SUITE:connect_component(ComponentConfig),
    component_SUITE:disconnect_component(Comp, Addr),

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
    EveSpec = muc_SUITE:given_fresh_spec(Config, eve),
    escalus:create_users(Config, [{eve, [{port, 5222} | EveSpec]}]),
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              Eve = connect_from_spec(EveSpec, Config),

              escalus_client:send(Eve, escalus_stanza:chat_to(Alice, <<"Hi from Asia!">>)),
              escalus_connection:stop(Eve),

              FromEve = escalus_client:wait_for_stanza(Alice),

              escalus_client:send(Alice, chat_with_seqnum(Eve, <<"Hi from Europe1!">>)),
              NewEve = connect_from_spec([{port, 5222} | EveSpec], Config),

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
      end),
    escalus_users:delete_users(Config, [{eve, [{port, 5222} | EveSpec]}]).

test_pm_with_ungraceful_reconnection_to_different_server(Config0) ->
    Config = escalus_users:update_userspec(Config0, eve, stream_management, true),
    EveSpec = muc_SUITE:given_fresh_spec(Config, eve),
    escalus:create_users(Config, [{eve, [{port, 5222} | EveSpec]}]),
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              StepsWithSM = [start_stream, stream_features, maybe_use_ssl,
                             authenticate, bind, session, stream_resumption],

              {ok, Eve0, _} = escalus_connection:start(EveSpec, StepsWithSM),
              Eve = Eve0#client{jid = sm_SUITE:get_bjid(EveSpec)},
              escalus_story:send_initial_presence(Eve),
              escalus_client:wait_for_stanza(Eve),

              escalus_connection:kill(Eve),

              escalus_client:send(Alice, chat_with_seqnum(Eve, <<"Hi from Europe1!">>)),

              NewEve = connect_from_spec([{port, 5222} | EveSpec], Config),
              ct:sleep(timer:seconds(1)), % without it, on very slow systems (e.g. travis),
                                          % global_distrib correctly routes "hi again from eu"
                                          % message to local host, but it's rejected by some
                                          % underlying mechanism (presence unavailable?)

              escalus_client:send(Alice, chat_with_seqnum(Eve, <<"Hi again from Europe1!">>)),
              escalus_client:send(NewEve, escalus_stanza:chat_to(Alice, <<"Hi from Asia!">>)),

              FirstFromAlice = escalus_client:wait_for_stanza(NewEve, timer:seconds(10)),
              SecondFromAlice = escalus_client:wait_for_stanza(NewEve, timer:seconds(10)),
              AgainFromEve = escalus_client:wait_for_stanza(Alice),

              [FromAlice, AgainFromAlice] = order_by_seqnum([FirstFromAlice, SecondFromAlice]),

              escalus:assert(is_chat_message, [<<"Hi from Europe1!">>], FromAlice),
              escalus:assert(is_chat_message, [<<"Hi again from Europe1!">>], AgainFromAlice),
              escalus:assert(is_chat_message, [<<"Hi from Asia!">>], AgainFromEve)
      end),
    escalus_users:delete_users(Config, [{eve, [{port, 5222} | EveSpec]}]).

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
                       {port, 8888}, {component, <<"test_service">>}],

    {Comp, Addr, _Name} = component_SUITE:connect_component(ComponentConfig),
    ?assertMatch({ok, _}, rpc(europe_node1, mod_global_distrib_mapping, for_domain,
                              [<<"test_service.localhost">>])),

    component_SUITE:disconnect_component(Comp, Addr),

    ?assertEqual(error, rpc(europe_node1, mod_global_distrib_mapping, for_domain,
                            [<<"test_service.localhost">>])).

test_error_on_wrong_hosts(_Config) ->
    Opts = [{cookie, "cookie"}, {local_host, "no_such_host"}, {global_host, "localhost"}],
    Result = rpc(europe_node1, gen_mod, start_module, [<<"localhost">>, mod_global_distrib, Opts]),
    ?assertMatch({badrpc, {'EXIT', {"no_such_host is not a member of the host list", _}}}, Result).

refresh_nodes(Config) ->
    NodesKey = ?config(nodes_key, Config),
    NodeBin = ?config(node_to_expire, Config),
    redis_query(europe_node1, [<<"HSET">>, NodesKey, NodeBin, <<"0">>]),
    refresh_mappings(europe_node1, Config),
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
              send_steps(Alice, Eve, 99, <<"fed1">>),
              %% Make sure that the last message is sent when the mapping is known
              set_mapping(europe_node1, Eve, <<"fed1">>),
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
              set_mapping(europe_node1, Eve, <<"fed1">>),

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
              GetCachesFun
              = fun() ->
                        rpc(asia_node, mod_global_distrib_mapping, for_jid, [AliceJid])
                end,
              wait_until(GetCachesFun, error, 10, 1000),

              %% TODO: Should prevent Redis refresher from executing for a moment,
              %%       as it may collide with this test.

              escalus:send(Alice, escalus_stanza:chat_to(Eve, <<"hi">>)),
              escalus:wait_for_stanza(Eve),

              {ok, <<"localhost.bis">>}
              = rpc(asia_node, mod_global_distrib_mapping, for_jid, [AliceJid])
      end).

test_update_senders_host_by_ejd_service(Config) ->
    %% Connects to europe_node1
    ComponentConfig = [{server, <<"localhost">>}, {host, <<"localhost">>}, {password, <<"secret">>},
                       {port, 8888}, {component, <<"test_service">>}],

    {Comp, Addr, _Name} = component_SUITE:connect_component(ComponentConfig),

    escalus:fresh_story(
      Config, [{eve, 1}],
      fun(Eve) ->
              %% Eve is connected to asia_node
              EveJid = rpc(asia_node, jid, from_binary, [escalus_client:full_jid(Eve)]),
              {ok, <<"fed1">>} = rpc(europe_node1, mod_global_distrib_mapping, for_jid, [EveJid]),
              {ok, <<"fed1">>} = rpc(europe_node2, mod_global_distrib_mapping, for_jid, [EveJid]),

              ok = rpc(asia_node, mod_global_distrib_mapping, delete_for_jid, [EveJid]),
              GetCachesFun
              = fun() ->
                        {
                         rpc(europe_node1, mod_global_distrib_mapping, for_jid, [EveJid]),
                         rpc(europe_node2, mod_global_distrib_mapping, for_jid, [EveJid])
                        }
                end,
              wait_until(GetCachesFun, {error, error}, 10, 1000),

              %% Component is connected to europe_node1
              %% but we force asia_node to connect to europe_node2 by hiding europe_node1
              %% to verify routing cache update on both nodes

              %% TODO: Should prevent Redis refresher from executing for a moment,
              %%       as it may collide with this test.

              hide_node(europe_node1, Config),

              escalus:send(Eve, escalus_stanza:chat_to(Addr, <<"hi">>)),
              escalus:wait_for_stanza(Comp),

              {ok, <<"fed1">>} = rpc(europe_node1, mod_global_distrib_mapping, for_jid, [EveJid]),
              {ok, <<"fed1">>} = rpc(europe_node2, mod_global_distrib_mapping, for_jid, [EveJid])
      end).

%% -------------------------------- Rebalancing --------------------------------

enable_new_endpoint_on_refresh(Config) ->
    get_connection(europe_node1, <<"fed1">>),

    {Enabled1, _Disabled1, Pools1} = get_outgoing_connections(europe_node1, <<"fed1">>),

    NewEndpoint = enable_extra_endpoint(asia_node, europe_node1, 10000, Config),

    {Enabled2, _Disabled2, Pools2} = get_outgoing_connections(europe_node1, <<"fed1">>),

    %% One new pool and one new endpoint
    [NewEndpoint] = Pools2 -- Pools1,
    [] = Pools1 -- Pools2,
    [NewEndpoint] = Enabled2 -- Enabled1,
    [] = Enabled1 -- Enabled2.

disable_endpoint_on_refresh(Config) ->
    enable_extra_endpoint(asia_node, europe_node1, 10000, Config),

    get_connection(europe_node1, <<"fed1">>),

    {Enabled1, Disabled1, Pools1} = get_outgoing_connections(europe_node1, <<"fed1">>),
    [_, _] = Enabled1,
    [] = Disabled1,

    hide_extra_endpoint(asia_node),
    trigger_rebalance(europe_node1, <<"fed1">>),

    {Enabled2, Disabled2, Pools2} = get_outgoing_connections(europe_node1, <<"fed1">>),

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

    spawn_connection_getter(europe_node1),

    receive
        Unexpected1 -> error({unexpected, Unexpected1})
    after
        2000 -> ok
    end,

    refresh_mappings(asia_node, Config),
    trigger_rebalance(europe_node1, <<"fed1">>),

    receive
        Conn when is_pid(Conn) -> ok;
        Unexpected2 -> error({unexpected, Unexpected2})
    after
        5000 -> error(timeout)
    end.

closed_connection_is_removed_from_disabled(_Config) ->
    get_connection(europe_node1, <<"fed1">>),
    set_endpoints(asia_node, []),
    trigger_rebalance(europe_node1, <<"fed1">>),

    {[], [_], [_]} = get_outgoing_connections(europe_node1, <<"fed1">>),

    % Will drop connections and prevent them from reconnecting
    restart_receiver(asia_node, [listen_endpoint(10001)]),

    wait_until(fun() -> get_outgoing_connections(europe_node1, <<"fed1">>) end,
               {[], [], []}, 5, 1000).

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

get_hosts() ->
    [
     {europe_node1, "localhost.bis", 5555},
     {europe_node2, "localhost.bis", 6666},
     {asia_node, "fed1", 7777}
    ].

listen_endpoint(NodeName) when is_atom(NodeName) ->
    {_, _, Port} = lists:keyfind(NodeName, 1, get_hosts()),
    listen_endpoint(Port);
listen_endpoint(Port) ->
    {{127, 0, 0, 1}, Port}.

rpc(NodeName, M, F, A) ->
    Node = ct:get_config(NodeName),
    ct_rpc:call(Node, M, F, A).

wait_until(Fun, ExpectedValue, Attempts, SleepTime) ->
    wait_until(Fun, ExpectedValue, Attempts, SleepTime, []).

wait_until(_Fun, _ExpectedValue, 0, _SleepTime, History) ->
    error({badmatch, History});
wait_until(Fun, ExpectedValue, AttemptsLeft, SleepTime, History) when AttemptsLeft > 0 ->
    case Fun() of
        ExpectedValue ->
            ok;
        OtherValue ->
            timer:sleep(SleepTime),
            wait_until(Fun, ExpectedValue, AttemptsLeft - 1, SleepTime, [OtherValue | History])
    end.

hide_node(NodeName, Config) ->
    NodesKey = ?config(nodes_key, Config),
    NodeBin = atom_to_binary(ct:get_config(NodeName), latin1),
    {ok, <<"1">>} = redis_query(europe_node1, [<<"HDEL">>, NodesKey, NodeBin]).

refresh_node(NodeName, Config) ->
    ?config({mapper_pid, NodeName}, Config) ! refresh.

connect_from_spec(UserSpec, Config) ->
    {ok, User} = escalus_client:start(Config, UserSpec, <<"res1">>),
    escalus_story:send_initial_presence(User),
    escalus_connection:set_filter_predicate(User, fun(S) -> not escalus_pred:is_presence(S) end),
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

canonicalize_path(Path) -> canonicalize_path(filename:split(Path), []).

canonicalize_path([], Acc) -> filename:join(lists:reverse(Acc));
canonicalize_path([".." | Path], [_ | Acc]) -> canonicalize_path(Path, Acc);
canonicalize_path(["." | Path], Acc) -> canonicalize_path(Path, Acc);
canonicalize_path([Elem | Path], Acc) -> canonicalize_path(Path, [Elem | Acc]).

send_steps(From, To, Max, ToHost) ->
    next_send_step(From, To, 1, Max, Max div 10, true, ToHost).

next_send_step(_From, _To, I, Max, _ToReset, _KnowsMapping, _ToHost) when I > Max -> ok;
next_send_step(From, To, I, Max, 0, KnowsMapping, ToHost) ->
    ct:print("Reset: I: ~B", [I]),
    case KnowsMapping of
        true -> delete_mapping(europe_node1, To);
        false -> set_mapping(europe_node1, To, ToHost)
    end,
    next_send_step(From, To, I, Max, Max div 10, not KnowsMapping, ToHost);
next_send_step(From, To, I, Max, ToReset, KnowsMapping, ToHost) ->
    ct:print("I: ~B ~B ~B", [I, Max, ToReset]),
    Stanza = escalus_stanza:chat_to(To, integer_to_binary(I)),
    escalus_client:send(From, Stanza),
    next_send_step(From, To, I + 1, Max, ToReset - 1, KnowsMapping, ToHost).

get_mapping(Node, Client) ->
    {FullJid, _BareJid} = jids(Client),
    {ok, What} = redis_query(Node, [<<"GET">>, FullJid]),
    What.

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
    RedisWorker = rpc(Node, wpool_pool, best_worker, [mod_global_distrib_mapping_redis]),
    rpc(Node, eredis, q, [RedisWorker, Query]).

%% ------------------------------- rebalancing helpers -----------------------------------

spawn_connection_getter(SenderNode) ->
    TestPid = self(),
    spawn(fun() ->
                  Conn = get_connection(SenderNode, <<"fed1">>),
                  TestPid ! Conn
          end).

enable_extra_endpoint(ListenNode, SenderNode, Port, Config) ->
    OriginalEndpoint = listen_endpoint(ListenNode),
    NewEndpoint = {{127, 0, 0, 1}, Port},

    restart_receiver(ListenNode, [NewEndpoint, OriginalEndpoint]),
    refresh_mappings(ListenNode, Config),
    trigger_rebalance(SenderNode, <<"fed1">>),

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
    ok = rpc(NodeName, gen_mod, reload_module,
             [<<"localhost">>, mod_global_distrib_receiver, NewOpts]).

refresh_mappings(NodeName, Config) ->
    ?config({mapper_pid, NodeName}, Config) ! refresh,
    timer:sleep(1000).

trigger_rebalance(NodeName, DestinationDomain) ->
    rpc(NodeName, mod_global_distrib_server_mgr, force_refresh, [DestinationDomain]),
    timer:sleep(1000).
