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
     {group, cluster_restart}
    ].

groups() ->
    [{mod_global_distrib, [],
      [
       test_pm_between_users_at_different_locations,
       test_muc_conversation_on_one_host,
       test_component_on_one_host,
       test_component_disconnect,
       test_pm_with_disconnection_on_other_server,
       test_pm_with_graceful_reconnection_to_different_server,
       test_pm_with_ungraceful_reconnection_to_different_server
      ]},
     {cluster_restart, [],
      [
       test_location_disconnect
      ]}].

suite() ->
    [{require, europe_node, {hosts, mim, node}},
     {require, asia_node, {hosts, fed, node}} |
     escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config0) ->
    Config2 =
        lists:foldl(
          fun({NodeName, LocalHost}, Config1) ->
                  Opts = [{cookie, "cookie"}, {local_host, LocalHost}, {global_host, "localhost"}],
                  rpc(NodeName, gen_mod, start_module, [<<"localhost">>, mod_global_distrib, Opts]),
                  rpc(NodeName, gen_mod, start_module, [<<"localhost">>, mod_global_distrib_bounce, Opts]),
                  HasOffline = rpc(NodeName, gen_mod, is_loaded, [<<"localhost">>, mod_offline]),
                  rpc(NodeName, gen_mod, stop_module_keep_config, [<<"localhost">>, mod_offline]),
                  ResumeTimeout = rpc(NodeName, mod_stream_management, get_resume_timeout, [1]),
                  rpc(NodeName, mod_stream_management, set_resume_timeout, [1]),
                  [
                   {{has_offline, NodeName}, HasOffline},
                   {{resume_timeout, NodeName}, ResumeTimeout} |
                   Config1
                  ]
          end,
          Config0,
          get_hosts()),
    Config3 = [{escalus_user_db, xmpp} | Config2],
    escalus:create_users(Config3, escalus:get_users([alice, eve])).

end_per_group(_, Config) ->
    lists:foreach(
      fun({NodeName, _}) ->
              rpc(NodeName, gen_mod, stop_module, [<<"localhost">>, mod_global_distrib_bounce]),
              rpc(NodeName, gen_mod, stop_module, [<<"localhost">>, mod_global_distrib]),

              ResumeTimeout = proplists:get_value({resume_timeout, NodeName}, Config),
              rpc(NodeName, mod_stream_management, set_resume_timeout, [ResumeTimeout]),

              case proplists:get_value({has_offline, NodeName}, Config) of
                  true ->
                      OffOpts = rpc(NodeName, gen_mod, get_module_opts,
                                    [<<"localhost">>, mod_offline]),
                      rpc(NodeName, gen_mod, start_module, [<<"localhost">>, mod_offline, OffOpts]);
                  _ ->
                      ok
              end
      end,
      get_hosts()),
    escalus:delete_users(Config, escalus:get_users([alice, eve])).

init_per_testcase(test_muc_conversation_on_one_host = CaseName, Config) ->
    muc_helper:load_muc(<<"muc.localhost">>),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(test_muc_conversation_on_one_host = CaseName, Config) ->
    muc_helper:unload_muc(),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------

test_pm_between_users_at_different_locations(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {eve, 1}],
      fun(Alice, Eve) ->
              escalus_client:send(Alice, escalus_stanza:chat_to(Eve, <<"Hi from Europe!">>)),
              escalus_client:send(Eve, escalus_stanza:chat_to(Alice, <<"Hi from Asia!">>)),

              FromAlice = escalus_client:wait_for_stanza(Eve),
              FromEve = escalus_client:wait_for_stanza(Alice),

              AliceJid = escalus_client:full_jid(Alice),
              EveJid = escalus_client:full_jid(Eve),

              escalus:assert(is_chat_message_from_to, [AliceJid, EveJid, <<"Hi from Europe!">>],
                             FromAlice),
              escalus:assert(is_chat_message_from_to, [EveJid, AliceJid, <<"Hi from Asia!">>],
                             FromEve),

              escalus_connection:stop(Eve)
      end).

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
              escalus:wait_for_stanzas(Eve, 3),

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

test_component_on_one_host(Config) ->
    ComponentConfig = [{server, <<"localhost">>}, {host, <<"localhost">>}, {password, <<"secret">>},
                       {port, 8888}, {component, <<"test_service">>}],

    {Comp, Addr, _Name} = component_SUITE:connect_component(ComponentConfig),

    Story = fun(User) ->
                    Msg1 = escalus_stanza:chat_to(Addr, <<"Hi!">>),
                    escalus:send(User, Msg1),
                    %% Then component receives it
                    Reply1 = escalus:wait_for_stanza(Comp),
                    escalus:assert(is_chat_message, [<<"Hi!">>], Reply1),

                    %% When components sends a reply
                    Msg2 = escalus_stanza:chat_to(User, <<"Oh hi!">>),
                    escalus:send(Comp, escalus_stanza:from(Msg2, Addr)),

                    %% Then Alice receives it
                    Reply2 = escalus:wait_for_stanza(User),
                    escalus:assert(is_chat_message, [<<"Oh hi!">>], Reply2),
                    escalus:assert(is_stanza_from, [Addr], Reply2)
            end,

    [escalus:story(Config, [{User, 1}], Story) || User <- [alice, eve]].

test_component_disconnect(Config) ->
    ComponentConfig = [{server, <<"localhost">>}, {host, <<"localhost">>}, {password, <<"secret">>},
                       {port, 8888}, {component, <<"test_service">>}],

    {Comp, Addr, _Name} = component_SUITE:connect_component(ComponentConfig),
    ok = escalus_connection:stop(Comp),

    Story = fun(User) ->
                    escalus:send(User, escalus_stanza:chat_to(Addr, <<"Hi!">>)),
                    Error = escalus:wait_for_stanza(User),
                    escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Error)
            end,

    [escalus:story(Config, [{User, 1}], Story) || User <- [alice, eve]].

test_location_disconnect(Config) ->
    try
        escalus:fresh_story(
          Config, [{alice, 1}, {eve, 1}],
          fun(Alice, Eve) ->
                  escalus_client:send(Alice, escalus_stanza:chat_to(Eve, <<"Hi from Europe!">>)),

                  escalus_client:wait_for_stanza(Eve),

                  ok = rpc(asia_node, application, stop, [ejabberd]),

                  escalus_client:send(Alice, escalus_stanza:chat_to(Eve, <<"Hi again!">>)),
                  Error = escalus:wait_for_stanza(Alice),
                  escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Error)
          end),

        ct:fail("the test was expected to fail but didn't")
    catch
        _:{assertion_failed, assert, is_error, _, _, _} ->
            {skip, "error translation not implemented"}
    after
        rpc(asia_node, application, start, [ejabberd])
    end.

test_pm_with_disconnection_on_other_server(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {eve, 1}],
      fun(Alice, Eve) ->
              escalus_client:send(Alice, escalus_stanza:chat_to(Eve, <<"Hi from Europe!">>)),
              escalus_connection:stop(Eve),
              FromAliceBounce = escalus_client:wait_for_stanza(Alice),
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

              escalus_client:send(Alice, chat_with_seqnum(Eve, <<"Hi from Europe!">>)),
              NewEve = connect_from_spec([{port, 5222} | EveSpec], Config),

              escalus_client:send(Alice, chat_with_seqnum(Eve, <<"Hi again from Europe!">>)),
              escalus_client:send(NewEve, escalus_stanza:chat_to(Alice, <<"Hi again from Asia!">>)),

              FromEve = escalus_client:wait_for_stanza(Alice),
              FirstFromAlice = escalus_client:wait_for_stanza(NewEve),
              AgainFromEve = escalus_client:wait_for_stanza(Alice),
              SecondFromAlice = escalus_client:wait_for_stanza(NewEve),

              [FromAlice, AgainFromAlice] = order_by_seqnum([FirstFromAlice, SecondFromAlice]),

              escalus:assert(is_chat_message, [<<"Hi from Europe!">>], FromAlice),
              escalus:assert(is_chat_message, [<<"Hi from Asia!">>], FromEve),
              escalus:assert(is_chat_message, [<<"Hi again from Europe!">>], AgainFromAlice),
              escalus:assert(is_chat_message, [<<"Hi again from Asia!">>], AgainFromEve)
      end).

test_pm_with_ungraceful_reconnection_to_different_server(Config0) ->
    Config = escalus_users:update_userspec(Config0, eve, stream_management, true),
    EveSpec = muc_SUITE:given_fresh_spec(Config, eve),
    escalus:create_users(Config, [{eve, [{port, 5222} | EveSpec]}]),
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              StepsWithSM = [start_stream, stream_features, maybe_use_ssl,
                             authenticate, bind, session, stream_resumption],

              {ok, Eve0, Eve0Props, _} = escalus_connection:start(EveSpec, StepsWithSM),
              Eve = Eve0#client{jid = sm_SUITE:get_bjid(Eve0Props)},
              escalus_story:send_initial_presence(Eve),
              escalus_client:wait_for_stanza(Eve),

              escalus_connection:kill(Eve),

              escalus_client:send(Alice, chat_with_seqnum(Eve, <<"Hi from Europe!">>)),

              NewEve = connect_from_spec([{port, 5222} | EveSpec], Config),

              escalus_client:send(Alice, chat_with_seqnum(Eve, <<"Hi again from Europe!">>)),
              escalus_client:send(NewEve, escalus_stanza:chat_to(Alice, <<"Hi from Asia!">>)),

              FirstFromAlice = escalus_client:wait_for_stanza(NewEve, timer:seconds(10)),
              SecondFromAlice = escalus_client:wait_for_stanza(NewEve),
              AgainFromEve = escalus_client:wait_for_stanza(Alice),

              [FromAlice, AgainFromAlice] = order_by_seqnum([FirstFromAlice, SecondFromAlice]),

              escalus:assert(is_chat_message, [<<"Hi from Europe!">>], FromAlice),
              escalus:assert(is_chat_message, [<<"Hi again from Europe!">>], AgainFromAlice),
              escalus:assert(is_chat_message, [<<"Hi from Asia!">>], AgainFromEve)
      end).

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

get_hosts() ->
    [{europe_node, "localhost.bis"}, {asia_node, "fed1"}].

rpc(NodeName, M, F, A) ->
    Node = ct:get_config(NodeName),
    ct_rpc:call(Node, M, F, A).

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
