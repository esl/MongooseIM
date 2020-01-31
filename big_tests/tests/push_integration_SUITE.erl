-module(push_integration_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("inbox.hrl").

-define(MUCLIGHTHOST, <<"muclight.localhost">>).
-define(RPC_SPEC, distributed_helper:mim()).
-define(SESSION_KEY, publish_service).

-import(muc_light_helper,
        [
         when_muc_light_message_is_sent/4,
         then_muc_light_message_is_received_by/2,
         when_muc_light_affiliations_are_set/3,
         then_muc_light_affiliations_are_received_by/2
        ]).
-import(push_helper,
        [
         enable_stanza/3,
         become_unavailable/1,
         become_available/2
        ]).
-import(distributed_helper, [rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, pubsub_ful},
     {group, pubsub_less}
    ].

basic_groups() ->
    [
     {group, pm_msg_notifications},
     {group, muclight_msg_notifications},
     {group, pm_notifications_with_inbox},
     {group, groupchat_notifications_with_inbox},
     {group, failure_cases_v3},
     {group, failure_cases_v2},
     {group, integration_with_sm_and_offline_storage},
     {group, enhanced_integration_with_sm}
    ].

groups() ->
    G = [
         {pubsub_ful, [], basic_groups()},
         {pubsub_less, [], basic_groups()},
         {integration_with_sm_and_offline_storage,[parallel],
          [
           no_duplicates_default_plugin,
           sm_unack_messages_notified_default_plugin
          ]},
         {enhanced_integration_with_sm,[],
          [
              immediate_notification,
              double_notification_with_two_sessions_in_resume
          ]},
         {pm_msg_notifications, [parallel],
          [
           pm_msg_notify_on_apns_w_high_priority,
           pm_msg_notify_on_fcm_w_high_priority,
           pm_msg_notify_on_apns_w_high_priority_silent,
           pm_msg_notify_on_fcm_w_high_priority_silent,
           pm_msg_notify_on_apns_no_click_action,
           pm_msg_notify_on_fcm_no_click_action,
           pm_msg_notify_on_apns_w_click_action,
           pm_msg_notify_on_fcm_w_click_action,
           pm_msg_notify_on_apns_silent,
           pm_msg_notify_on_fcm_silent,
           pm_msg_notify_on_apns_w_topic
          ]},
         {muclight_msg_notifications, [parallel],
          [
           muclight_msg_notify_on_apns_w_high_priority,
           muclight_msg_notify_on_fcm_w_high_priority,
           muclight_msg_notify_on_apns_w_high_priority_silent,
           muclight_msg_notify_on_fcm_w_high_priority_silent,
           muclight_msg_notify_on_apns_no_click_action,
           muclight_msg_notify_on_fcm_no_click_action,
           muclight_msg_notify_on_apns_w_click_action,
           muclight_msg_notify_on_fcm_w_click_action,
           muclight_msg_notify_on_apns_silent,
           muclight_msg_notify_on_fcm_silent,
           muclight_msg_notify_on_w_topic
          ]},
         {groupchat_notifications_with_inbox, [parallel],
          [
           muclight_inbox_msg_unread_count_apns,
           muclight_inbox_msg_unread_count_fcm,
           muclight_aff_change_fcm,
           muclight_aff_change_apns
          ]},
         {pm_notifications_with_inbox, [parallel],
          [
           inbox_msg_unread_count_apns,
           inbox_msg_unread_count_fcm,
           inbox_msg_reset_unread_count_apns,
           inbox_msg_reset_unread_count_fcm
          ]},
         {failure_cases_v3, [parallel], failure_cases()},
         {failure_cases_v2, [parallel], failure_cases()}
        ],
    G.

failure_cases() ->
    [
     no_push_notification_for_expired_device,
     no_push_notification_for_internal_mongoose_push_error
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    catch mongoose_push_mock:stop(),
    mongoose_push_mock:start(Config),
    Port = mongoose_push_mock:port(),

    PoolOpts = [{strategy, available_worker}, {workers, 20}],
    HTTPOpts = [{server, "https://localhost:" ++ integer_to_list(Port)}],
    rpc(?RPC_SPEC, mongoose_wpool, start_configured_pools,
        [[{http, global, mongoose_push_http, PoolOpts, HTTPOpts}]]),
    ConfigWithModules = dynamic_modules:save_modules(domain(), Config),
    escalus:init_per_suite(ConfigWithModules).


end_per_suite(Config) ->
    escalus_fresh:clean(),
    rpc(?RPC_SPEC, mongoose_wpool, stop, [http, global, mongoose_push_http]),
    mongoose_push_mock:stop(),
    escalus:end_per_suite(Config).

init_per_group(pubsub_less, Config) ->
    [{pubsub_host, virtual} | Config];
init_per_group(pubsub_ful, Config) ->
    [{pubsub_host, real} | Config];
init_per_group(G, Config) when G =:= pm_notifications_with_inbox;
                               G =:= groupchat_notifications_with_inbox;
                               G =:= integration_with_sm_and_offline_storage ->
    case mongoose_helper:is_rdbms_enabled(domain()) of
        true ->
            init_modules(G, Config);
        _ ->
            {skip, require_rdbms}
    end;
init_per_group(G, Config) ->
    %% Some cleaning up
    C = init_modules(G, Config),
    catch rpc(?RPC_SPEC, mod_muc_light_db_backend, force_clear, []),
    C.

end_per_group(_, Config) ->
    dynamic_modules:restore_modules(domain(), Config),
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%------------------------------------------------------------------------------------
%% GROUP integration_with_sm_and_offline_storage & enhanced_integration_with_sm
%%------------------------------------------------------------------------------------
no_duplicates_default_plugin(Config) ->
    ConnSteps = [start_stream, stream_features, maybe_use_ssl,
                 authenticate, bind, session],

    %% connect bob and alice
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    BobJID = bare_jid(Bob),

    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),

    #{device_token := APNSDevice} = enable_push_for_user(Alice, <<"apns">>, [], Config),

    escalus_connection:stop(Alice),
    push_helper:wait_for_user_offline(Alice),

    escalus_connection:send(Bob, escalus_stanza:chat_to(bare_jid(Alice), <<"msg-1">>)),
    mongoose_helper:wait_until(fun() -> get_number_of_offline_msgs(AliceSpec) end, 1),
    verify_notification(APNSDevice, <<"apns">>, [], BobJID, <<"msg-1">>),
    {ok, NewAlice, _} = escalus_connection:start([{manual_ack, true} | AliceSpec],
                                                 ConnSteps ++ [stream_management]),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),
    Stanzas = escalus:wait_for_stanzas(NewAlice, 2),
    escalus:assert_many([is_presence, is_chat_message], Stanzas),

    escalus_connection:stop(NewAlice),
    mongoose_helper:wait_until(fun() -> get_number_of_offline_msgs(AliceSpec) end, 1),

    ?assertExit({test_case_failed, _}, wait_for_push_request(APNSDevice, 500)),

    escalus_connection:stop(Bob).

get_number_of_offline_msgs(Spec) ->
    Username = escalus_utils:jid_to_lower(proplists:get_value(username, Spec)),
    Server = proplists:get_value(server, Spec),
    mongoose_helper:total_offline_messages({Username, Server}).

sm_unack_messages_notified_default_plugin(Config) ->
    ConnSteps = [start_stream, stream_features, maybe_use_ssl,
                 authenticate, bind, session, stream_management],

    %% connect bob and alice
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),
    BobJID = bare_jid(Bob),

    AliceSpec = [{manual_ack, false}, {stream_management, true} |
                 escalus_fresh:create_fresh_user(Config, alice)],
    {ok, Alice, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),

    Room = fresh_room_name(),
    RoomJID = muc_light_helper:given_muc_light_room(Room, Alice, [{Bob, member}]),

    #{device_token := FCMDevice} = enable_push_for_user(Alice, <<"fcm">>, [], Config),

    escalus_connection:send(Bob, escalus_stanza:chat_to(bare_jid(Alice), <<"msg-0">>)),
    escalus:assert(is_chat_message, [<<"msg-0">>], escalus_connection:get_stanza(Alice, msg)),

    H = escalus_connection:get_sm_h(Alice),
    escalus:send(Alice, escalus_stanza:sm_ack(H)),

    escalus_connection:send(Bob, escalus_stanza:chat_to(bare_jid(Alice), <<"msg-1">>)),
    escalus:assert(is_chat_message, [<<"msg-1">>], escalus_connection:get_stanza(Alice, msg)),
    SenderJID = muclight_conversation(Bob, RoomJID, <<"msg-2">>),
    escalus:assert(is_groupchat_message, [<<"msg-2">>], escalus_connection:get_stanza(Alice, msg)),

    escalus_connection:stop(Alice),
    push_helper:wait_for_user_offline(Alice),

    verify_notification(FCMDevice, <<"fcm">>, [], [{SenderJID, <<"msg-2">>},
                                                   {BobJID, <<"msg-1">>}]),

    ?assertExit({test_case_failed, _}, wait_for_push_request(FCMDevice, 500)),

    escalus_connection:stop(Bob).

immediate_notification(Config) ->
    ConnSteps = [start_stream, stream_features, maybe_use_ssl,
                 authenticate, bind, session, stream_resumption],

    %% connect bob and alice
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),
    BobJID = bare_jid(Bob),

    AliceSpec = [{manual_ack, false}, {stream_management, true} |
                 escalus_fresh:create_fresh_user(Config, alice)],
    {ok, Alice, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),

    #{device_token := APNSDevice} = enable_push_for_user(Alice, <<"apns">>, [], Config),
    #{device_token := FCMDevice} = enable_push_for_user(Alice, <<"fcm">>, [], Config),

    escalus_connection:send(Bob, escalus_stanza:chat_to(bare_jid(Alice), <<"msg-0">>)),
    escalus:assert(is_chat_message, [<<"msg-0">>], escalus_connection:get_stanza(Alice, msg)),

    H = escalus_connection:get_sm_h(Alice),
    escalus:send(Alice, escalus_stanza:sm_ack(H)),

    escalus_connection:send(Bob, escalus_stanza:chat_to(bare_jid(Alice), <<"msg-1">>)),
    escalus:assert(is_chat_message, [<<"msg-1">>], escalus_connection:get_stanza(Alice, msg)),

    C2SPid = mongoose_helper:get_session_pid(Alice, distributed_helper:mim()),
    escalus_connection:kill(Alice),

    verify_notification(FCMDevice, <<"fcm">>, [], BobJID, <<"msg-1">>),

    escalus_connection:send(Bob, escalus_stanza:chat_to(bare_jid(Alice), <<"msg-2">>)),
    verify_notification(FCMDevice, <<"fcm">>, [], BobJID, <<"msg-2">>),

    ?assertExit({test_case_failed, _}, wait_for_push_request(APNSDevice, 500)),

    rpc(?RPC_SPEC, sys, terminate, [C2SPid, normal]),

    verify_notification(APNSDevice, <<"apns">>, [], [{BobJID, <<"msg-1">>},
                                                     {BobJID, <<"msg-2">>}]),

    ?assertExit({test_case_failed, _}, wait_for_push_request(FCMDevice, 500)),

    escalus_connection:stop(Bob).

double_notification_with_two_sessions_in_resume(Config) ->

%%    This test case serves as a demonstration of doubled push notifications
%%    which occur with multiple push devices and sessions in resume state

%%    diagram presenting the test's logic
%%    generated using https://www.sequencediagram.org/
%%           title double notifications in 2 sessions in resume
%%           BobSocket -> BobC2S: msg-1
%%           participant Alice1C2S #red
%%           participant Alice2C2S #blue
%%           BobC2S -#red> Alice1C2S: msg-1
%%           BobC2S -#blue> Alice2C2S: msg-1
%%           note over Alice1C2S: connection dies
%%           activate Alice1C2S
%%           note over Alice2C2S: connection dies
%%           activate Alice2C2S
%%           Alice1C2S -#red> PushService: msg-1 (alice1 device)
%%           deactivate Alice1C2S
%%           Alice2C2S -#blue> PushService: msg-1 (alice2 device)
%%           deactivate Alice2C2S
%%           note over Alice1C2S: resumption t/o
%%           activate Alice1C2S
%%           Alice1C2S -#red> Alice2C2S: msg-1
%%           activate Alice2C2S #red
%%           deactivate Alice1C2S
%%           destroyafter Alice1C2S
%%           Alice2C2S -#red> PushService: msg-1 (alice2 device)
%%           deactivate Alice2C2S
%%           note over Alice2C2S: resumption t/o
%%           activate Alice2C2S
%%           Alice2C2S -#blue> PushService: msg-1 (alice1 device)
%%           deactivate Alice2C2S
%%           destroyafter Alice2C2S

    ConnSteps = [start_stream, stream_features, maybe_use_ssl,
                 authenticate, bind, session, stream_resumption],

    %% connect bob
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    escalus_session:send_presence_available(Bob),
    escalus_connection:get_stanza(Bob, presence),
    BobJID = bare_jid(Bob),

    %% connect two resources for alice
    AliceSpec1 = [{manual_ack, false}, {stream_resumption, true} |
                  escalus_fresh:create_fresh_user(Config, alice)],
    AliceSpec2 = [{resource,<<"RES2">>} | AliceSpec1],

    {ok, Alice1, _} = escalus_connection:start(AliceSpec1, ConnSteps),
    {ok, Alice2, _} = escalus_connection:start(AliceSpec2, ConnSteps),

    escalus_session:send_presence_available(Alice1),
    escalus_connection:get_stanza(Alice1, presence),

    escalus_session:send_presence_available(Alice2),
    escalus_connection:get_stanza(Alice2, presence),

    escalus_connection:get_stanza(Alice1, presence),
    escalus_connection:get_stanza(Alice2, presence),

    #{device_token := APNSDevice1} = enable_push_for_user(Alice1, <<"apns">>, [], Config),
    #{device_token := APNSDevice2} = enable_push_for_user(Alice2, <<"apns">>, [], Config),

    escalus_connection:send(Bob, escalus_stanza:chat_to(bare_jid(Alice1), <<"msg-1">>)),
    escalus:assert(is_chat_message, [<<"msg-1">>], escalus_connection:get_stanza(Alice1, msg)),
    escalus:assert(is_chat_message, [<<"msg-1">>], escalus_connection:get_stanza(Alice2, msg)),

    %% go into resume state, which should fire a hook which pushes notifications
    C2SPid1 = mongoose_helper:get_session_pid(Alice1, distributed_helper:mim()),
    escalus_connection:kill(Alice1),
    C2SPid2 = mongoose_helper:get_session_pid(Alice2, distributed_helper:mim()),
    escalus_connection:kill(Alice2),

    verify_notification(APNSDevice1, <<"apns">>, [], [{BobJID, <<"msg-1">>}]),
    verify_notification(APNSDevice2, <<"apns">>, [], [{BobJID, <<"msg-1">>}]),

    ?assertExit({test_case_failed, _}, wait_for_push_request(APNSDevice1, 500)),
    ?assertExit({test_case_failed, _}, wait_for_push_request(APNSDevice2, 1)),

    %% close xmpp stream for Alice1, which causes push notification for APNSDevice2
    rpc(?RPC_SPEC, sys, terminate, [C2SPid1, normal]),

    verify_notification(APNSDevice2, <<"apns">>, [], [{BobJID, <<"msg-1">>}]),
    ?assertExit({test_case_failed, _}, wait_for_push_request(APNSDevice1, 500)),

    %% close xmpp stream for Alice2, which causes push notification for APNSDevice1
    rpc(?RPC_SPEC, sys, terminate, [C2SPid2, normal]),

    verify_notification(APNSDevice1, <<"apns">>, [], [{BobJID, <<"msg-1">>}]),
    ?assertExit({test_case_failed, _}, wait_for_push_request(APNSDevice2, 500)),

    escalus_connection:stop(Bob).

verify_notification(DeviceToken, Service, EnableOpts, Jid, Msg) ->
    verify_notification(DeviceToken, Service, EnableOpts, [{Jid, Msg}]).

verify_notification(DeviceToken, Service, EnableOpts, ParamsList) ->
    PredGen = fun({Jid, Msg}) ->
                  fun(Notification) ->
                      try
                          Expected = [{body, Msg}, {unread_count, 1}, {badge, 1}],
                          assert_push_notification(Notification, Service, EnableOpts,
                                                   Jid, Expected),
                          true
                      catch
                          _:_ -> false
                      end
                  end
              end,
    Notifications = [begin
                         {Notification, _} = wait_for_push_request(DeviceToken),
                         Notification
                     end || _ <- ParamsList],
    ?assertEqual(true, escalus_utils:mix_match(PredGen, ParamsList, Notifications)).

%%--------------------------------------------------------------------
%% GROUP pm_msg_notifications
%%--------------------------------------------------------------------

pm_msg_notify_on_apns(Config, EnableOpts) ->
    escalus:fresh_story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            {SenderJID, DeviceToken} = pm_conversation(Alice, Bob, <<"apns">>, EnableOpts, Config),
            {Notification, _} = wait_for_push_request(DeviceToken),

            assert_push_notification(Notification, <<"apns">>, EnableOpts, SenderJID, [])

        end).

pm_msg_notify_on_fcm(Config, EnableOpts) ->
    escalus:fresh_story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            {SenderJID, DeviceToken} = pm_conversation(Alice, Bob, <<"fcm">>, EnableOpts, Config),
            {Notification, _} = wait_for_push_request(DeviceToken),

            assert_push_notification(Notification, <<"fcm">>, EnableOpts, SenderJID)

        end).

assert_push_notification(Notification, Service, EnableOpts, SenderJID) ->
    assert_push_notification(Notification, Service, EnableOpts, SenderJID, []).

assert_push_notification(Notification, Service, EnableOpts, SenderJID, Expected) ->

    ?assertMatch(#{<<"service">> := Service}, Notification),

    Alert = maps:get(<<"alert">>, Notification, undefined),
    Data = maps:get(<<"data">>, Notification, undefined),

    ExpectedBody = proplists:get_value(body, Expected, <<"OH, HAI!">>),
    UnreadCount = proplists:get_value(unread_count, Expected, 1),
    Badge = proplists:get_value(badge, Expected, 1),

    case proplists:get_value(<<"silent">>, EnableOpts) of
        undefined ->
            ?assertMatch(#{<<"body">> := ExpectedBody}, Alert),
            ?assertMatch(#{<<"title">> := SenderJID}, Alert),
            ?assertMatch(#{<<"badge">> := Badge}, Alert),
            ?assertMatch(#{<<"tag">> := SenderJID}, Alert),

            case proplists:get_value(<<"click_action">>, EnableOpts) of
                undefined ->
                    ?assertMatch(#{<<"click_action">> := null}, Alert);
                Activity ->
                    ?assertMatch(#{<<"click_action">> := Activity}, Alert)
            end;
        <<"true">> ->
            ?assertMatch(#{<<"last-message-body">> := ExpectedBody}, Data),
            ?assertMatch(#{<<"last-message-sender">> := SenderJID}, Data),
            ?assertMatch(#{<<"message-count">> := UnreadCount}, Data)
    end,

    case proplists:get_value(<<"priority">>, EnableOpts) of
        undefined -> ok;
        Priority ->
            ?assertMatch(Priority, maps:get(<<"priority">>, Notification, undefined))
    end,

    case proplists:get_value(<<"topic">>, EnableOpts) of
        undefined -> ok;
        Topic ->
            ?assertMatch(Topic, maps:get(<<"topic">>, Notification, undefined))
    end.


pm_msg_notify_on_apns_no_click_action(Config) ->
    pm_msg_notify_on_apns(Config, []).

pm_msg_notify_on_fcm_no_click_action(Config) ->
    pm_msg_notify_on_fcm(Config, []).

pm_msg_notify_on_apns_w_high_priority(Config) ->
    pm_msg_notify_on_apns(Config, [{<<"priority">>, <<"high">>}]).

pm_msg_notify_on_fcm_w_high_priority(Config) ->
    pm_msg_notify_on_fcm(Config, [{<<"priority">>, <<"high">>}]).

pm_msg_notify_on_apns_w_high_priority_silent(Config) ->
    pm_msg_notify_on_apns(Config, [{<<"silent">>, <<"true">>}, {<<"priority">>, <<"high">>}]).

pm_msg_notify_on_fcm_w_high_priority_silent(Config) ->
    pm_msg_notify_on_fcm(Config, [{<<"silent">>, <<"true">>}, {<<"priority">>, <<"high">>}]).

pm_msg_notify_on_apns_w_click_action(Config) ->
    pm_msg_notify_on_apns(Config, [{<<"click_action">>, <<"myactivity">>}]).

pm_msg_notify_on_fcm_w_click_action(Config) ->
    pm_msg_notify_on_fcm(Config, [{<<"click_action">>, <<"myactivity">>}]).

pm_msg_notify_on_fcm_silent(Config) ->
    pm_msg_notify_on_fcm(Config, [{<<"silent">>, <<"true">>}]).

pm_msg_notify_on_apns_silent(Config) ->
    pm_msg_notify_on_apns(Config, [{<<"silent">>, <<"true">>}]).

pm_msg_notify_on_apns_w_topic(Config) ->
    pm_msg_notify_on_apns(Config, [{<<"topic">>, <<"some_topic">>}]).


%%--------------------------------------------------------------------
%% GROUP inbox_msg_notifications
%%--------------------------------------------------------------------

inbox_msg_unread_count_apns(Config) ->
    inbox_msg_unread_count(Config, <<"apns">>, [{<<"silent">>, <<"true">>}]).

inbox_msg_unread_count_fcm(Config) ->
    inbox_msg_unread_count(Config, <<"fcm">>, [{<<"silent">>, <<"true">>}]).

muclight_inbox_msg_unread_count_apns(Config) ->
    muclight_inbox_msg_unread_count(Config, <<"apns">>, [{<<"silent">>, <<"true">>}]).

muclight_inbox_msg_unread_count_fcm(Config) ->
    muclight_inbox_msg_unread_count(Config, <<"fcm">>, [{<<"silent">>, <<"true">>}]).

inbox_msg_reset_unread_count_apns(Config) ->
    inbox_msg_reset_unread_count(Config, <<"apns">>, [{<<"silent">>, <<"true">>}]).

inbox_msg_reset_unread_count_fcm(Config) ->
    inbox_msg_reset_unread_count(Config, <<"fcm">>, [{<<"silent">>, <<"true">>}]).

inbox_msg_unread_count(Config, Service, EnableOpts) ->
    escalus:fresh_story(
      Config, [{bob, 1}, {alice, 1}, {kate, 1}],
      fun(Bob, Alice, Kate) ->
              % In this test Bob is the only recipient of all messages
              #{device_token := DeviceToken} =
                    enable_push_and_become_unavailable(Bob, Service, EnableOpts, Config),

              % We're going to interleave messages from Alice and Kate to ensure
              % that their unread counts don't leak to each other's notifications

              % We send a first message from Alice, unread counts in convs.: Alice 1, Kate 0
              send_private_message(Alice, Bob),
              check_notification(DeviceToken, 1),

              % We send a first message from Kate, unread counts in convs.: Alice 1, Kate 1
              send_private_message(Kate, Bob),
              check_notification(DeviceToken, 1),

              % Now a second message from Alice, unread counts in convs.: Alice 2, Kate 1
              send_private_message(Alice, Bob),
              check_notification(DeviceToken, 2),

              % And one more from Alice, unread counts in convs.: Alice 3, Kate 1
              send_private_message(Alice, Bob),
              check_notification(DeviceToken, 3),

              % Time for Kate again, unread counts in convs.: Alice 3, Kate 2
              send_private_message(Kate, Bob),
              check_notification(DeviceToken, 2)
      end).

inbox_msg_reset_unread_count(Config, Service, EnableOpts) ->
    escalus:fresh_story(
      Config, [{bob, 1}, {alice, 1}],
      fun(Bob, Alice) ->
              #{device_token := DeviceToken} =
                    enable_push_and_become_unavailable(Bob, Service, EnableOpts, Config),
              send_private_message(Alice, Bob, <<"FIRST MESSAGE">>),
              check_notification(DeviceToken, 1),
              MsgId = send_private_message(Alice, Bob, <<"SECOND MESSAGE">>),
              check_notification(DeviceToken, 2),

              become_available(Bob, 2),
              inbox_helper:get_inbox(Bob, #{ count => 1 }),
              ChatMarker = escalus_stanza:chat_marker(Alice, <<"displayed">>, MsgId),
              escalus:send(Bob, ChatMarker),
              escalus:wait_for_stanza(Alice),

              become_unavailable(Bob),
              send_private_message(Alice, Bob, <<"THIRD MESSAGE">>),
              check_notification(DeviceToken, 1)
      end).


muclight_inbox_msg_unread_count(Config, Service, EnableOpts) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {kate, 1}],
      fun(Alice, Kate) ->
              Room = fresh_room_name(),
              RoomJID = muc_light_helper:given_muc_light_room(Room, Alice, []),
              KateJid = inbox_helper:to_bare_lower(Kate),

              when_muc_light_affiliations_are_set(Alice, Room, [{Kate, member}]),
              muc_light_helper:verify_aff_bcast([{Kate, member}, {Alice, owner}], [{Kate, member}]),
              escalus:wait_for_stanza(Alice),

              #{device_token := KateToken} =
                    enable_push_and_become_unavailable(Kate, Service, EnableOpts, Config),

              SenderJID = muclight_conversation(Alice, RoomJID, <<"First!">>),
              escalus:wait_for_stanza(Alice),
              {Notification, _} = wait_for_push_request(KateToken),
              assert_push_notification(Notification, Service, EnableOpts, SenderJID,
                                       [{body, <<"First!">>}, {unread_count, 1}, {badge, 1}]),

              muclight_conversation(Alice, RoomJID, <<"Second!">>),
              escalus:wait_for_stanza(Alice),
              {Notification2, _} = wait_for_push_request(KateToken),
              assert_push_notification(Notification2, Service, EnableOpts, SenderJID,
                                       [{body, <<"Second!">>}, {unread_count, 2}, {badge, 1}]),

              {ok, true} = become_available(Kate, 0),

              muclight_conversation(Alice, RoomJID, <<"Third!">>),
              escalus:wait_for_stanza(Kate),
              escalus:wait_for_stanza(Alice),
              inbox_helper:check_inbox(Kate, [#conv{unread = 3,
                                                    from = SenderJID,
                                                    to = KateJid,
                                                    content = <<"Third!">>}])
      end).

send_private_message(Sender, Recipient) ->
    send_private_message(Sender, Recipient, <<"Private message">>).

send_private_message(Sender, Recipient, Body) ->
    Id = escalus_stanza:id(),
    Msg = escalus_stanza:set_id( escalus_stanza:chat_to(bare_jid(Recipient), Body), Id),
    escalus:send(Sender, Msg),
    Id.

check_notification(DeviceToken, ExpectedCount) ->
    {Notification, _} = wait_for_push_request(DeviceToken),
    Data = maps:get(<<"data">>, Notification, undefined),
    ?assertMatch(#{<<"message-count">> := ExpectedCount}, Data).

send_message_to_room(Sender, RoomJID) ->
    Stanza = escalus_stanza:groupchat_to(RoomJID, <<"GroupChat message">>),
    escalus:send(Sender, Stanza).


%%--------------------------------------------------------------------
%% GROUP muclight_msg_notifications
%%--------------------------------------------------------------------

muclight_msg_notify_on_apns(Config, EnableOpts) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            Room = fresh_room_name(),
            RoomJID = muc_light_helper:given_muc_light_room(Room, Alice, [{Bob, member}]),
            #{device_token := DeviceToken} =
                enable_push_and_become_unavailable(Bob, <<"apns">>, EnableOpts, Config),

            SenderJID = muclight_conversation(Alice, RoomJID, <<"Heyah!">>),
            {Notification, _} = wait_for_push_request(DeviceToken),
            assert_push_notification(Notification, <<"apns">>, EnableOpts, SenderJID,
                                     [{body, <<"Heyah!">>}, {unread_count, 1}, {badge, 1}])
        end).

muclight_msg_notify_on_fcm(Config, EnableOpts) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            Room = fresh_room_name(),
            RoomJID = muc_light_helper:given_muc_light_room(Room, Alice, [{Bob, member}]),
            #{device_token := DeviceToken} =
                enable_push_and_become_unavailable(Bob, <<"fcm">>, EnableOpts, Config),

            SenderJID = muclight_conversation(Alice, RoomJID, <<"Heyah!">>),
            {Notification, _} = wait_for_push_request(DeviceToken),
            assert_push_notification(Notification, <<"fcm">>,
                                     EnableOpts, SenderJID, [{body, <<"Heyah!">>}, {unread_count, 1}, {badge, 1}])
        end).

muclight_aff_change(Config, Service, EnableOpts) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {kate, 1}, {bob, 1}],
      fun(Alice, Kate, Bob) ->
              Room = fresh_room_name(),
              RoomJID = muc_light_helper:given_muc_light_room(Room, Alice, []),

              {_, Affiliations} = when_muc_light_affiliations_are_set(Alice, Room, [{Kate, member}]),
              then_muc_light_affiliations_are_received_by([Alice, Kate], {Room, Affiliations}),
              escalus:wait_for_stanza(Alice),

              #{device_token := KateToken} =
                    enable_push_and_become_unavailable(Kate, Service, EnableOpts, Config),

              Bare = bare_jid(Alice),
              SenderJID = <<RoomJID/binary, "/", Bare/binary>>,

              {Room, Body, M1} = when_muc_light_message_is_sent(Alice, Room, <<"First!">>, <<"M1">>),
              then_muc_light_message_is_received_by([Alice], {Room, Body, M1}),

              {Notification, _} = wait_for_push_request(KateToken),
              assert_push_notification(Notification, Service, EnableOpts, SenderJID,
                                       [{body, <<"First!">>}, {unread_count, 1}, {badge, 1}]),

              {_, Aff} = when_muc_light_affiliations_are_set(Alice, Room, [{Bob, member}]),
              then_muc_light_affiliations_are_received_by([Alice, Bob], {Room, Aff}),
              escalus:wait_for_stanza(Alice),

              {_, B2, M2} = when_muc_light_message_is_sent(Alice, Room, <<"Second!">>, <<"M2">>),
              then_muc_light_message_is_received_by([Alice, Bob], {Room, B2, M2}),

              {Notification2, _} = wait_for_push_request(KateToken),
              assert_push_notification(Notification2, Service, EnableOpts, SenderJID,
                                       [{body, <<"Second!">>}, {unread_count, 2}, {badge, 1}])

      end).


muclight_msg_notify_on_apns_no_click_action(Config) ->
    muclight_msg_notify_on_apns(Config, []).

muclight_msg_notify_on_fcm_no_click_action(Config) ->
    muclight_msg_notify_on_fcm(Config, []).

muclight_msg_notify_on_apns_w_high_priority(Config) ->
    muclight_msg_notify_on_apns(Config, [{<<"priority">>, <<"high">>}]).

muclight_msg_notify_on_fcm_w_high_priority(Config) ->
    muclight_msg_notify_on_fcm(Config, [{<<"priority">>, <<"high">>}]).

muclight_msg_notify_on_apns_w_high_priority_silent(Config) ->
    muclight_msg_notify_on_apns(Config, [{<<"silent">>, <<"true">>}, {<<"priority">>, <<"high">>}]).

muclight_msg_notify_on_fcm_w_high_priority_silent(Config) ->
    muclight_msg_notify_on_fcm(Config, [{<<"silent">>, <<"true">>}, {<<"priority">>, <<"high">>}]).

muclight_msg_notify_on_apns_w_click_action(Config) ->
    muclight_msg_notify_on_apns(Config, [{<<"click_action">>, <<"myactivity">>}]).

muclight_msg_notify_on_fcm_w_click_action(Config) ->
    muclight_msg_notify_on_fcm(Config, [{<<"click_action">>, <<"myactivity">>}]).

muclight_msg_notify_on_fcm_silent(Config) ->
    muclight_msg_notify_on_fcm(Config, [{<<"silent">>, <<"true">>}]).

muclight_msg_notify_on_apns_silent(Config) ->
    muclight_msg_notify_on_apns(Config, [{<<"silent">>, <<"true">>}]).

muclight_msg_notify_on_w_topic(Config) ->
    muclight_msg_notify_on_apns(Config, [{<<"topic">>, <<"some_topic">>}]).

muclight_aff_change_fcm(Config) ->
    muclight_aff_change(Config, <<"fcm">>, [{<<"silent">>, <<"true">>}]).

muclight_aff_change_apns(Config) ->
    muclight_aff_change(Config, <<"apns">>, [{<<"silent">>, <<"true">>}]).

no_push_notification_for_expired_device(Config) ->
    escalus:fresh_story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            Response = mongoose_push_unregistered_device_resp(Config),
            #{device_token := DeviceToken, pubsub_node := PushNode} =
                    enable_push_and_become_unavailable(Bob, <<"fcm">>, [], Response, Config),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
            {_, Response} = wait_for_push_request(DeviceToken),
            maybe_check_if_push_node_was_disabled(?config(api_v, Config), Bob, PushNode)

        end).

mongoose_push_unregistered_device_resp(Config) ->
    case ?config(api_v, Config) of
        "v3" ->
            {410, jiffy:encode(#{<<"reason">> => <<"unregistered">>})};
        "v2" ->
            {500, jiffy:encode(#{<<"details">> => <<"probably_unregistered">>})}
    end.

maybe_check_if_push_node_was_disabled("v2", _, _) ->
    ok;
maybe_check_if_push_node_was_disabled("v3", User, PushNode) ->
    JID = rpc(?RPC_SPEC, jid, binary_to_bare, [escalus_utils:get_jid(User)]),
    Fun = fun() ->
                  {ok, Services} = rpc(?RPC_SPEC, mod_event_pusher_push_backend, get_publish_services, [JID]),
                  lists:keymember(PushNode, 2, Services)
          end,
    mongoose_helper:wait_until(Fun, false),

    Fun2 = fun() ->
                   Info = mongoose_helper:get_session_info(?RPC_SPEC, User),
                   lists:keyfind(?SESSION_KEY, 1, Info)
           end,
    mongoose_helper:wait_until(Fun2, false).

no_push_notification_for_internal_mongoose_push_error(Config) ->
    escalus:fresh_story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            Response = {503, jiffy:encode(#{<<"reason">> => <<"unspecified">>})},
            #{device_token := DeviceToken} =
                enable_push_and_become_unavailable(Bob, <<"fcm">>, [], Response, Config),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
            {_, Response} = wait_for_push_request(DeviceToken)

        end).


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

muclight_conversation(Sender, RoomJID, Msg) ->
    Bare = bare_jid(Sender),
    SenderJID = <<RoomJID/binary, "/", Bare/binary>>,
    Stanza = escalus_stanza:groupchat_to(RoomJID, Msg),
    escalus:send(Sender, Stanza),
    SenderJID.

pm_conversation(Alice, Bob, Service, EnableOpts, Config) ->
    AliceJID = bare_jid(Alice),
    #{device_token := DeviceToken} =
        enable_push_and_become_unavailable(Bob, Service, EnableOpts, Config),
    escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
    {AliceJID, DeviceToken}.

enable_push_and_become_unavailable(User, Service, EnableOpts, Config) ->
    enable_push_and_become_unavailable(User, Service, EnableOpts, {200, <<"OK">>}, Config).

enable_push_and_become_unavailable(User, Service, EnableOpts, MockResponse, Config) ->
    Ret = enable_push_for_user(User, Service, EnableOpts, MockResponse, Config),
    become_unavailable(User),
    Ret.

enable_push_for_user(User, Service, EnableOpts, Config) ->
    enable_push_for_user(User, Service, EnableOpts, {200, <<"OK">>}, Config).

enable_push_for_user(User, Service, EnableOpts, MockResponse, Config) ->
    Node = {PubsubJID, NodeName} = pubsub_node_from_host(Config),

    DeviceToken = gen_token(),

    case ?config(pubsub_host, Config) of
        real ->
            Configuration = [{<<"pubsub#access_model">>, <<"whitelist">>},
                             {<<"pubsub#publish_model">>, <<"publishers">>}],
            pubsub_tools:create_node(User, Node, [{type, <<"push">>},
                                                  {config, Configuration}]),
            add_user_server_to_whitelist(User, Node);
        _ ->
            skip
    end,

    escalus:send(User, enable_stanza(PubsubJID, NodeName,
                                     [{<<"service">>, Service},
                                      {<<"device_id">>, DeviceToken}] ++ EnableOpts)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(User)),

    assert_push_notification_in_session(User, NodeName, Service, DeviceToken),

    mongoose_push_mock:subscribe(DeviceToken, MockResponse),
    #{device_token => DeviceToken,
      pubsub_node => NodeName}.

add_user_server_to_whitelist(User, {NodeAddr, NodeName}) ->
    AffList = [ #xmlel{ name = <<"affiliation">>,
                        attrs = [{<<"jid">>, escalus_utils:get_server(User)},
                                 {<<"affiliation">>, <<"publish-only">>}] }
              ],
    Affiliations = #xmlel{ name = <<"affiliations">>, attrs = [{<<"node">>, NodeName}],
                           children = AffList },
    Id = base64:encode(crypto:strong_rand_bytes(5)),
    Stanza = escalus_pubsub_stanza:pubsub_owner_iq(<<"set">>, User, Id, NodeAddr, [Affiliations]),
    escalus:send(User, Stanza),
    escalus:assert(is_iq_result, [Stanza], escalus:wait_for_stanza(User)).

assert_push_notification_in_session(User, NodeName, Service, DeviceToken) ->
    Info = mongoose_helper:get_session_info(?RPC_SPEC, User),
    {?SESSION_KEY, {_JID, NodeName, Details}} = lists:keyfind(?SESSION_KEY, 1, Info),
    ?assertMatch({<<"service">>, Service}, lists:keyfind(<<"service">>, 1, Details)),
    ?assertMatch({<<"device_id">>, DeviceToken}, lists:keyfind(<<"device_id">>, 1, Details)).

wait_for_push_request(DeviceToken) ->
    mongoose_push_mock:wait_for_push_request(DeviceToken, 10000).

wait_for_push_request(DeviceToken, Timeout) ->
    mongoose_push_mock:wait_for_push_request(DeviceToken, Timeout).

%% ----------------------------------
%% Other helpers
%% ----------------------------------

fresh_room_name(Username) ->
    escalus_utils:jid_to_lower(<<"room-", Username/binary>>).

fresh_room_name() ->
    fresh_room_name(base16:encode(crypto:strong_rand_bytes(5))).


bare_jid(JIDOrClient) ->
    ShortJID = escalus_client:short_jid(JIDOrClient),
    escalus_utils:jid_to_lower(ShortJID).

gen_token() ->
    integer_to_binary(binary:decode_unsigned(crypto:strong_rand_bytes(16)), 24).

lower(Bin) when is_binary(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).

domain() ->
    ct:get_config({hosts, mim, domain}).

pubsub_node_from_host(Config) ->
    case ?config(pubsub_host, Config) of
        virtual ->
            pubsub_tools:pubsub_node_with_subdomain("virtual.");
        real ->
            pubsub_tools:pubsub_node()
    end.

getenv(VarName, Default) ->
    case os:getenv(VarName) of
        false ->
            Default;
        Value ->
            Value
    end.

h2_req(Conn, Method, Path) ->
    h2_req(Conn, Method, Path, <<>>).
h2_req(Conn, Method, Path, Body) ->
    BinMethod = list_to_binary(string:to_upper(atom_to_list(Method))),
    Headers = [
        {<<":method">>, BinMethod},
        {<<":path">>, Path},
        {<<":authority">>, <<"localhost">>},
        {<<":scheme">>, <<"https">>}
    ],
    case h2_client:sync_request(Conn, Headers, Body) of
        {ok, {RespHeaders, RespBody}} ->
            Status = proplists:get_value(<<":status">>, RespHeaders),
            {ok, binary_to_integer(Status), iolist_to_binary(RespBody)};
        {error, Reason} ->
            {error, Reason}
    end.

init_modules(G, Config) ->
    MongoosePushAPI = mongoose_push_api_for_group(G),
    PubSubHost = ?config(pubsub_host, Config),
    Modules = required_modules_for_group(G, MongoosePushAPI, PubSubHost),
    C = dynamic_modules:save_modules(domain(), Config),
    Fun = fun() -> catch dynamic_modules:ensure_modules(domain(), Modules) end,
    mongoose_helper:wait_until(Fun, ok),
    [{api_v, MongoosePushAPI} | C].

mongoose_push_api_for_group(failure_cases_v2) ->
    "v2";
mongoose_push_api_for_group(_) ->
    "v3".

required_modules_for_group(pm_notifications_with_inbox, API, PubSubHost) ->
    [{mod_inbox, inbox_opts()} | required_modules(API, PubSubHost)];
required_modules_for_group(groupchat_notifications_with_inbox, API, PubSubHost)->
    [{mod_inbox, inbox_opts()}, {mod_muc_light, muc_light_opts()}
     | required_modules(API, PubSubHost)];
required_modules_for_group(muclight_msg_notifications, API, PubSubHost) ->
    [{mod_muc_light, muc_light_opts()} | required_modules(API, PubSubHost)];
required_modules_for_group(integration_with_sm_and_offline_storage, API, PubSubHost) ->
    [{mod_muc_light, muc_light_opts()},
     {mod_stream_management, [{ack_freq, never},
                              {resume_timeout,1}]},
     {mod_offline, []} |
     required_modules(API, PubSubHost)];
required_modules_for_group(enhanced_integration_with_sm, API, PubSubHost) ->
    [{mod_stream_management, [{ack_freq, never}]} |
     required_modules(API, PubSubHost, mod_event_pusher_push_plugin_enhanced)];
required_modules_for_group(_, API, PubSubHost) ->
    required_modules(API, PubSubHost).

required_modules(API, PubSubHost)->
    required_modules(API, PubSubHost, undefined).

required_modules(API, PubSubHost, PluginModule) ->
    VirtualHostOpt = case PubSubHost of
                         virtual -> [{virtual_pubsub_hosts, ["virtual.@HOSTS@"]}];
                         _ -> []
                     end,
    PushOpts = case PluginModule of
                   undefined -> VirtualHostOpt;
                   _ -> [{plugin_module, PluginModule} | VirtualHostOpt]
               end,
    PubSub = case PubSubHost of
                 virtual -> [];
                 _ ->
                     [{mod_pubsub, [{plugins, [<<"dag">>, <<"push">>]},
                                    {backend, mongoose_helper:mnesia_or_rdbms_backend()},
                                    {nodetree, <<"dag">>},
                                    {host, "pubsub.@HOST@"}]}]
             end,
    PushBackend = {push, [{backend, mongoose_helper:mnesia_or_rdbms_backend()} | PushOpts]},
    [
        {mod_push_service_mongoosepush, [{pool_name, mongoose_push_http},
                                         {api_version, API}]},
        {mod_event_pusher, [{backends, [PushBackend]}]} |
        PubSub
    ].

muc_light_opts() ->
    [
     {host, binary_to_list(?MUCLIGHTHOST)},
     {backend, mongoose_helper:mnesia_or_rdbms_backend()},
     {rooms_in_rosters, true}
    ].
inbox_opts() ->
    [{aff_changes, false},
     {remove_on_kicked, true},
     {groupchat, [muclight]},
     {markers, [displayed]}].

