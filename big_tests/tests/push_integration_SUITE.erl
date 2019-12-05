-module(push_integration_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("inbox.hrl").

-define(MUCLIGHTHOST, <<"muclight.localhost">>).
-define(RPC_SPEC, #{node => distributed_helper:mim()}).



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
         become_available/2,
         become_available/3
        ]).
-import(distributed_helper, [rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, pm_msg_notifications},
        {group, muclight_msg_notifications},
        {group, pm_notifications_with_inbox},
        {group, groupchat_notifications_with_inbox},
        {group, failure_cases_v3},
        {group, failure_cases_v2}
    ].

groups() ->
    G = [
         {pm_msg_notifications, [parallel],
          [
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
    [no_push_notification_for_expired_device,
     no_push_notification_for_internal_mongoose_push_error].

%%    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    catch mongoose_push_mock:stop(),
    mongoose_push_mock:start(Config0),
    Port = mongoose_push_mock:port(),

    %% Start modules
    Config = dynamic_modules:save_modules(domain(), Config0),
    dynamic_modules:ensure_modules(domain(), required_modules("v3")),

    PoolOpts = [{strategy, available_worker}, {workers, 20}],
    HTTPOpts = [{server, "https://localhost:" ++ integer_to_list(Port)}],
    rpc(?RPC_SPEC, mongoose_wpool, start_configured_pools,
        [[{http, global, mongoose_push_http, PoolOpts, HTTPOpts}]]),
    escalus:init_per_suite(Config).


end_per_suite(Config) ->
    escalus_fresh:clean(),
    rpc(?RPC_SPEC, mongoose_wpool, stop, [http, global, mongoose_push_http]),
    dynamic_modules:restore_modules(domain(), Config),
    mongoose_push_mock:stop(),
    escalus:end_per_suite(Config).

init_per_group(G, Config) when G =:= pm_notifications_with_inbox;
                               G =:= groupchat_notifications_with_inbox ->
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

%%--------------------------------------------------------------------
%% GROUP pm_msg_notifications
%%--------------------------------------------------------------------


pm_msg_notify_on_apns(Config, EnableOpts) ->
    escalus:fresh_story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            {SenderJID, DeviceToken} = pm_conversation(Alice, Bob, <<"apns">>, EnableOpts),
            {Notification, _} = wait_for_push_request(DeviceToken),

            assert_push_notification(Notification, <<"apns">>, EnableOpts, SenderJID, [])

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

    case proplists:get_value(<<"topic">>, EnableOpts) of
        undefined -> ok;
        Topic ->
            ?assertMatch(Topic, maps:get(<<"topic">>, Notification, undefined))
    end.


pm_msg_notify_on_fcm(Config, EnableOpts) ->
    escalus:fresh_story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            {SenderJID, DeviceToken} = pm_conversation(Alice, Bob, <<"fcm">>, EnableOpts),
            {Notification, _} = wait_for_push_request(DeviceToken),

            assert_push_notification(Notification, <<"fcm">>, EnableOpts, SenderJID)

        end).

pm_msg_notify_on_apns_no_click_action(Config) ->
    pm_msg_notify_on_apns(Config, []).

pm_msg_notify_on_fcm_no_click_action(Config) ->
    pm_msg_notify_on_fcm(Config, []).

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
              DeviceToken = enable_push_for_user(Bob, Service, EnableOpts),

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
              DeviceToken = enable_push_for_user(Bob, Service, EnableOpts),
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

              KateToken = enable_push_for_user(Kate, Service, EnableOpts),

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
            DeviceToken = enable_push_for_user(Bob, <<"apns">>, EnableOpts),

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
            DeviceToken = enable_push_for_user(Bob, <<"fcm">>, EnableOpts),

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

              KateToken = enable_push_for_user(Kate, Service, EnableOpts),

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
            DeviceToken = enable_push_for_user(Bob, <<"fcm">>, [], Response),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
            {_, Response} = wait_for_push_request(DeviceToken)

        end).

mongoose_push_unregistered_device_resp(Config) ->
    case ?config(api_v, Config) of
        "v3" ->
            {410, jiffy:encode(#{<<"reason">> => <<"unregistered">>})};
        "v2" ->
            {500, jiffy:encode(#{<<"details">> => <<"probably_unregistered">>})}
    end.

no_push_notification_for_internal_mongoose_push_error(Config) ->
    escalus:fresh_story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            Response = {503, jiffy:encode(#{<<"reason">> => <<"unspecified">>})},
            DeviceToken = enable_push_for_user(Bob, <<"fcm">>, [], Response),
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

pm_conversation(Alice, Bob, Service, EnableOpts) ->
    AliceJID = bare_jid(Alice),
    DeviceToken = enable_push_for_user(Bob, Service, EnableOpts),
    escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
    {AliceJID, DeviceToken}.

enable_push_for_user(User, Service, EnableOpts) ->
    enable_push_for_user(User, Service, EnableOpts, {200, <<"OK">>}).

enable_push_for_user(User, Service, EnableOpts, MockResponse) ->
    PubsubJID = node_addr(),
    Node = {_, NodeName} = pubsub_node(),

    DeviceToken = gen_token(),

    Configuration = [{<<"pubsub#access_model">>, <<"whitelist">>},
                     {<<"pubsub#publish_model">>, <<"publishers">>}],
    pubsub_tools:create_node(User, Node, [{type, <<"push">>},
                                          {config, Configuration}]),

    add_user_server_to_whitelist(User, Node),
    escalus:send(User, enable_stanza(PubsubJID, NodeName,
                                     [{<<"service">>, Service},
                                      {<<"device_id">>, DeviceToken}] ++ EnableOpts)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(User)),
    mongoose_push_mock:subscribe(DeviceToken, MockResponse),
    become_unavailable(User),
    DeviceToken.

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

wait_for_push_request(DeviceToken) ->
    mongoose_push_mock:wait_for_push_request(DeviceToken).


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

node_addr() ->
    Domain = domain(),
    <<"pubsub.", Domain/binary>>.

rand_name(Prefix) ->
    Suffix = base64:encode(crypto:strong_rand_bytes(5)),
    <<Prefix/binary, "_", Suffix/binary>>.

pubsub_node_name() ->
    rand_name(<<"princely_musings">>).

pubsub_node() ->
    {node_addr(), pubsub_node_name()}.

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
    Modules = required_modules_for_group(G, MongoosePushAPI),
    C = dynamic_modules:save_modules(domain(), Config),
    dynamic_modules:ensure_modules(domain(), Modules),
    [{api_v, MongoosePushAPI} | C].

mongoose_push_api_for_group(failure_cases_v2) ->
    "v2";
mongoose_push_api_for_group(_) ->
    "v3".

required_modules_for_group(pm_notifications_with_inbox, API) ->
    [{mod_inbox, inbox_opts()} | required_modules(API)];
required_modules_for_group(groupchat_notifications_with_inbox, API)->
    [{mod_inbox, inbox_opts()}, {mod_muc_light, muc_light_opts()} |
     required_modules(API)];
required_modules_for_group(muclight_msg_notifications, API) ->
    [{mod_muc_light, muc_light_opts()} | required_modules(API)];
required_modules_for_group(_, API) ->
    required_modules(API).

required_modules(API) ->
    PushBackend = {push, [{backend, mongoose_helper:mnesia_or_rdbms_backend()}]},
    [
     {mod_pubsub, [{plugins, [<<"dag">>, <<"push">>]},
                   {backend, mongoose_helper:mnesia_or_rdbms_backend()},
                   {nodetree, <<"dag">>},
                   {host, "pubsub.@HOST@"}]},
     {mod_push_service_mongoosepush, [{pool_name, mongoose_push_http},
                                      {api_version, API}]},
     {mod_event_pusher, [{backends, [PushBackend]}]}
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
