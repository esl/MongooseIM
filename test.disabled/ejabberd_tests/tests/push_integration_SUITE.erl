-module(push_integration_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(NS_PUSH,                <<"urn:xmpp:push:0">>).
-define(NS_XDATA,               <<"jabber:x:data">>).
-define(NS_PUBSUB_PUB_OPTIONS,  <<"http://jabber.org/protocol/pubsub#publish-options">>).
-define(PUSH_FORM_TYPE,         <<"urn:xmpp:push:summary">>).
-define(MUCHOST,                <<"muclight.@HOST@">>).

-import(muc_light_SUITE,
    [
        room_bin_jid/1,
        create_room/6
    ]).
-import(escalus_ejabberd, [rpc/3]).
-import(push_SUITE, [
    enable_stanza/2, enable_stanza/3, enable_stanza/4,
    disable_stanza/1, disable_stanza/2,
    make_form/1, maybe_form/2
]).

-record(route, {from, to, packet}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, pm_msg_notifications},
        {group, muclight_msg_notifications}
    ].

groups() ->
    [
        {pm_msg_notifications, [], [
            pm_msg_notify_on_apns_no_click_action,
            pm_msg_notify_on_fcm_no_click_action,
            pm_msg_notify_on_apns_w_click_action,
            pm_msg_notify_on_fcm_w_click_action
        ]},
        {muclight_msg_notifications, [], [
            muclight_msg_notify_on_apns_no_click_action,
            muclight_msg_notify_on_fcm_no_click_action,
            muclight_msg_notify_on_apns_w_click_action,
            muclight_msg_notify_on_fcm_w_click_action
        ]}
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

connect_to_mongoose_push(Config) ->
    {ok, FCM} = shotgun:open("localhost", 12991, https),
    {ok, APNS} = shotgun:open("localhost", 12992, https),
    {ok, Push} = shotgun:open("localhost", 12993, https),

    {FCM, APNS, Push}.

init_per_suite(Config0) ->
    %% For mocking with unnamed functions
    shotgun:start(),

    {_Module, Binary, Filename} = code:get_object_code(?MODULE),
    rpc(code, load_binary, [?MODULE, Filename, Binary]),

    %% Start modules
    Config = dynamic_modules:save_modules(domain(), Config0),
    dynamic_modules:ensure_modules(domain(), required_modules()),

    try
        {FCM, APNS, Push} = connect_to_mongoose_push(Config),
        escalus:init_per_suite([{fcm, FCM}, {apns, APNS}, {push, Push} | Config])
    catch
        _:_ ->
            {skip, "MongoosePush is not available"}
    end.


end_per_suite(Config) ->
    shotgun:close(proplists:get_value(apns, Config)),
    shotgun:close(proplists:get_value(fcm, Config)),
    shotgun:close(proplists:get_value(push, Config)),

    escalus_fresh:clean(),
    dynamic_modules:restore_modules(domain(), Config),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config0) ->
    %% Some cleaning up
    FCM = proplists:get_value(fcm, Config0),
    APNS = proplists:get_value(apns, Config0),
    {ok, #{status_code := 200}} = shotgun:post(FCM, <<"/reset">>, #{}, <<>>, #{}),
    {ok, #{status_code := 200}} = shotgun:post(APNS, <<"/reset">>, #{}, <<>>, #{}),
    rpc(mod_muc_light_db_backend, force_clear, []),

    Config1 = escalus_fresh:create_users(Config0, [{bob, 1}, {alice, 1}, {kate, 1}]),
    Config = [{case_name, CaseName} | Config1],
    HTTPOpts = [{mongoose_push_http, [
        {server, "https://localhost:12993"}
    ]}],
    rpc(mongoose_http_client, start, [HTTPOpts]),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    rpc(mongoose_http_client, stop, []),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% GROUP pm_msg_notifications
%%--------------------------------------------------------------------

pm_msg_notify_on_apns_no_click_action(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = <<"pubsub.localhost">>,
            Node = {_, NodeName} = pubsub_node(),
            AliceJID = bare_jid(Alice),
            pubsub_tools:create_node(Bob, Node, [{type, <<"push">>}]),

            escalus:send(Bob, enable_stanza(PubsubJID, NodeName,
                                            [{<<"service">>, <<"apns">>},
                                             {<<"device_id">>, <<"mydevicetoken">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            [Notification = #{}] = get_push_logs(apns, Config),

            APNSData = maps:get(<<"aps">>, maps:get(<<"request_data">>, Notification)),
            APNSAlert = maps:get(<<"alert">>, APNSData),
            ?assertMatch(#{<<"device_token">> := <<"mydevicetoken">>}, Notification),
            ?assertMatch(#{<<"body">> := <<"OH, HAI!">>}, APNSAlert),
            ?assertMatch(#{<<"title">> := AliceJID}, APNSAlert),
            ?assertMatch(#{<<"badge">> := 1}, APNSData),
            ?assertMatch(#{<<"category">> := null}, APNSData),

            ok
        end).

pm_msg_notify_on_fcm_no_click_action(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = <<"pubsub.localhost">>,
            Node = {_, NodeName} = pubsub_node(),
            AliceJID = bare_jid(Alice),
            pubsub_tools:create_node(Bob, Node, [{type, <<"push">>}]),

            escalus:send(Bob, enable_stanza(PubsubJID, NodeName,
                                            [{<<"service">>, <<"fcm">>},
                                             {<<"device_id">>, <<"mydevicetoken">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            [Notification = #{}] = get_push_logs(fcm, Config),

            FCMData = maps:get(<<"notification">>, maps:get(<<"request_data">>, Notification)),
            ?assertMatch(#{<<"device_token">> := <<"mydevicetoken">>}, Notification),
            ?assertMatch(#{<<"body">> := <<"OH, HAI!">>}, FCMData),
            ?assertMatch(#{<<"title">> := AliceJID}, FCMData),
            ?assertMatch(#{<<"tag">> := AliceJID}, FCMData),
            ?assertMatch(#{<<"click_action">> :=  null}, FCMData),

            ok
        end).

pm_msg_notify_on_apns_w_click_action(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = <<"pubsub.localhost">>,
            Node = {_, NodeName} = pubsub_node(),
            AliceJID = bare_jid(Alice),
            pubsub_tools:create_node(Bob, Node, [{type, <<"push">>}]),

            escalus:send(Bob, enable_stanza(PubsubJID, NodeName,
                                            [{<<"service">>, <<"apns">>},
                                             {<<"device_id">>, <<"mydevicetoken">>},
                                             {<<"click_action">>, <<"myactivity">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            [Notification = #{}] = get_push_logs(apns, Config),

            APNSData = maps:get(<<"aps">>, maps:get(<<"request_data">>, Notification)),
            APNSAlert = maps:get(<<"alert">>, APNSData),
            ?assertMatch(#{<<"device_token">> := <<"mydevicetoken">>}, Notification),
            ?assertMatch(#{<<"body">> := <<"OH, HAI!">>}, APNSAlert),
            ?assertMatch(#{<<"title">> := AliceJID}, APNSAlert),
            ?assertMatch(#{<<"badge">> := 1}, APNSData),
            ?assertMatch(#{<<"category">> := <<"myactivity">>}, APNSData),

            ok
        end).

pm_msg_notify_on_fcm_w_click_action(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = <<"pubsub.localhost">>,
            Node = {_, NodeName} = pubsub_node(),
            AliceJID = bare_jid(Alice),
            pubsub_tools:create_node(Bob, Node, [{type, <<"push">>}]),

            escalus:send(Bob, enable_stanza(PubsubJID, NodeName,
                                            [{<<"service">>, <<"fcm">>},
                                             {<<"device_id">>, <<"mydevicetoken">>},
                                             {<<"click_action">>, <<"myactivity">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            [Notification = #{}] = get_push_logs(fcm, Config),

            FCMData = maps:get(<<"notification">>, maps:get(<<"request_data">>, Notification)),
            ?assertMatch(#{<<"device_token">> := <<"mydevicetoken">>}, Notification),
            ?assertMatch(#{<<"body">> := <<"OH, HAI!">>}, FCMData),
            ?assertMatch(#{<<"title">> := AliceJID}, FCMData),
            ?assertMatch(#{<<"tag">> := AliceJID}, FCMData),
            ?assertMatch(#{<<"click_action">> :=  <<"myactivity">>}, FCMData),

            ok
        end).

%%--------------------------------------------------------------------
%% GROUP muclight_msg_notifications
%%--------------------------------------------------------------------


muclight_msg_notify_on_apns_no_click_action(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            PubsubJID = <<"pubsub.localhost">>,
            Room = room_name(Config),
            BobJID = bare_jid(Bob),
            RoomJID = room_bin_jid(Room),
            SenderJID = <<RoomJID/binary, "/", BobJID/binary>>,
            Node = {_, NodeName} = pubsub_node(),

            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),
            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, NodeName,
                                              [{<<"device_id">>, <<"mydevicetoken">>},
                                               {<<"service">>, <<"apns">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            [Notification = #{}] = get_push_logs(apns, Config),

            APNSData = maps:get(<<"aps">>, maps:get(<<"request_data">>, Notification)),
            APNSAlert = maps:get(<<"alert">>, APNSData),
            ?assertMatch(#{<<"device_token">> := <<"mydevicetoken">>}, Notification),
            ?assertMatch(#{<<"body">> := <<"Heyah!">>}, APNSAlert),
            ?assertMatch(#{<<"title">> := SenderJID}, APNSAlert),
            ?assertMatch(#{<<"badge">> := 1}, APNSData),
            ?assertMatch(#{<<"category">> := null}, APNSData),

            ok
        end).

muclight_msg_notify_on_fcm_no_click_action(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            PubsubJID = <<"pubsub.localhost">>,
            Room = room_name(Config),
            BobJID = bare_jid(Bob),
            RoomJID = room_bin_jid(Room),
            SenderJID = <<RoomJID/binary, "/", BobJID/binary>>,
            Node = {_, NodeName} = pubsub_node(),

            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),
            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, NodeName,
                                              [{<<"device_id">>, <<"mydevicetoken">>},
                                               {<<"service">>, <<"fcm">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            [Notification = #{}] = get_push_logs(fcm, Config),

            FCMData = maps:get(<<"notification">>, maps:get(<<"request_data">>, Notification)),
            ?assertMatch(#{<<"device_token">> := <<"mydevicetoken">>}, Notification),
            ?assertMatch(#{<<"body">> := <<"Heyah!">>}, FCMData),
            ?assertMatch(#{<<"title">> := SenderJID}, FCMData),
            ?assertMatch(#{<<"tag">> := SenderJID}, FCMData),
            ?assertMatch(#{<<"click_action">> :=  null}, FCMData),

            ok
        end).

muclight_msg_notify_on_apns_w_click_action(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            PubsubJID = <<"pubsub.localhost">>,
            Room = room_name(Config),
            BobJID = bare_jid(Bob),
            RoomJID = room_bin_jid(Room),
            SenderJID = <<RoomJID/binary, "/", BobJID/binary>>,
            Node = {_, NodeName} = pubsub_node(),

            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),
            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, NodeName,
                                              [{<<"device_id">>, <<"mydevicetoken">>},
                                               {<<"service">>, <<"apns">>},
                                               {<<"click_action">>, <<"myactivity">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            [Notification = #{}] = get_push_logs(apns, Config),

            APNSData = maps:get(<<"aps">>, maps:get(<<"request_data">>, Notification)),
            APNSAlert = maps:get(<<"alert">>, APNSData),
            ?assertMatch(#{<<"device_token">> := <<"mydevicetoken">>}, Notification),
            ?assertMatch(#{<<"body">> := <<"Heyah!">>}, APNSAlert),
            ?assertMatch(#{<<"title">> := SenderJID}, APNSAlert),
            ?assertMatch(#{<<"badge">> := 1}, APNSData),
            ?assertMatch(#{<<"category">> := <<"myactivity">>}, APNSData),

            ok
        end).

muclight_msg_notify_on_fcm_w_click_action(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            PubsubJID = <<"pubsub.localhost">>,
            Room = room_name(Config),
            BobJID = bare_jid(Bob),
            RoomJID = room_bin_jid(Room),
            SenderJID = <<RoomJID/binary, "/", BobJID/binary>>,
            Node = {_, NodeName} = pubsub_node(),

            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),
            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, NodeName,
                                            [{<<"device_id">>, <<"mydevicetoken">>},
                                             {<<"service">>, <<"fcm">>},
                                             {<<"click_action">>, <<"myactivity">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            [Notification = #{}] = get_push_logs(fcm, Config),

            FCMData = maps:get(<<"notification">>, maps:get(<<"request_data">>, Notification)),
            ?assertMatch(#{<<"device_token">> := <<"mydevicetoken">>}, Notification),
            ?assertMatch(#{<<"body">> := <<"Heyah!">>}, FCMData),
            ?assertMatch(#{<<"title">> := SenderJID}, FCMData),
            ?assertMatch(#{<<"tag">> := SenderJID}, FCMData),
            ?assertMatch(#{<<"click_action">> :=  <<"myactivity">>}, FCMData),

            ok
        end).


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

get_push_logs(Service, Config) ->
    PushMock = proplists:get_value(Service, Config),
    wait_for(timer:seconds(5), fun() ->
        {ok, #{body := Body}} = shotgun:get(PushMock, <<"/activity">>),
        #{<<"logs">> := Logs} = jiffy:decode(Body, [return_maps]),
        1 = length(Logs),
        Logs
     end).

%% ----------------------------------
%% Other helpers
%% ----------------------------------

process_packet(From, To, Packet, TestCasePid) ->
    TestCasePid ! #route{from = From, to = To, packet = Packet}.

create_room(Room, [Owner | Members], Config) ->
    Domain = ct:get_config({hosts, mim, domain}),
    create_room(Room, <<"muclight.", Domain/binary>>, Owner, Members,
                                Config, <<"v1">>).

received_route() ->
    receive
        #route{} = R ->
            R
    after timer:seconds(5) ->
        false
    end.

truly(false) ->
    false;
truly(undefined) ->
    false;
truly(_) ->
    true.

bare_jid(JIDOrClient) ->
    ShortJID = escalus_client:short_jid(JIDOrClient),
    list_to_binary(string:to_lower(binary_to_list(ShortJID))).

pubsub_jid(Config) ->
    CaseName = proplists:get_value(case_name, Config),
    <<"pubsub@", (atom_to_binary(CaseName, utf8))/binary>>.

room_name(Config) ->
    CaseName = proplists:get_value(case_name, Config),
    <<"room_", (atom_to_binary(CaseName, utf8))/binary>>.

is_offline(LUser, LServer) ->
    case catch lists:max(rpc(ejabberd_sm, get_user_present_pids, [LUser, LServer])) of
        {Priority, _} when is_integer(Priority), Priority >= 0 ->
            false;
        _ ->
            true
    end.

become_unavailable(Client) ->
    escalus:send(Client, escalus_stanza:presence(<<"unavailable">>)),
    true = wait_for(timer:seconds(20), fun() ->
        is_offline(lower(escalus_client:username(Client)), lower(escalus_client:server(Client)))
    end). %% There is no ACK for unavailable status

wait_for(TimeLeft, Fun) when TimeLeft < 0 ->
    Fun();
wait_for(TimeLeft, Fun) ->
    Step = 500,
    try
        case Fun() of
            ok -> ok;
            true -> true;
            {ok, _} = R -> R
        end
    catch
        _:_ ->
            timer:sleep(Step),
            wait_for(TimeLeft - Step, Fun)
    end.

lower(Bin) when is_binary(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).

domain() ->
    ct:get_config({hosts, mim, domain}).

node_addr() ->
    Domain = domain(),
    <<"pubsub.", Domain/binary>>.

rand_name(Prefix) ->
    Suffix = base64:encode(crypto:rand_bytes(5)),
    <<Prefix/binary, "_", Suffix/binary>>.

pubsub_node_name() ->
    rand_name(<<"princely_musings">>).

pubsub_node() ->
    {node_addr(), pubsub_node_name()}.

required_modules() ->
    [
        {mod_pubsub, [
            {plugins, [<<"dag">>, <<"push">>]},
            {nodetree, <<"dag">>},
            {host, "pubsub.@HOST@"}
        ]},
        {mod_push_service_mongoosepush, [
            {pool_name, mongoose_push_http},
            {api_version, "v1"}
        ]},
        {mod_push, [
            {backend, mnesia}
        ]},
        {mod_muc_light, [
            {host, binary_to_list(?MUCHOST)},
            {backend, mnesia},
            {rooms_in_rosters, true}
        ]}
    ].
