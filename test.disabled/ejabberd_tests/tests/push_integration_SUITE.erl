-module(push_integration_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(MUCHOST,                        <<"muclight.@HOST@">>).
-define(DEFAULT_FCM_MOCK_PORT,          "12991").
-define(DEFAULT_APNS_MOCK_PORT,         "12992").
-define(DEFAULT_MONGOOSE_PUSH_PORT,     "12993").


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

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, pm_msg_notifications},
        {group, pm_msg_notifications},
        {group, pm_msg_notifications},
        {group, muclight_msg_notifications}
    ].

groups() ->
    [
        {pm_msg_notifications, [parallel], [
            pm_msg_notify_on_apns_no_click_action,
            pm_msg_notify_on_fcm_no_click_action,
            pm_msg_notify_on_apns_w_click_action,
            pm_msg_notify_on_fcm_w_click_action
        ]},
        {muclight_msg_notifications, [parallel], [
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

connect_to(fcm) ->
    FCMPort = list_to_integer(getenv("FCM_MOCK_PORT", ?DEFAULT_FCM_MOCK_PORT)),
    {ok, FCM} = h2_client:start_link(https, "localhost", FCMPort),
    FCM;
connect_to(apns) ->
    APNSPort = list_to_integer(getenv("APNS_MOCK_PORT", ?DEFAULT_APNS_MOCK_PORT)),
    {ok, APNS} = h2_client:start_link(https, "localhost", APNSPort),
    APNS;
connect_to(mongoose_push) ->
    PushPort = list_to_integer(getenv("MONGOOSE_PUSH_PORT", ?DEFAULT_MONGOOSE_PUSH_PORT)),
    {ok, Push} = h2_client:start_link(https, "localhost", PushPort),
    Push.

init_per_suite(Config0) ->
    %% For mocking with unnamed functions
    {_Module, Binary, Filename} = code:get_object_code(?MODULE),
    rpc(code, load_binary, [?MODULE, Filename, Binary]),

    application:set_env(chatterbox, ssl_options, []),
    application:ensure_all_started(chatterbox),

    %% Start modules
    Config = dynamic_modules:save_modules(domain(), Config0),
    dynamic_modules:ensure_modules(domain(), required_modules()),

    rpc(mongoose_http_client, start, [[]]),
    rpc(mongoose_http_client, start_pool, [mongoose_push_http, [
        {server, "https://localhost:" ++ getenv("MONGOOSE_PUSH_PORT",
                                                   ?DEFAULT_MONGOOSE_PUSH_PORT)}
    ]]),

    SSLAppVersion = proplists:get_value(ssl_app, ssl:versions()),
    [SSLMajorVersion | _] = string:tokens(SSLAppVersion, "."),
    ct:pal("SSL Version ~p", [{SSLAppVersion, SSLMajorVersion}]),
    case list_to_integer(SSLMajorVersion) >= 7 of
        true ->
            try
                %% Connect only to test if MongoosePush is available
                PushPortStr = getenv("MONGOOSE_PUSH_PORT", ?DEFAULT_MONGOOSE_PUSH_PORT),
                {ok, _} = gen_tcp:connect("localhost", list_to_integer(PushPortStr), []),
                escalus:init_per_suite(Config)
            catch
                _:_ ->
                    {skip,  "MongoosePush is not available"}
            end;
        false ->
            {skip,  "This test suite requires Erlang's SSL 7.0+ that is available in"
                    "Erlang/OTP 18.0+"}
    end.


end_per_suite(Config) ->
    escalus_fresh:clean(),
    rpc(mongoose_http_client, stop_pool, [mongoose_push_http]),
    rpc(mongoose_http_client, stop, []),
    dynamic_modules:restore_modules(domain(), Config),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    %% Some cleaning up
    FCM = connect_to(fcm),
    APNS = connect_to(apns),
    {ok, 200, _} = h2_req(FCM, post, <<"/reset">>),
    {ok, 200, _} = h2_req(APNS, post, <<"/reset">>),
    rpc(mod_muc_light_db_backend, force_clear, []),

    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config0) ->
    Config1 = escalus_fresh:create_users(Config0, [{bob, 1}, {alice, 1}, {kate, 1}]),
    Config = [{case_name, CaseName} | Config1],
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
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
            DeviceToken = gen_token(),

            pubsub_tools:create_node(Bob, Node, [{type, <<"push">>}]),
            escalus:send(Bob, enable_stanza(PubsubJID, NodeName,
                                            [{<<"service">>, <<"apns">>},
                                             {<<"device_id">>, DeviceToken}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            [Notification = #{}] = get_push_logs(apns, DeviceToken, Config),

            APNSData = maps:get(<<"aps">>, maps:get(<<"request_data">>, Notification)),
            APNSAlert = maps:get(<<"alert">>, APNSData),
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
            DeviceToken = gen_token(),

            pubsub_tools:create_node(Bob, Node, [{type, <<"push">>}]),
            escalus:send(Bob, enable_stanza(PubsubJID, NodeName,
                                            [{<<"service">>, <<"fcm">>},
                                             {<<"device_id">>, DeviceToken}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            [Notification = #{}] = get_push_logs(fcm, DeviceToken, Config),

            FCMData = maps:get(<<"notification">>, maps:get(<<"request_data">>, Notification)),
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
            DeviceToken = gen_token(),

            pubsub_tools:create_node(Bob, Node, [{type, <<"push">>}]),
            escalus:send(Bob, enable_stanza(PubsubJID, NodeName,
                                            [{<<"service">>, <<"apns">>},
                                             {<<"device_id">>, DeviceToken},
                                             {<<"click_action">>, <<"myactivity">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            [Notification = #{}] = get_push_logs(apns, DeviceToken, Config),

            APNSData = maps:get(<<"aps">>, maps:get(<<"request_data">>, Notification)),
            APNSAlert = maps:get(<<"alert">>, APNSData),
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
            DeviceToken = gen_token(),

            pubsub_tools:create_node(Bob, Node, [{type, <<"push">>}]),
            escalus:send(Bob, enable_stanza(PubsubJID, NodeName,
                                            [{<<"service">>, <<"fcm">>},
                                             {<<"device_id">>, DeviceToken},
                                             {<<"click_action">>, <<"myactivity">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            [Notification = #{}] = get_push_logs(fcm, DeviceToken, Config),

            FCMData = maps:get(<<"notification">>, maps:get(<<"request_data">>, Notification)),
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
            DeviceToken = gen_token(),

            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),
            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, NodeName,
                                              [{<<"device_id">>, DeviceToken},
                                               {<<"service">>, <<"apns">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            [Notification = #{}] = get_push_logs(apns, DeviceToken, Config),

            APNSData = maps:get(<<"aps">>, maps:get(<<"request_data">>, Notification)),
            APNSAlert = maps:get(<<"alert">>, APNSData),
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
            DeviceToken = gen_token(),

            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),
            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, NodeName,
                                              [{<<"device_id">>, DeviceToken},
                                               {<<"service">>, <<"fcm">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            [Notification = #{}] = get_push_logs(fcm, DeviceToken, Config),

            FCMData = maps:get(<<"notification">>, maps:get(<<"request_data">>, Notification)),
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
            DeviceToken = gen_token(),

            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),
            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, NodeName,
                                              [{<<"device_id">>, DeviceToken},
                                               {<<"service">>, <<"apns">>},
                                               {<<"click_action">>, <<"myactivity">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            [Notification = #{}] = get_push_logs(apns, DeviceToken, Config),

            APNSData = maps:get(<<"aps">>, maps:get(<<"request_data">>, Notification)),
            APNSAlert = maps:get(<<"alert">>, APNSData),
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
            DeviceToken = gen_token(),

            pubsub_tools:create_node(Alice, Node, [{type, <<"push">>}]),
            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, NodeName,
                                            [{<<"device_id">>, DeviceToken},
                                             {<<"service">>, <<"fcm">>},
                                             {<<"click_action">>, <<"myactivity">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            [Notification = #{}] = get_push_logs(fcm, DeviceToken, Config),

            FCMData = maps:get(<<"notification">>, maps:get(<<"request_data">>, Notification)),
            ?assertMatch(#{<<"body">> := <<"Heyah!">>}, FCMData),
            ?assertMatch(#{<<"title">> := SenderJID}, FCMData),
            ?assertMatch(#{<<"tag">> := SenderJID}, FCMData),
            ?assertMatch(#{<<"click_action">> :=  <<"myactivity">>}, FCMData),

            ok
        end).


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

get_push_logs(Service, DeviceToken, Config) ->
    PushMock = connect_to(Service),
    wait_for(timer:seconds(10), fun() ->
        {ok, 200, Body} = h2_req(PushMock, get, <<"/activity">>),
        #{<<"logs">> := Logs} = jiffy:decode(Body, [return_maps]),
                                    
        DeviceLogs = lists:filter(
            fun(#{<<"device_token">> := Token}) ->
                DeviceToken =:= Token
            end, Logs),

        case length(DeviceLogs) > 0 of 
            true -> 
                DeviceLogs;
            false ->
                throw({no_push_messages, DeviceToken})
        end             
     end).

%% ----------------------------------
%% Other helpers
%% ----------------------------------

create_room(Room, [Owner | Members], Config) ->
    Domain = ct:get_config({hosts, mim, domain}),
    create_room(Room, <<"muclight.", Domain/binary>>, Owner, Members,
                                Config, <<"v1">>).

bare_jid(JIDOrClient) ->
    ShortJID = escalus_client:short_jid(JIDOrClient),
    list_to_binary(string:to_lower(binary_to_list(ShortJID))).

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

gen_token() ->
    integer_to_binary(binary:decode_unsigned(crypto:rand_bytes(16)), 24).

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
