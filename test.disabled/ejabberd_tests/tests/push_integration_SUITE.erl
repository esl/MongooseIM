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


-import(muc_light_helper,
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
        {group, muclight_msg_notifications}
    ].

groups() ->
    [
        {pm_msg_notifications, [parallel], [
            pm_msg_notify_on_apns_no_click_action,
            pm_msg_notify_on_fcm_no_click_action,
            pm_msg_notify_on_apns_w_click_action,
            pm_msg_notify_on_fcm_w_click_action,
            pm_msg_notify_on_apns_silent,
            pm_msg_notify_on_fcm_silent,
            pm_msg_notify_on_apns_w_topic
        ]},
        {muclight_msg_notifications, [parallel], [
            muclight_msg_notify_on_apns_no_click_action,
            muclight_msg_notify_on_fcm_no_click_action,
            muclight_msg_notify_on_apns_w_click_action,
            muclight_msg_notify_on_fcm_w_click_action,
            muclight_msg_notify_on_apns_silent,
            muclight_msg_notify_on_fcm_silent,
            muclight_msg_notify_on_w_topic
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

    try
        %% Connect only to test if MongoosePush is available
        PushPortStr = getenv("MONGOOSE_PUSH_PORT", ?DEFAULT_MONGOOSE_PUSH_PORT),
        {ok, _} = gen_tcp:connect("localhost", list_to_integer(PushPortStr), []),
        escalus:init_per_suite(Config)
    catch
        _:_ ->
            {skip,  "MongoosePush is not available"}
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


pm_msg_notify_on_apns(Config, EnableOpts) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            {SenderJID, DeviceToken} = pm_conversation(Alice, Bob, <<"apns">>, EnableOpts),
            [Notification = #{}] = get_push_logs(apns, DeviceToken, Config),

            APNSNotification = maps:get(<<"aps">>, maps:get(<<"request_data">>, Notification),
                                        undefined),
            APNSData = maps:remove(<<"aps">>, maps:get(<<"request_data">>, Notification)),
            APNSAlert = maps:get(<<"alert">>, APNSNotification, undefined),

            case proplists:get_value(<<"silent">>, EnableOpts) of
                undefined ->
                    ?assertMatch(#{<<"body">> := <<"OH, HAI!">>}, APNSAlert),
                    ?assertMatch(#{<<"title">> := SenderJID}, APNSAlert),
                    ?assertMatch(#{<<"badge">> := 1}, APNSNotification),

                    case proplists:get_value(<<"click_action">>, EnableOpts) of
                        undefined ->
                            ?assertMatch(#{<<"category">> := null}, APNSNotification);
                        Activity ->
                            ?assertMatch(#{<<"category">> := Activity}, APNSNotification)
                    end;
                <<"true">> ->
                    ?assert(not maps:is_key(<<"aps">>, APNSNotification)),
                    ?assert(not maps:is_key(<<"sound">>, APNSNotification)),
                    ?assert(not maps:is_key(<<"badge">>, APNSNotification)),
                    ?assertMatch(#{<<"content-available">> := 1}, APNSNotification),
                    ?assertMatch(#{<<"last-message-body">> := <<"OH, HAI!">>}, APNSData),
                    ?assertMatch(#{<<"last-message-sender">> := SenderJID}, APNSData),
                    ?assertMatch(#{<<"message-count">> := 1}, APNSData)
            end,

            case  proplists:get_value(<<"topic">>, EnableOpts) of
                undefined -> ok;
                Topic ->
                    Headers = maps:get(<<"request_headers">>, Notification),
                    ?assertMatch(Topic, maps:get(<<"apns-topic">>, Headers, undefined))
            end,

            ok
        end).

pm_msg_notify_on_fcm(Config, EnableOpts) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            {SenderJID, DeviceToken} = pm_conversation(Alice, Bob, <<"fcm">>, EnableOpts),
            [Notification = #{}] = get_push_logs(fcm, DeviceToken, Config),

            FCMNotification = maps:get(<<"notification">>,
                                       maps:get(<<"request_data">>, Notification)),

            FCMData = maps:get(<<"data">>,
                               maps:get(<<"request_data">>, Notification)),

            case proplists:get_value(<<"silent">>, EnableOpts) of
                undefined ->
                    ?assertMatch(#{<<"body">> := <<"OH, HAI!">>}, FCMNotification),
                    ?assertMatch(#{<<"title">> := SenderJID}, FCMNotification),
                    ?assertMatch(#{<<"tag">> := SenderJID}, FCMNotification),

                    case proplists:get_value(<<"click_action">>, EnableOpts) of
                        undefined ->
                            ?assertMatch(#{<<"click_action">> :=  null}, FCMNotification);
                        Activity ->
                            ?assertMatch(#{<<"click_action">> :=  Activity}, FCMNotification)
                    end;
                <<"true">> ->
                    ?assertMatch(#{<<"last-message-body">> := <<"OH, HAI!">>}, FCMData),
                    ?assertMatch(#{<<"last-message-sender">> := SenderJID}, FCMData),
                    ?assertMatch(#{<<"message-count">> := 1}, FCMData)
            end,

            ok
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
%% GROUP muclight_msg_notifications
%%--------------------------------------------------------------------


muclight_msg_notify_on_apns(Config, EnableOpts) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            {SenderJID, DeviceToken} =
                muclight_conversation(Config, Alice, Bob, <<"apns">>,EnableOpts),
            [Notification = #{}] = get_push_logs(apns, DeviceToken, Config),

            APNSNotification = maps:get(<<"aps">>, maps:get(<<"request_data">>, Notification),
                                        undefined),
            APNSData = maps:remove(<<"aps">>, maps:get(<<"request_data">>, Notification)),
            APNSAlert = maps:get(<<"alert">>, APNSNotification, undefined),

            case proplists:get_value(<<"silent">>, EnableOpts) of
                undefined ->
                    ?assertMatch(#{<<"body">> := <<"Heyah!">>}, APNSAlert),
                    ?assertMatch(#{<<"title">> := SenderJID}, APNSAlert),
                    ?assertMatch(#{<<"badge">> := 1}, APNSNotification),

                    case proplists:get_value(<<"click_action">>, EnableOpts) of
                        undefined ->
                            ?assertMatch(#{<<"category">> := null}, APNSNotification);
                        Activity ->
                            ?assertMatch(#{<<"category">> := Activity}, APNSNotification)
                    end;
                <<"true">> ->
                    ?assert(not maps:is_key(<<"aps">>, APNSNotification)),
                    ?assert(not maps:is_key(<<"sound">>, APNSNotification)),
                    ?assert(not maps:is_key(<<"badge">>, APNSNotification)),
                    ?assertMatch(#{<<"content-available">> := 1}, APNSNotification),
                    ?assertMatch(#{<<"last-message-body">> := <<"Heyah!">>}, APNSData),
                    ?assertMatch(#{<<"last-message-sender">> := SenderJID}, APNSData),
                    ?assertMatch(#{<<"message-count">> := 1}, APNSData)
            end,

            case  proplists:get_value(<<"topic">>, EnableOpts) of
                undefined -> ok;
                Topic ->
                    Headers = maps:get(<<"request_headers">>, Notification),
                    ?assertMatch(Topic, maps:get(<<"apns-topic">>, Headers, undefined))
            end,

            ok
        end).

muclight_msg_notify_on_fcm(Config, EnableOpts) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            {SenderJID, DeviceToken} =
                muclight_conversation(Config, Alice, Bob, <<"fcm">>,EnableOpts),
            [Notification = #{}] = get_push_logs(fcm, DeviceToken, Config),

            FCMNotification = maps:get(<<"notification">>,
                                       maps:get(<<"request_data">>, Notification)),
            FCMData = maps:get(<<"data">>,
                               maps:get(<<"request_data">>, Notification)),

            case proplists:get_value(<<"silent">>, EnableOpts) of
                undefined ->
                    ?assertMatch(#{<<"body">> := <<"Heyah!">>}, FCMNotification),
                    ?assertMatch(#{<<"title">> := SenderJID}, FCMNotification),
                    ?assertMatch(#{<<"tag">> := SenderJID}, FCMNotification),

                    case proplists:get_value(<<"click_action">>, EnableOpts) of
                        undefined ->
                            ?assertMatch(#{<<"click_action">> :=  null}, FCMNotification);
                        Activity ->
                            ?assertMatch(#{<<"click_action">> :=  Activity}, FCMNotification)
                    end;
                <<"true">> ->
                    ?assertMatch(#{<<"last-message-body">> := <<"Heyah!">>}, FCMData),
                    ?assertMatch(#{<<"last-message-sender">> := SenderJID}, FCMData),
                    ?assertMatch(#{<<"message-count">> := 1}, FCMData)
            end,

            ok
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


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

muclight_conversation(Config, Alice, Bob, Service, EnableOpts) ->
    Room = room_name(Config),
    BobJID = bare_jid(Bob),
    RoomJID = room_bin_jid(Room),
    SenderJID = <<RoomJID/binary, "/", BobJID/binary>>,
    create_room(Room, [bob, alice, kate], Config),

    DeviceToken = enable_push_for_user(Alice, Service, EnableOpts),

    Msg = <<"Heyah!">>,
    Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
    escalus:send(Bob, Stanza),
    {SenderJID, DeviceToken}.

pm_conversation(Alice, Bob, Service, EnableOpts) ->
    AliceJID = bare_jid(Alice),
    DeviceToken = enable_push_for_user(Bob, Service, EnableOpts),
    escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
    {AliceJID, DeviceToken}.


enable_push_for_user(User, Service, EnableOpts) ->
    PubsubJID = node_addr(),
    Node = {_, NodeName} = pubsub_node(),

    DeviceToken = gen_token(),

    pubsub_tools:create_node(User, Node, [{type, <<"push">>}]),
    escalus:send(User, enable_stanza(PubsubJID, NodeName,
                                     [{<<"service">>, Service},
                                      {<<"device_id">>, DeviceToken}] ++ EnableOpts)),
    escalus:assert(is_result, escalus:wait_for_stanza(User)),
    become_unavailable(User),
    DeviceToken.


get_push_logs(Service, DeviceToken, _Config) ->
    wait_for(timer:seconds(10), fun() ->
        PushMock = connect_to(Service),
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
    escalus_utils:jid_to_lower(ShortJID).

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
    integer_to_binary(binary:decode_unsigned(crypto:strong_rand_bytes(16)), 24).

become_unavailable(Client) ->
    escalus:send(Client, escalus_stanza:presence(<<"unavailable">>)),
    true = wait_for(timer:seconds(20), fun() ->
        is_offline(escalus_utils:jid_to_lower(escalus_client:username(Client)),
                   escalus_utils:jid_to_lower(escalus_client:server(Client)))
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

required_modules() ->
    [
        {mod_pubsub, [
            {plugins, [<<"dag">>, <<"push">>]},
            {nodetree, <<"dag">>},
            {host, "pubsub.@HOST@"}
        ]},
        {mod_push_service_mongoosepush, [
            {pool_name, mongoose_push_http},
            {api_version, "v2"}
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
