-module(push_integration_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(MUCHOST,                        <<"muclight.@HOST@">>).

-import(muc_light_helper,
    [
        room_bin_jid/1,
        create_room/6
    ]).
-import(escalus_ejabberd, [rpc/3]).
-import(push_helper, [enable_stanza/3, become_unavailable/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, pm_msg_notifications},
        {group, muclight_msg_notifications}
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
          ]}
        ],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    mongoose_push_mock:start(Config0),
    Port = mongoose_push_mock:port(),

    %% Start modules
    Config = dynamic_modules:save_modules(domain(), Config0),
    dynamic_modules:ensure_modules(domain(), required_modules()),

    try
        rpc(mongoose_http_client, start, [[]])
    catch
        error:Reason ->
            ct:pal("pool sup start failed ~p", [Reason]),
            ok
    end,

    rpc(mongoose_http_client, start_pool, [mongoose_push_http, [
        {server, "https://localhost:" ++ integer_to_list(Port)}
    ]]),
    escalus:init_per_suite(Config).


end_per_suite(Config) ->
    escalus_fresh:clean(),
    rpc(mongoose_http_client, stop_pool, [mongoose_push_http]),
    rpc(mongoose_http_client, stop, []),
    dynamic_modules:restore_modules(domain(), Config),
    mongoose_push_mock:stop(),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    %% Some cleaning up
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
            Notification = wait_for_push_request(DeviceToken),

            assert_push_notification(Notification, <<"apns">>, EnableOpts, SenderJID)

        end).

assert_push_notification(Notification, Service, EnableOpts, SenderJID) ->

    ?assertMatch(#{<<"service">> := Service}, Notification),

    Alert = maps:get(<<"alert">>, Notification, undefined),
    Data = maps:get(<<"data">>, Notification, undefined),

    ExpectedBody = proplists:get_value(body, EnableOpts, <<"OH, HAI!">>),

    case proplists:get_value(<<"silent">>, EnableOpts) of
        undefined ->
            ?assertMatch(#{<<"body">> := ExpectedBody}, Alert),
            ?assertMatch(#{<<"title">> := SenderJID}, Alert),
            ?assertMatch(#{<<"badge">> := 1}, Alert),
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
            ?assertMatch(#{<<"message-count">> := 1}, Data)
    end,

    case  proplists:get_value(<<"topic">>, EnableOpts) of
        undefined -> ok;
        Topic ->
            ?assertMatch(Topic, maps:get(<<"topic">>, Notification, undefined))
    end.


pm_msg_notify_on_fcm(Config, EnableOpts) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            {SenderJID, DeviceToken} = pm_conversation(Alice, Bob, <<"fcm">>, EnableOpts),
            Notification = wait_for_push_request(DeviceToken),

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
%% GROUP muclight_msg_notifications
%%--------------------------------------------------------------------


muclight_msg_notify_on_apns(Config, EnableOpts) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            {SenderJID, DeviceToken} =
                muclight_conversation(Config, Alice, Bob, <<"apns">>, EnableOpts),
            Notification = wait_for_push_request(DeviceToken),
            assert_push_notification(Notification, <<"apns">>,
                                     [{body, <<"Heyah!">>} | EnableOpts], SenderJID)

        end).

muclight_msg_notify_on_fcm(Config, EnableOpts) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            {SenderJID, DeviceToken} =
                muclight_conversation(Config, Alice, Bob, <<"fcm">>,EnableOpts),
            Notification = wait_for_push_request(DeviceToken),
            assert_push_notification(Notification, <<"fcm">>,
                                     [{body, <<"Heyah!">>} | EnableOpts], SenderJID)
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
    escalus:assert(is_iq_result, escalus:wait_for_stanza(User)),
    mongoose_push_mock:subscribe(DeviceToken),
    become_unavailable(User),
    DeviceToken.


wait_for_push_request(DeviceToken) ->
    Body = mongoose_push_mock:wait_for_push_request(DeviceToken),
    jiffy:decode(Body, [return_maps]).


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
