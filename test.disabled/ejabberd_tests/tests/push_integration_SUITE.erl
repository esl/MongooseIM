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

-define(PUSH_OPTS,
    [
        {backend, mnesia}
    ]).

-import(muc_light_SUITE,
    [
        room_bin_jid/1,
        create_room/6
    ]).
-import(escalus_ejabberd, [rpc/3]).

-record(route, {from, to, packet}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, pm_msg_notifications}
%%        {group, muclight_msg_notifications}
    ].

groups() ->
    [
        {pm_msg_notifications, [parallel], [
%%            pm_no_msg_notifications_if_not_enabled,
%%            pm_no_msg_notifications_if_user_online,
%%            pm_msg_notify_if_user_offline,
            pm_msg_notify_if_user_offline_with_publish_options
%%            pm_msg_notify_stops_after_disabling
        ]},
        {muclight_msg_notifications, [parallel], [
%%            muclight_no_msg_notifications_if_not_enabled,
%%            muclight_no_msg_notifications_if_user_online,
%%            muclight_msg_notify_if_user_offline,
%%            muclight_msg_notify_if_user_offline_with_publish_options,
            muclight_msg_notify_stops_after_disabling
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
        escalus:init_per_suite([{fcm, FCM}, {apns, APNS} | Config])
    catch
        _:_ ->
            {skip, "MongoosePush is not available"}
    end.


end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(domain(), Config),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config0) ->
    FCM = proplists:get_value(fcm, Config0),
    APNS = proplists:get_value(fcm, Config0),
    {ok, #{status_code := 200}} = shotgun:post(FCM, <<"/reset">>, #{}, <<>>, #{}),
    {ok, #{status_code := 200}} = shotgun:post(APNS, <<"/reset">>, #{}, <<>>, #{}),

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

pm_no_msg_notifications_if_not_enabled(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            become_unavailable(Bob),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            ?assert(not truly(received_route())),
            ok
        end).

pm_no_msg_notifications_if_user_online(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            ?assert(not truly(received_route())),
            ok
        end).

pm_msg_notify_if_user_offline(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = pubsub_jid(Config),

            AliceJID = bare_jid(Alice),
            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            Published = received_route(),
            ?assertMatch(#route{}, Published),
            #route{to = RealPubsubJID, packet = Packet} = Published,
            ?assertMatch(PubsubJID, rpc(jid, to_binary, [RealPubsubJID])),
            Form = exml_query:path(Packet, [{element, <<"pubsub">>},
                                             {element, <<"publish">>},
                                             {element, <<"item">>},
                                             {element, <<"notification">>},
                                             {element, <<"x">>}]),
            Fields = parse_form(Form),
            ?assertMatch(?PUSH_FORM_TYPE, proplists:get_value(<<"FORM_TYPE">>, Fields)),
            ?assertMatch(<<"OH, HAI!">>, proplists:get_value(<<"last-message-body">>, Fields)),
            ?assertMatch(AliceJID,
                         proplists:get_value(<<"last-message-sender">>, Fields)),

            ok
        end).

pm_msg_notify_if_user_offline_with_publish_options(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = <<"pubsub.localhost">>,
            Node = {_, NodeName} = pubsub_node(),
            pubsub_tools:create_node(Bob, Node, [{type, <<"push">>}]),

            escalus:send(Bob, enable_stanza(PubsubJID, NodeName,
                                            [{<<"service">>, <<"apns">>},
                                             {<<"device_id">>, <<"mydevicetoken">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            timer:sleep(10000),
            APNS = proplists:get_value(apns, Config),
            wait_for(timer:seconds(5), fun() ->
                {ok, #{body := Body}} = shotgun:get(APNS, <<"/activity">>),
                #{<<"logs">> := Logs} = jiffy:decode(Body, [return_maps]),
                1 = length(Logs)
            end),



            ok
        end).



pm_msg_notify_stops_after_disabling(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = pubsub_jid(Config),

            %% Enable
            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>, [])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),

            %% Disable
            escalus:send(Bob, disable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            ?assert(not received_route()),

            ok
        end).

%%--------------------------------------------------------------------
%% GROUP muclight_msg_notifications
%%--------------------------------------------------------------------

muclight_no_msg_notifications_if_not_enabled(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            Room = room_name(Config),
            create_room(Room, [bob, alice, kate], Config),
            become_unavailable(Alice),
            become_unavailable(Kate),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),

            escalus:send(Bob, Stanza),

            ?assert(not truly(received_route())),

            ok
        end).

muclight_no_msg_notifications_if_user_online(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            Room = room_name(Config),
            PubsubJID = pubsub_jid(Config),

            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Kate),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            ?assert(not truly(received_route())),
            ok
        end).

muclight_msg_notify_if_user_offline(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            PubsubJID = pubsub_jid(Config),
            Room = room_name(Config),
            BobJID = bare_jid(Bob),

            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            Published = received_route(),
            ?assertMatch(#route{}, Published),
            #route{to = RealPubsubJID, packet = Packet} = Published,
            ?assertMatch(PubsubJID, rpc(jid, to_binary, [RealPubsubJID])),
            Form = exml_query:path(Packet, [{element, <<"pubsub">>},
                                            {element, <<"publish">>},
                                            {element, <<"item">>},
                                            {element, <<"notification">>},
                                            {element, <<"x">>}]),
            Fields = parse_form(Form),
            ?assertMatch(?PUSH_FORM_TYPE, proplists:get_value(<<"FORM_TYPE">>, Fields)),
            ?assertMatch(Msg, proplists:get_value(<<"last-message-body">>, Fields)),
            SenderId = <<(room_bin_jid(Room))/binary, "/" ,BobJID/binary>>,
            ?assertMatch(SenderId,
                         proplists:get_value(<<"last-message-sender">>, Fields)),
            ok
        end).

muclight_msg_notify_if_user_offline_with_publish_options(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            PubsubJID = pubsub_jid(Config),
            Room = room_name(Config),

            create_room(Room, [bob, alice, kate], Config),
            escalus:send(Alice, enable_stanza(PubsubJID, <<"NodeId">>,
                                            [{<<"field1">>, <<"value1">>},
                                             {<<"field2">>, <<"value2">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            Published = received_route(),
            ?assertMatch(#route{}, Published),
            #route{to = RealPubsubJID, packet = Packet} = Published,
            ?assertMatch(PubsubJID, rpc(jid, to_binary, [RealPubsubJID])),
            Form = exml_query:path(Packet, [{element, <<"pubsub">>},
                                            {element, <<"publish-options">>},
                                            {element, <<"x">>}]),
            Fields = parse_form(Form),
            ?assertMatch(?NS_PUBSUB_PUB_OPTIONS, proplists:get_value(<<"FORM_TYPE">>, Fields)),
            ?assertMatch(<<"value1">>, proplists:get_value(<<"field1">>, Fields)),
            ?assertMatch(<<"value2">>, proplists:get_value(<<"field2">>, Fields)),
            ok
        end).

muclight_msg_notify_stops_after_disabling(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, _Kate) ->
            Room = room_name(Config),
            PubsubJID = pubsub_jid(Config),
            create_room(Room, [bob, alice, kate], Config),

            %% Enable
            escalus:send(Alice, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),

            %% Disable
            escalus:send(Alice, disable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            become_unavailable(Alice),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            ?assert(not truly(received_route())),
            ok
        end).


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

%% ----------------------------------
%% Stanzas
%% ----------------------------------

disable_stanza(JID, undefined) ->
    disable_stanza([
        {<<"xmlns">>, <<"urn:xmpp:push:0">>},
        {<<"jid">>, JID}
    ]);
disable_stanza(JID, Node) ->
    disable_stanza([
        {<<"xmlns">>, <<"urn:xmpp:push:0">>},
        {<<"jid">>, JID},
        {<<"node">>, Node}
    ]).
disable_stanza(JID) when is_binary(JID) ->
    disable_stanza(JID, undefined);
disable_stanza(Attrs) when is_list(Attrs) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"disable">>, attrs = Attrs}]).

enable_stanza(JID, Node) ->
    enable_stanza(JID, Node, undefined).
enable_stanza(JID, Node, FormFields) ->
    enable_stanza(JID, Node, FormFields, ?NS_PUBSUB_PUB_OPTIONS).
enable_stanza(JID, Node, FormFields, FormType) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"enable">>, attrs = [
        {<<"xmlns">>, <<"urn:xmpp:push:0">>},
        {<<"jid">>, JID},
        {<<"node">>, Node}
    ], children = maybe_form(FormFields, FormType)}]).

maybe_form(undefined, _FormType) ->
    [];
maybe_form(FormFields, FormType) ->
    [make_form([{<<"FORM_TYPE">>, FormType} | FormFields])].

make_form(Fields) ->
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"submit">>}],
           children = [make_form_field(Name, Value) || {Name, Value} <- Fields]}.

make_form_field(Name, Value) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name}],
           children = [#xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}]}.



%% ----------------------------------
%% Other helpers
%% ----------------------------------

parse_form(#xmlel{name = <<"x">>} = Form) ->
    parse_form(exml_query:subelements(Form, <<"field">>));
parse_form(Fields) when is_list(Fields) ->
    lists:map(
        fun(Field) ->
            {exml_query:attr(Field, <<"var">>),
             exml_query:path(Field, [{element, <<"value">>}, cdata])}
        end, Fields).

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
    ok = wait_for(timer:seconds(20), fun() ->
        is_offline(lower(escalus_client:username(Client)), lower(escalus_client:server(Client)))
    end). %% There is no ACK for unavailable status

wait_for(TimeLeft, _Fun) when TimeLeft < 0 ->
    timeout;
wait_for(TimeLeft, Fun) ->
    Step = 500,
    try
        case Fun() of
            ok -> ok;
            true -> ok;
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
        ]}
    ].
