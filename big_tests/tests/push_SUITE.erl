-module(push_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include("push_helper.hrl").

-import(muc_light_helper,
    [
        room_bin_jid/1,
        create_room/6
    ]).
-import(escalus_ejabberd, [rpc/3]).
-import(push_helper, [
    enable_stanza/2, enable_stanza/3, enable_stanza/4,
    disable_stanza/1, disable_stanza/2, become_unavailable/1
]).

-record(route, {from, to, acc, packet}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
        {group, disco},
        {group, toggling},
        {group, pm_msg_notifications},
        {group, muclight_msg_notifications}
    ].

groups() ->
    G = [
         {disco, [], [
                      push_notifications_listed_disco_when_available,
                      push_notifications_not_listed_disco_when_not_available
                     ]},
         {toggling, [parallel], [
                                 enable_should_fail_with_missing_attributes,
                                 enable_should_fail_with_invalid_attributes,
                                 enable_should_succeed_without_form,
                                 enable_with_form_should_fail_with_incorrect_from,
                                 enable_should_accept_correct_from,
                                 disable_should_fail_with_missing_attributes,
                                 disable_should_fail_with_invalid_attributes,
                                 disable_all,
                                 disable_node
                                ]},
         {pm_msg_notifications, [parallel], [
                                             pm_no_msg_notifications_if_not_enabled,
                                             pm_no_msg_notifications_if_user_online,
                                             pm_msg_notify_if_user_offline,
                                             pm_msg_notify_if_user_offline_with_publish_options,
                                             pm_msg_notify_stops_after_disabling
                                            ]},
         {muclight_msg_notifications, [parallel], [
                                                   muclight_no_msg_notifications_if_not_enabled,
                                                   muclight_no_msg_notifications_if_user_online,
                                                   muclight_msg_notify_if_user_offline,
                                                   muclight_msg_notify_if_user_offline_with_publish_options,
                                                   muclight_msg_notify_stops_after_disabling
                                                  ]}
        ],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    %% For mocking with unnamed functions
    {_Module, Binary, Filename} = code:get_object_code(?MODULE),
    rpc(code, load_binary, [?MODULE, Filename, Binary]),
    escalus:init_per_suite(Config).
end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(disco, Config) ->
    escalus:create_users(Config, escalus:get_users([alice]));
init_per_group(muclight_msg_notifications, Config0) ->
    Host = ct:get_config({hosts, mim, domain}),
    Config = init_per_group(generic_group, Config0),
    dynamic_modules:ensure_modules(Host, [{mod_muc_light,
                                           [{host, binary_to_list(?MUCHOST)},
                                            {backend, mnesia},
                                            {rooms_in_rosters, true}]}]),
    rpc(mod_muc_light_db_backend, force_clear, []),
    Config;
init_per_group(_, Config0) ->
    Config1 = [{push_config, ?PUSH_OPTS} | Config0],
    Host = ct:get_config({hosts, mim, domain}),
    Config = dynamic_modules:save_modules(Host, Config1),
    dynamic_modules:ensure_modules(Host, [{mod_push, ?PUSH_OPTS}]),
    Config.

end_per_group(disco, Config) ->
    escalus:delete_users(Config),
    Config;
end_per_group(_, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:restore_modules(Host, Config),
    Config.

init_per_testcase(CaseName = push_notifications_listed_disco_when_available, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    OldModules = rpc(gen_mod, loaded_modules_with_opts, [Host]),
    rpc(gen_mod_deps, start_modules, [Host, [{mod_push, ?PUSH_OPTS}]]),
    escalus:init_per_testcase(CaseName, [{old_modules, OldModules} | Config]);
init_per_testcase(CaseName = push_notifications_not_listed_disco_when_not_available, Config) ->
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CaseName, Config0) ->
    start_publish_listener(CaseName, Config0),
    Config1 = escalus_fresh:create_users(Config0, [{bob, 1}, {alice, 1}, {kate, 1}]),
    Config = [{case_name, CaseName} | Config1],
    escalus:init_per_testcase(CaseName, Config).


end_per_testcase(CaseName = push_notifications_listed_disco_when_available, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    OldModules = ?config(old_modules, Config),
    CurrentModules = rpc(gen_mod, loaded_modules_with_opts, [Host]),
    rpc(gen_mod_deps, replace_modules, [Host, CurrentModules, OldModules]),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName = push_notifications_not_listed_disco_when_not_available, Config) ->
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) ->
    rpc(ejabberd_router, unregister_route, [atom_to_binary(CaseName, utf8)]),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% GROUP disco
%%--------------------------------------------------------------------

push_notifications_listed_disco_when_available(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Server = escalus_client:server(Alice),
            escalus:send(Alice, escalus_stanza:disco_info(Server)),
            Stanza = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, Stanza),
            escalus:assert(has_feature, [push_helper:ns_push()], Stanza),
            ok
        end).

push_notifications_not_listed_disco_when_not_available(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Server = escalus_client:server(Alice),
            escalus:send(Alice, escalus_stanza:disco_info(Server)),
            Stanza = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, Stanza),
            Pred = fun(Feature, Stanza) -> not escalus_pred:has_feature(Feature, Stanza) end,
            escalus:assert(Pred, [push_helper:ns_push()], Stanza),
            ok
        end).

%%--------------------------------------------------------------------
%% GROUP toggling
%%--------------------------------------------------------------------

enable_should_fail_with_missing_attributes(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            BobJID = escalus_utils:get_jid(Bob),

            escalus:send(Bob, escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"enable">>}])),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            CorrectAttrs = [{<<"xmlns">>, <<"urn:xmpp:push:0">>},
                            {<<"jid">>, BobJID},
                            {<<"node">>, <<"NodeKey">>}],

            %% Sending only one attribute should fail
            lists:foreach(
                fun(Attr) ->
                    escalus:send(Bob, escalus_stanza:iq(<<"set">>,
                                                        [#xmlel{name = <<"enable">>,
                                                                attrs = [Attr]}])),
                    escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                                   escalus:wait_for_stanza(Bob))
                end, CorrectAttrs),

            %% Sending all but one attribute should fail
            lists:foreach(
                fun(Attr) ->
                    escalus:send(Bob, escalus_stanza:iq(<<"set">>,
                                                        [#xmlel{name = <<"enable">>,
                                                                attrs = CorrectAttrs -- [Attr]}])),
                    escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                                   escalus:wait_for_stanza(Bob))
                end, CorrectAttrs),

            ok
        end).

enable_should_fail_with_invalid_attributes(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            %% Empty JID
            escalus:send(Bob, enable_stanza(<<>>, <<"nodeId">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            %% Empty node
            escalus:send(Bob, enable_stanza(PubsubJID, <<>>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            ok
        end).


enable_should_succeed_without_form(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

enable_with_form_should_fail_with_incorrect_from(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>, [], <<"wrong">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            ok
        end).

enable_should_accept_correct_from(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>, [])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>, [
                {<<"secret1">>, <<"token1">>},
                {<<"secret2">>, <<"token2">>}
            ])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

disable_should_fail_with_missing_attributes(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            BobJID = escalus_utils:get_jid(Bob),

            escalus:send(Bob, escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"disable">>}])),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            CorrectAttrs = [{<<"xmlns">>, <<"urn:xmpp:push:0">>}, {<<"jid">>, BobJID}],

            %% Sending only one attribute should fail
            lists:foreach(
                fun(Attr) ->
                    escalus:send(Bob, escalus_stanza:iq(<<"set">>,
                                                        [#xmlel{name = <<"disable">>,
                                                                attrs = [Attr]}])),
                    escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                                   escalus:wait_for_stanza(Bob))
                end, CorrectAttrs),
            ok
        end).

disable_should_fail_with_invalid_attributes(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            %% Empty JID
            escalus:send(Bob, disable_stanza(<<>>, <<"nodeId">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            escalus:send(Bob, disable_stanza(<<>>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            ok
        end).

disable_all(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, disable_stanza(PubsubJID)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

disable_node(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, disable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

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
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

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
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
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
            NS = push_helper:push_form_type(),
            ?assertMatch(NS, proplists:get_value(<<"FORM_TYPE">>, Fields)),
            ?assertMatch(<<"OH, HAI!">>, proplists:get_value(<<"last-message-body">>, Fields)),
            ?assertMatch(AliceJID,
                         proplists:get_value(<<"last-message-sender">>, Fields)),

            ok
        end).

pm_msg_notify_if_user_offline_with_publish_options(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = pubsub_jid(Config),

            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>,
                                            [{<<"field1">>, <<"value1">>},
                                             {<<"field2">>, <<"value2">>}])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
            become_unavailable(Bob),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            Published = received_route(),
            ?assertMatch(#route{}, Published),
            #route{to = RealPubsubJID, packet = Packet} = Published,
            ?assertMatch(PubsubJID, rpc(jid, to_binary, [RealPubsubJID])),
            Form = exml_query:path(Packet, [{element, <<"pubsub">>},
                                            {element, <<"publish-options">>},
                                            {element, <<"x">>}]),
            Fields = parse_form(Form),
            NS = push_helper:ns_pubsub_pub_options(),
            ?assertMatch(NS, proplists:get_value(<<"FORM_TYPE">>, Fields)),
            ?assertMatch(<<"value1">>, proplists:get_value(<<"field1">>, Fields)),
            ?assertMatch(<<"value2">>, proplists:get_value(<<"field2">>, Fields)),
            ok
        end).

pm_msg_notify_stops_after_disabling(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            PubsubJID = pubsub_jid(Config),

            %% Enable
            escalus:send(Bob, enable_stanza(PubsubJID, <<"NodeId">>, [])),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            %% Disable
            escalus:send(Bob, disable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
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
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
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
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
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
            NS = push_helper:push_form_type(),
            ?assertMatch(NS, proplists:get_value(<<"FORM_TYPE">>, Fields)),
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
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
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
            NS = push_helper:ns_pubsub_pub_options(),
            ?assertMatch(NS, proplists:get_value(<<"FORM_TYPE">>, Fields)),
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
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

            %% Disable
            escalus:send(Alice, disable_stanza(PubsubJID, <<"NodeId">>)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
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

parse_form(#xmlel{name = <<"x">>} = Form) ->
    parse_form(exml_query:subelements(Form, <<"field">>));
parse_form(Fields) when is_list(Fields) ->
    lists:map(
        fun(Field) ->
            {exml_query:attr(Field, <<"var">>),
             exml_query:path(Field, [{element, <<"value">>}, cdata])}
        end, Fields).

start_publish_listener(CaseName, Config) ->
    TestCasePid = self(),
    Handler = rpc(mongoose_packet_handler, new, [?MODULE, TestCasePid]),
    rpc(ejabberd_router, register_route, [atom_to_binary(CaseName, utf8), Handler]),

    Config.

process_packet(Acc, From, To, El, TestCasePid) ->
    TestCasePid ! #route{from = From, to = To, acc = Acc, packet = El}.

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

lower(Bin) when is_binary(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).
