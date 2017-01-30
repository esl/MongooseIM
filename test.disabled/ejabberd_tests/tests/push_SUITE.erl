-module(push_SUITE).
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
-define(MUCHOST,                <<"muclight.localhost">>).

-define(MUC_HOST,               <<"muc.localhost">>).
-define(NS_HTTP_UPLOAD,         <<"urn:xmpp:http:upload">>).
-define(PUSH_OPTS,
    [
        {backend, mnesia}
    ]).

-import(muc_light_SUITE,
    [
        stanza_create_room/3,
        verify_aff_bcast/2,
        room_bin_jid/1,
        gc_message_verify_fun/3
    ]).

-record(route, {from, to, packet}).

-define(assertReceivedMatch(Expect), ?assertReceivedMatch(Expect, 0)).

-define(assertReceivedMatch(Expect, Timeout),
    ((fun() ->
        receive
            Expect = __Result__ -> __Result__
        after
            Timeout ->
                __Reason__ =
                receive
                    __Result__ -> __Result__
                after
                    0 -> timeout
                end,

                __Args__ = [{module, ?MODULE},
                            {line, ?LINE},
                            {expected, (??Expect)},
                            {value, __Reason__}],
                ct:print("assertReceivedMatch_failed: ~p~n", [__Args__]),
                erlang:error({assertReceivedMatch_failed, __Args__})
        end
      end)())).


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
    [
        {disco, [], [
            push_notifications_listed_disco_when_available,
            push_notifications_not_listed_disco_when_not_available
        ]},
        {toggling, [], [
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
        {pm_msg_notifications, [], [
            pm_no_msg_notifications_if_not_enabled,
            pm_no_msg_notifications_if_user_online,
            pm_msg_notify_if_user_offline,
            pm_msg_notify_if_user_offline_with_publish_options,
            pm_msg_notify_stops_after_disabling
        ]},
        {muclight_msg_notifications, [], [
            muclight_no_msg_notifications_if_not_enabled,
            muclight_no_msg_notifications_if_user_online,
            muclight_msg_notify_if_user_offline,
            muclight_msg_notify_if_user_offline_with_publish_options,
            muclight_msg_notify_stops_after_disabling
        ]}
    ].

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
    escalus:end_per_suite(Config).

init_per_group(disco, Config) ->
    escalus:create_users(Config, escalus:get_users([alice]));
init_per_group(muclight_msg_notifications, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:start(Host, mod_push, ?PUSH_OPTS),
    dynamic_modules:start(<<"localhost">>, mod_muc_light,
                          [{host, binary_to_list(?MUCHOST)},
                           {backend, mnesia},
                           {rooms_in_rosters, true}]),
    Config;
init_per_group(_, Config0) ->
    Config = [{push_config, ?PUSH_OPTS} | Config0],
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:start(Host, mod_push, ?PUSH_OPTS),
    escalus:create_users(Config, escalus:get_users([bob, alice])).

end_per_group(disco, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice]));
end_per_group(muclight_msg_notifications, _Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Host, mod_push),
    dynamic_modules:stop(Host, mod_muc_light);
end_per_group(_, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Host, mod_push),
    escalus:delete_users(Config, escalus:get_users([bob, alice])).

init_per_testcase(CaseName = push_notifications_listed_disco_when_available, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:start(Host, mod_push, ?PUSH_OPTS),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CaseName = push_notifications_not_listed_disco_when_not_available, Config) ->
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CaseName, Config) ->
    start_publish_listener(Config),
    rpc(mod_muc_light_db_backend, force_clear, []),
    escalus:init_per_testcase(CaseName, Config).


end_per_testcase(CaseName = push_notifications_listed_disco_when_available, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Host, mod_push),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName = push_notifications_not_listed_disco_when_not_available, Config) ->
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) ->
    rpc(meck, unload, []),
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
            try
                escalus:assert(is_iq_result, Stanza),
                escalus:assert(has_feature, [?NS_PUSH], Stanza),
                ok
            catch Class:Reason ->
                Stacktrace = erlang:get_stacktrace(),
                ct:pal("Stanza ~p.", [Stanza]),
                erlang:raise(Class, Reason, Stacktrace)
            end
        end).

push_notifications_not_listed_disco_when_not_available(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(Alice) ->
            Server = escalus_client:server(Alice),
            escalus:send(Alice, escalus_stanza:disco_info(Server)),
            Stanza = escalus:wait_for_stanza(Alice),
            try
                escalus:assert(is_iq_result, Stanza),
                Pred = fun(Feature, Stanza) -> not escalus_pred:has_feature(Feature, Stanza) end,
                escalus:assert(Pred, [?NS_PUSH], Stanza),
                ok
            catch Class:Reason ->
                Stacktrace = erlang:get_stacktrace(),
                ct:pal("Stanza ~p.", [Stanza]),
                erlang:raise(Class, Reason, Stacktrace)
            end
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
            %% Empty JID
            escalus:send(Bob, enable_stanza(<<>>, <<"nodeId">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            %% Invalid JID
            escalus:send(Bob, enable_stanza(<<"this_is_not_a_valid_jid">>, <<"nodeId">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            %% Empty node
            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<>>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            ok
        end).


enable_should_succeed_without_form(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

enable_with_form_should_fail_with_incorrect_from(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>, [], <<"wrong">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            ok
        end).

enable_should_accept_correct_from(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>, [])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),

            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>, [
                {<<"secret1">>, <<"token1">>},
                {<<"secret2">>, <<"token2">>}
            ])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),

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

            %% Invalid JID
            escalus:send(Bob, disable_stanza(<<"this_is_not_a_valid_jid">>, <<"nodeId">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            escalus:send(Bob, disable_stanza(<<"this_is_not_a_valid_jid">>)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),
            ok
        end).

disable_all(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            escalus:send(Bob, disable_stanza(<<"pubsub@localhost">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

disable_node(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            escalus:send(Bob, disable_stanza(<<"pubsub@localhost">>, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),

            ok
        end).

%%--------------------------------------------------------------------
%% GROUP pm_msg_notifications
%%--------------------------------------------------------------------

pm_no_msg_notifications_if_not_enabled(Config) ->
    escalus_fresh:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            ?assert(not truly(received_route())),
            ok
        end).

pm_no_msg_notifications_if_user_online(Config) ->
    escalus_fresh:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            ?assert(not truly(received_route())),
            ok
        end).

pm_msg_notify_if_user_offline(Config) ->
    escalus_fresh:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            AliceJID = bare_jid(Alice),
            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            Published = received_route(),
            ?assertMatch(#route{}, Published),
            #route{to = PubsubJID, packet = Packet} = Published,
            ?assertMatch(<<"pubsub@localhost">>, rpc(jid, to_binary, [PubsubJID])),
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
    escalus_fresh:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>,
                                            [{<<"field1">>, <<"value1">>},
                                             {<<"field2">>, <<"value2">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            Published = received_route(),
            ?assertMatch(#route{}, Published),
            #route{to = PubsubJID, packet = Packet} = Published,
            ?assertMatch(<<"pubsub@localhost">>, rpc(jid, to_binary, [PubsubJID])),
            Form = exml_query:path(Packet, [{element, <<"pubsub">>},
                                            {element, <<"publish-options">>},
                                            {element, <<"x">>}]),
            Fields = parse_form(Form),
            ?assertMatch(?NS_PUBSUB_PUB_OPTIONS, proplists:get_value(<<"FORM_TYPE">>, Fields)),
            ?assertMatch(<<"value1">>, proplists:get_value(<<"field1">>, Fields)),
            ?assertMatch(<<"value2">>, proplists:get_value(<<"field2">>, Fields)),
            ok
        end).

pm_msg_notify_stops_after_disabling(Config) ->
    escalus_fresh:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            %% Enable
            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>, [])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),

            %% Disable
            escalus:send(Bob, disable_stanza(<<"pubsub@localhost">>, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            ?assert(not received_route()),

            ok
        end).

%%--------------------------------------------------------------------
%% GROUP muclight_msg_notifications
%%--------------------------------------------------------------------

muclight_no_msg_notifications_if_not_enabled(Config) ->
    escalus_fresh:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            Room = <<"bobroom">>,
            create_room(Room, [Bob, Alice, Kate]),
            escalus:send(Alice, escalus_stanza:presence(<<"unavailable">>)),
            escalus:send(Kate, escalus_stanza:presence(<<"unavailable">>)),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),

            escalus:send(Bob, Stanza),

            ?assert(not truly(received_route())),

            ok
        end).

muclight_no_msg_notifications_if_user_online(Config) ->
    escalus_fresh:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            Room = <<"bobroom">>,
            create_room(Room, [Bob, Alice, Kate]),
            escalus:send(Alice, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            escalus:send(Kate, escalus_stanza:presence(<<"unavailable">>)),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            ?assert(not truly(received_route())),
            ok
        end).

muclight_msg_notify_if_user_offline(Config) ->
    escalus_fresh:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            Room = <<"bobroom">>,
            BobJID = bare_jid(Bob),
            create_room(Room, [Bob, Alice, Kate]),
            escalus:send(Alice, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            escalus:send(Alice, escalus_stanza:presence(<<"unavailable">>)),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            Published = received_route(),
            ?assertMatch(#route{}, Published),
            #route{to = PubsubJID, packet = Packet} = Published,
            ?assertMatch(<<"pubsub@localhost">>, rpc(jid, to_binary, [PubsubJID])),
            Form = exml_query:path(Packet, [{element, <<"pubsub">>},
                                            {element, <<"publish">>},
                                            {element, <<"item">>},
                                            {element, <<"notification">>},
                                            {element, <<"x">>}]),
            Fields = parse_form(Form),
            ?assertMatch(?PUSH_FORM_TYPE, proplists:get_value(<<"FORM_TYPE">>, Fields)),
            ?assertMatch(Msg, proplists:get_value(<<"last-message-body">>, Fields)),
            ?assertMatch(BobJID,
                         proplists:get_value(<<"last-message-sender">>, Fields)),
            ok
        end).

muclight_msg_notify_if_user_offline_with_publish_options(Config) ->
    escalus_fresh:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            Room = <<"bobroom">>,
            create_room(Room, [Bob, Alice, Kate]),
            escalus:send(Alice, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>,
                                            [{<<"field1">>, <<"value1">>},
                                             {<<"field2">>, <<"value2">>}])),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            escalus:send(Alice, escalus_stanza:presence(<<"unavailable">>)),

            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(Room), Msg),
            escalus:send(Bob, Stanza),

            Published = received_route(),
            ?assertMatch(#route{}, Published),
            #route{to = PubsubJID, packet = Packet} = Published,
            ?assertMatch(<<"pubsub@localhost">>, rpc(jid, to_binary, [PubsubJID])),
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
    escalus_fresh:story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            Room = <<"bobroom">>,
            create_room(Room, [Bob, Alice, Kate]),

            %% Enable
            escalus:send(Alice, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),
            escalus:send(Alice, escalus_stanza:presence(<<"unavailable">>)),

            %% Disable
            escalus:send(Alice, disable_stanza(<<"pubsub@localhost">>, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Alice)),

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
           children = [#xmlcdata{content = Value}]}.



%% ----------------------------------
%% Other helpers
%% ----------------------------------

parse_form(#xmlel{name = <<"x">>} = Form) ->
    parse_form(exml_query:subelements(Form, <<"field">>));
parse_form(Fields) when is_list(Fields) ->
    lists:map(
        fun(Field) ->
            {exml_query:attr(Field, <<"var">>), exml_query:cdata(Field)}
        end, Fields).

%% @doc Forwards all erlcloud_sns:publish calls to local PID as messages
start_publish_listener(Config) ->
    TestCasePid = self(),
    rpc(meck, new, [ejabberd_router, [no_link, passthrough]]),
    rpc(meck, expect,
        [ejabberd_router, route,
         fun(From, To, Packet) ->
             case exml_query:subelement(Packet, <<"pubsub">>) of
                 undefined ->
                     meck:passthrough([From, To, Packet]);
                 _ ->
                     TestCasePid ! #route{from = From, to = To, packet = Packet},
                     ok
             end
         end]),
    Config.

-spec rpc(M :: atom(), F :: atom(), A :: [term()]) -> term().
rpc(M, F, A) ->
    Node = ct:get_config({hosts, mim, node}),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    escalus_ct:rpc_call(Node, M, F, A, 10000, Cookie).

create_room(Room, [Owner | Members]) ->
    InitOccupants = [{Member, member} || Member <- Members],
    FinalOccupants = [{Owner, owner} | InitOccupants],
    InitConfig = [{<<"roomname">>, <<"Bob's room">>}],
    escalus:send(Owner, stanza_create_room(Room, InitConfig, InitOccupants)),
    verify_aff_bcast(FinalOccupants, FinalOccupants),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Owner)).

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