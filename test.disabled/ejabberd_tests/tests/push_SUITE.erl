-module(push_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(NS_XDATA,        <<"jabber:x:data">>).
-define(NS_PUBSUB_PUB_OPTIONS, <<"http://jabber.org/protocol/pubsub#publish-options">>).

-define(MUC_HOST, <<"muc.localhost">>).
-define(NS_HTTP_UPLOAD, <<"urn:xmpp:http:upload">>).
-define(S3_HOSTNAME, "http://bucket.s3-eu-east-25.example.com").
-define(PUSH_OPTS,
    [
        {backend, mnesia}
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
%%        {group, toggling},
        {group, pm_msg_notifications}
    ].

groups() ->
    [
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
            pm_msg_notify_if_user_offline_with_publish_options
        ]},
        {muclight_msg_notifications, [], [
            muclight_no_msg_notifications_if_not_enabled,
            muclight_no_msg_notifications_if_user_online,
            muclight_msg_notify_if_user_offline,
            muclight_msg_notify_if_user_offline_with_publish_options
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

    muc_helper:load_muc(muc_host()),
    escalus:init_per_suite(Config).
end_per_suite(Config) ->
    muc_helper:unload_muc(),
    escalus:end_per_suite(Config).

init_per_group(_, Config0) ->
    Config = [{push_config, ?PUSH_OPTS} | Config0],
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:start(Host, mod_push, ?PUSH_OPTS),
    escalus:create_users(Config, escalus:get_users([bob, alice])).

end_per_group(_, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Host, mod_push),
    escalus:delete_users(Config, escalus:get_users([bob, alice])).

init_per_testcase(muc_messages = C, Config) ->
    start_publish_listener(Config),
    [User | _] = ?config(escalus_users, Config),
    Config2 = muc_helper:start_room(Config, User, <<"muc_publish">>, <<"user_nick">>,
                                    [{persistent, true},
                                     {anonymous, false}]),
    escalus:init_per_testcase(C, Config2);
init_per_testcase(CaseName, Config) ->
    start_publish_listener(Config),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(muc_messages, Config) ->
    muc_helper:destroy_room(Config),
    escalus:delete_users(Config),
    end_per_testcase(any, Config);
end_per_testcase(CaseName, Config) ->
    rpc(meck, unload, []),
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% GROUP presence_status_publish
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

pm_no_msg_notifications_if_not_enabled(Config) ->
    escalus_fresh:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            Published =
                receive
                    #route{} -> true
                after timer:seconds(5) ->
                    false
                end,
            ?assert(not Published),
            ok
        end).

pm_no_msg_notifications_if_user_online(Config) ->
    escalus_fresh:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>)),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            Published =
                receive
                    #route{} -> true
                after timer:seconds(5) ->
                    false
                end,
            ?assert(not Published),
            ok
        end).

pm_msg_notify_if_user_offline(Config) ->
    escalus_fresh:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            AliceJID = escalus_client:full_jid(Alice),
            escalus:send(Bob, enable_stanza(<<"pubsub@localhost">>, <<"NodeId">>, [])),
            escalus:assert(is_result, escalus:wait_for_stanza(Bob)),
            escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            Published =
                receive
                    #route{} = R ->
                        R
                after timer:seconds(5) ->
                    timeout
                end,
            ?assertMatch(#route{}, Published),
            #route{to = PubsubJID, packet = Packet} = Published,
            ?assertMatch(<<"pubsub@localhost">>, rpc(jid, to_binary, [PubsubJID])),
            Form = exml_query:path(Packet, [{element, <<"pubsub">>},
                                             {element, <<"publish">>},
                                             {element, <<"item">>},
                                             {element, <<"notification">>},
                                             {element, <<"x">>}]),
            Fields = parse_form(Form),
            ?assertMatch(<<"OH, HAI!">>, proplists:get_value(<<"last-message-body">>, Fields)),
            ?assertMatch(AliceJID,
                         proplists:get_value(<<"last-message-sender">>, Fields)),

            ok
        end).

pm_msg_notify_if_user_offline_with_publish_options(Config) ->
    escalus_fresh:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            ?assert(false),
            ok
        end).


muclight_no_msg_notifications_if_not_enabled(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            ?assert(false),
            ok
        end).

muclight_no_msg_notifications_if_user_online(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            ?assert(false),
            ok
        end).

muclight_msg_notify_if_user_offline(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            ?assert(false),
            ok
        end).

muclight_msg_notify_if_user_offline_with_publish_options(Config) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            ?assert(false),
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

make_topic_arn(Config, TopicVar) ->
    SNSConfig = proplists:get_value(sns_config, Config),
    string:join(["arn", "aws", "sns",
                 proplists:get_value(region, SNSConfig),
                 proplists:get_value(account_id, SNSConfig),
                 proplists:get_value(TopicVar, SNSConfig)], ":").

%% @doc Get a binary jid of the user, that tagged with `UserName' in the config.
nick_to_jid(UserName, Config) when is_atom(UserName) ->
    UserSpec = escalus_users:get_userspec(Config, UserName),
    escalus_utils:jid_to_lower(escalus_users:get_jid(Config, UserSpec)).

stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence(<<"available">>,
                                [#xmlel{name = <<"x">>,
                                        attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}
                                ]),
        Room, Nick).

stanza_default_muc_room(Room, Nick) ->
    Form = escalus_stanza:x_data_form(<<"submit">>, []),
    Query = escalus_stanza:query_el(?NS_MUC_OWNER, [Form]),
    IQSet = escalus_stanza:iq(<<"set">>, [Query]),
    stanza_to_room(IQSet, Room, Nick).

stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

room_address(Room) ->
    <<Room/binary, "@", ?MUC_HOST/binary>>.

room_address(Room, Nick) ->
    <<Room/binary, "@", ?MUC_HOST/binary, "/", Nick/binary>>.

nick(User) -> escalus_utils:get_username(User).

muc_host() ->
    ?MUC_HOST.
