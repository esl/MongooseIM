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
        {group, toggling}
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

%%disconnected_user_becomes_unavailable(Config) ->
%%    escalus:story(
%%        Config, [{bob, 1}, {alice, 1}],
%%        fun(_Bob, _Alice) ->
%%            %% Presences
%%            ?assertReceivedMatch(#publish{}),
%%            ?assertReceivedMatch(#publish{})
%%        end),
%%
%%    BobJID = nick_to_jid(bob, Config),
%%    AliceJID = nick_to_jid(alice, Config),
%%    ?assertReceivedMatch(#publish{
%%        message = #{<<"user_id">> := BobJID, <<"present">> := false}
%%    }, timer:seconds(5)),
%%    ?assertReceivedMatch(#publish{
%%        message = #{<<"user_id">> := AliceJID, <<"present">> := false}
%%    }, timer:seconds(5)).
%%
%%%%--------------------------------------------------------------------
%%%% GROUP message_publish
%%%%--------------------------------------------------------------------
%%
%%pm_messages(Config) ->
%%    escalus:story(
%%        Config, [{bob, 1}, {alice, 1}],
%%        fun(Bob, Alice) ->
%%            Topic = make_topic_arn(Config, pm_messages_topic),
%%            BobJID = nick_to_jid(bob, Config),
%%            AliceJID = nick_to_jid(alice, Config),
%%
%%            %% Presences
%%            ?assertReceivedMatch(#publish{}),
%%            ?assertReceivedMatch(#publish{}),
%%
%%            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
%%            ?assertReceivedMatch(#publish{
%%                topic_arn = Topic,
%%                message = #{<<"from_user_id">> := AliceJID,
%%                            <<"to_user_id">> := BobJID,
%%                            <<"message">> := <<"OH, HAI!">>}
%%            }, timer:seconds(5)),
%%
%%            escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi there!">>)),
%%            escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"How are you?">>)),
%%            ?assertReceivedMatch(#publish{
%%                topic_arn = Topic,
%%                message = #{<<"from_user_id">> := BobJID,
%%                            <<"to_user_id">> := AliceJID,
%%                            <<"message">> := <<"Hi there!">>}
%%            }, timer:seconds(5)),
%%            ?assertReceivedMatch(#publish{
%%                topic_arn = Topic,
%%                message = #{<<"from_user_id">> := BobJID,
%%                            <<"to_user_id">> := AliceJID,
%%                            <<"message">> := <<"How are you?">>}
%%            }, timer:seconds(5)),
%%
%%            ok
%%        end).
%%
%%muc_messages(Config) ->
%%    escalus:story(
%%        Config, [{bob, 1}, {alice, 1}],
%%        fun(Bob, Alice) ->
%%            Topic = make_topic_arn(Config, muc_messages_topic),
%%            Room = ?config(room, Config),
%%            RoomAddr = room_address(Room),
%%            BobJID = nick_to_jid(bob, Config),
%%
%%            %% Presences
%%            ?assertReceivedMatch(#publish{}),
%%            ?assertReceivedMatch(#publish{}),
%%
%%            escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
%%            escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),
%%
%%            escalus:send(Bob, escalus_stanza:groupchat_to(RoomAddr, <<"Hi there!">>)),
%%
%%            %% 2x presence, topic and message
%%            escalus:wait_for_stanzas(Bob, 4),
%%            escalus:wait_for_stanzas(Alice, 4),
%%
%%            ?assertReceivedMatch(#publish{
%%                topic_arn = Topic,
%%                message = #{<<"from_user_id">> := BobJID,
%%                            <<"to_user_id">> := RoomAddr,
%%                            <<"message">> := <<"Hi there!">>}
%%            }, timer:seconds(5)),
%%
%%            ok
%%        end).


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

%% @doc Forwards all erlcloud_sns:publish calls to local PID as messages
start_publish_listener(Config) ->
    TestCasePid = self(),
%%    rpc(meck, new, [erlcloud_sns, [no_link, passthrough]]),
%%    rpc(meck, expect,
%%        [erlcloud_sns, publish,
%%         fun(topic, RecipientArn, Message, Subject, Attributes, AWSConfig) ->
%%             TestCasePid ! #publish{topic_arn = RecipientArn,
%%                                    message = jiffy:decode(Message, [return_maps]),
%%                                    subject = Subject,
%%                                    attributes = Attributes,
%%                                    config = AWSConfig},
%%             uuid:uuid_to_string(uuid:get_v4())
%%         end]),
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
