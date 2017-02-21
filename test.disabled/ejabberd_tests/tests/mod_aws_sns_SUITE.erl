-module(mod_aws_sns_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(MUC_HOST, <<"muc.localhost">>).
-define(NS_HTTP_UPLOAD, <<"urn:xmpp:http:upload">>).
-define(S3_HOSTNAME, "http://bucket.s3-eu-east-25.example.com").
-define(SNS_OPTS,
    [
        {access_key_id, "AKIAIH54ALYGMZTESTID"},
        {secret_access_key, "buRqHOxXCFUQkiuYgdUAy+XoGKt0Ec6DTESTKEY+"},
        {region, "eu-west-1"},
        {account_id, "123456789012"},
        {sns_host, "sns.eu-west-1.amazonaws.com"},
        {plugin_module, mod_aws_sns_defaults},
        {presence_updates_topic, "user_presence_updated-dev-1"},
        {pm_messages_topic, "user_message_sent-dev-1"},
        {muc_messages_topic, "user_messagegroup_sent-dev-1"},
        {muc_host, "muc.@HOST@"}
    ]).

-record(publish, {
    topic_arn,
    message,
    subject,
    attributes,
    config
}).

-define(assertReceivedMatch(Expect), ?assertReceivedMatch(Expect, timer:seconds(5))).

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
        {group, presence_status_publish},
        {group, message_publish}
    ].

groups() ->
    [
     {presence_status_publish, [], [
         connected_user_changes_status,
         disconnected_user_becomes_unavailable
     ]},
     {message_publish, [], [
         pm_messages,
         muc_messages
     ]}
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    case rpc(application, ensure_all_started, [erlcloud]) of
        {ok, _} ->
            %% For mocking with unnamed functions
            {_Module, Binary, Filename} = code:get_object_code(?MODULE),
            rpc(code, load_binary, [?MODULE, Filename, Binary]),

            muc_helper:load_muc(muc_host()),
            escalus:init_per_suite(Config);
        {error, _} ->
            {skip, "erlcloud dependency is not enabled"}
    end.

end_per_suite(Config) ->
    escalus_fresh:clean(),
    muc_helper:unload_muc(),
    escalus:end_per_suite(Config).

init_per_group(_, Config0) ->
    Config = [{sns_config, ?SNS_OPTS} | Config0],
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:start(Host, mod_aws_sns, ?SNS_OPTS),
    Config.

end_per_group(_, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Host, mod_aws_sns),
    escalus:delete_users(Config, escalus:get_users([bob, alice])).

init_per_testcase(muc_messages = C, Config0) ->
    Config = escalus_fresh:create_users(Config0, [alice, bob]),
    start_publish_listener(Config),
    [User | _] = ?config(escalus_users, Config),
    Config2 = muc_helper:start_room(Config, User, <<"muc_publish">>, <<"user_nick">>,
                                    [{persistent, true},
                                     {anonymous, false}]),
    escalus:init_per_testcase(C, Config2);
init_per_testcase(CaseName, Config0) ->
    Config = escalus_fresh:create_users(Config0, [alice, bob]),
    start_publish_listener(Config),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(muc_messages, Config) ->
    muc_helper:destroy_room(Config),
    rpc(meck, unload, []),
    end_per_testcase(any, Config);
end_per_testcase(CaseName, Config) ->
    rpc(meck, unload, []),
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% GROUP presence_status_publish
%%--------------------------------------------------------------------

connected_user_changes_status(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            Topic = make_topic_arn(Config, presence_updates_topic),
            BobJID = nick_to_jid(bob, Config),
            AliceJID = nick_to_jid(alice, Config),

            %% Available after login
            escalus:wait_for_stanzas(Bob, 1),
            escalus:wait_for_stanzas(Alice, 1),
            ?assertReceivedMatch(#publish{
                topic_arn = Topic, message = #{<<"user_id">> := BobJID, <<"present">> := true}
            }, timer:seconds(5)),

            ?assertReceivedMatch(#publish{
                topic_arn = Topic, message = #{<<"user_id">> := AliceJID, <<"present">> := true}
            }, timer:seconds(5)),

            %% Unavailable after presence change
            escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),
            escalus:send(Alice, escalus_stanza:presence(<<"unavailable">>)),
            ?assertReceivedMatch(#publish{
                topic_arn = Topic, message = #{<<"user_id">> := BobJID, <<"present">> := false}
            }, timer:seconds(5)),
            ?assertReceivedMatch(#publish{
                topic_arn = Topic, message = #{<<"user_id">> := AliceJID, <<"present">> := false}
            }, timer:seconds(5)),

            %% Available after presence change
            escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
            ?assertReceivedMatch(#publish{
                topic_arn = Topic, message = #{<<"user_id">> := BobJID, <<"present">> := true}
            }, timer:seconds(5)),

            ok
        end).

disconnected_user_becomes_unavailable(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(_Bob, _Alice) ->
            %% Presences
            ?assertReceivedMatch(#publish{}),
            ?assertReceivedMatch(#publish{})
        end),

    BobJID = nick_to_jid(bob, Config),
    AliceJID = nick_to_jid(alice, Config),
    ?assertReceivedMatch(#publish{
        message = #{<<"user_id">> := BobJID, <<"present">> := false}
    }, timer:seconds(5)),
    ?assertReceivedMatch(#publish{
        message = #{<<"user_id">> := AliceJID, <<"present">> := false}
    }, timer:seconds(5)).

%%--------------------------------------------------------------------
%% GROUP message_publish
%%--------------------------------------------------------------------

pm_messages(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            Topic = make_topic_arn(Config, pm_messages_topic),
            BobJID = nick_to_jid(bob, Config),
            AliceJID = nick_to_jid(alice, Config),

            %% Presences
            ?assertReceivedMatch(#publish{}),
            ?assertReceivedMatch(#publish{}),

            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
            ?assertReceivedMatch(#publish{
                topic_arn = Topic,
                message = #{<<"from_user_id">> := AliceJID,
                            <<"to_user_id">> := BobJID,
                            <<"message">> := <<"OH, HAI!">>}
            }, timer:seconds(5)),

            escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi there!">>)),
            escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"How are you?">>)),
            ?assertReceivedMatch(#publish{
                topic_arn = Topic,
                message = #{<<"from_user_id">> := BobJID,
                            <<"to_user_id">> := AliceJID,
                            <<"message">> := <<"Hi there!">>}
            }, timer:seconds(5)),
            ?assertReceivedMatch(#publish{
                topic_arn = Topic,
                message = #{<<"from_user_id">> := BobJID,
                            <<"to_user_id">> := AliceJID,
                            <<"message">> := <<"How are you?">>}
            }, timer:seconds(5)),

            ok
        end).

muc_messages(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            Topic = make_topic_arn(Config, muc_messages_topic),
            Room = ?config(room, Config),
            RoomAddr = room_address(Room),
            BobJID = nick_to_jid(bob, Config),

            %% Presences
            ?assertReceivedMatch(#publish{}),
            ?assertReceivedMatch(#publish{}),

            escalus:send(Alice, stanza_muc_enter_room(Room, nick(Alice))),
            escalus:send(Bob, stanza_muc_enter_room(Room, nick(Bob))),

            escalus:send(Bob, escalus_stanza:groupchat_to(RoomAddr, <<"Hi there!">>)),

            %% 2x presence, topic and message
            escalus:wait_for_stanzas(Bob, 4),
            escalus:wait_for_stanzas(Alice, 4),

            ?assertReceivedMatch(#publish{
                topic_arn = Topic,
                message = #{<<"from_user_id">> := BobJID,
                            <<"to_user_id">> := RoomAddr,
                            <<"message">> := <<"Hi there!">>}
            }, timer:seconds(5)),

            ok
        end).


%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

%% @doc Forwards all erlcloud_sns:publish calls to local PID as messages
start_publish_listener(Config) ->
    TestCasePid = self(),
    ok = rpc(meck, new, [erlcloud_sns, [no_link, passthrough]]),
    ok = rpc(meck, expect,
        [erlcloud_sns, publish,
         fun(topic, RecipientArn, Message, Subject, Attributes, AWSConfig) ->
             TestCasePid ! #publish{topic_arn = RecipientArn,
                                    message = jiffy:decode(Message, [return_maps]),
                                    subject = Subject,
                                    attributes = Attributes,
                                    config = AWSConfig},
             uuid:uuid_to_string(uuid:get_v4())
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
