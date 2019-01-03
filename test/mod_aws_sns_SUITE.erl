-module(mod_aws_sns_SUITE).

-compile([export_all]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mod_event_pusher_events.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_SNS_CONFIG, [
                             {access_key_id, "AKIAJAZYHOIPY6A2PESA"},
                             {secret_access_key, "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"},
                             {region, "eu-west-1"},
                             {account_id, "251423380551"},
                             {sns_host, "sns.eu-west-1.amazonaws.com"},
                             {plugin_module, mod_event_pusher_sns_defaults},
                             {presence_updates_topic, "user_presence_updated-dev-1"},
                             {pm_messages_topic, "user_message_sent-dev-1"},
                             {muc_messages_topic, "user_messagegroup_sent-dev-1"}
                            ]).

all() ->
    [
     handles_unicode_messages,
     forwards_chat_messages_to_chat_topic,
     forwards_groupchat_messages_to_groupchat_topic,
     does_not_forward_other_messages,
     creates_proper_sns_topic_arn,
     forwards_online_presence_to_presence_topic,
     forwards_offline_presence_to_presence_topic,
     does_not_forward_messages_without_body,
     does_not_forward_messages_when_topic_is_unset,
     does_not_forward_presences_when_topic_is_unset,
     calls_callback_module_to_get_user_id,
     calls_callback_module_to_retrieve_attributes_for_presence,
     calls_callback_module_to_retrieve_attributes_for_message
    ].

%% Tests

handles_unicode_messages(Config) ->
    expect_message_entry(message, <<"❤☀☆☂☻♞☯☭☢€"/utf8>>),
    send_packet_callback(Config, <<"chat">>, <<"❤☀☆☂☻♞☯☭☢€"/utf8>>).

forwards_chat_messages_to_chat_topic(Config) ->
    expect_topic("user_message_sent-dev-1"),
    send_packet_callback(Config, <<"chat">>, <<"message">>).

forwards_groupchat_messages_to_groupchat_topic(Config) ->
    expect_topic("user_messagegroup_sent-dev-1"),
    send_packet_callback(Config, <<"groupchat">>, <<"message">>).

does_not_forward_other_messages(Config) ->
    meck:expect(erlcloud_sns, publish, fun(_, _, _, _, _, _) -> ok end),
    send_packet_callback(Config, <<"othertype">>, <<"message">>),
    ?assertNot(meck:called(erlcloud_sns, publish, '_')).

creates_proper_sns_topic_arn(Config) ->
    meck:expect(erlcloud_sns, publish, fun(_, _, _, _, _, _) -> ok end),
    ExpectedTopic = craft_arn("user_message_sent-dev-1"),
    send_packet_callback(Config, <<"chat">>, <<"message">>),
    ?assert(meck:called(erlcloud_sns, publish, [topic, ExpectedTopic, '_', '_', '_', '_'])),

    set_sns_config(#{pm_messages_topic => "othertopicname", account_id =>
                         "otheraccountid", region => "otherregion"}),
    OtherExpectedTopic = craft_arn("otherregion", "otheraccountid", "othertopicname"),
    send_packet_callback(Config, <<"chat">>, <<"message">>),
    ?assert(meck:called(erlcloud_sns, publish, [topic, OtherExpectedTopic, '_', '_', '_', '_'])).

forwards_online_presence_to_presence_topic(Config) ->
    expect_message_entry(present, true),
    user_present_callback(Config),
    ExpectedTopic = craft_arn("user_presence_updated-dev-1"),
    ?assert(meck:called(erlcloud_sns, publish, [topic, ExpectedTopic, '_', '_', '_', '_'])).

forwards_offline_presence_to_presence_topic(Config) ->
    expect_message_entry(present, false),
    user_not_present_callback(Config),
    ExpectedTopic = craft_arn("user_presence_updated-dev-1"),
    ?assert(meck:called(erlcloud_sns, publish, [topic, ExpectedTopic, '_', '_', '_', '_'])).

does_not_forward_messages_without_body(Config) ->
    meck:expect(erlcloud_sns, publish, fun(_, _, _, _, _, _) -> ok end),
    send_packet_callback(Config, <<"chat">>, undefined),
    ?assertNot(meck:called(erlcloud_sns, publish, '_')).

does_not_forward_messages_when_topic_is_unset(Config) ->
    set_sns_config(#{pm_messages_topic => undefined}),
    meck:expect(erlcloud_sns, publish, fun(_, _, _, _, _, _) -> ok end),
    send_packet_callback(Config, <<"chat">>, <<"message">>),
    ?assertNot(meck:called(erlcloud_sns, publish, '_')).

does_not_forward_presences_when_topic_is_unset(Config) ->
    set_sns_config(#{presence_updates_topic => undefined}),
    meck:expect(erlcloud_sns, publish, fun(_, _, _, _, _, _) -> ok end),
    user_not_present_callback(Config),
    ?assertNot(meck:called(erlcloud_sns, publish, '_')).

calls_callback_module_to_get_user_id(Config) ->
    Module = custom_callback_module("customuserid", #{}),
    set_sns_config(#{plugin_module => Module}),
    expect_message_entry(user_id, "customuserid"),
    user_not_present_callback(Config),
    ?assert(meck:called(Module, user_guid, '_')).

calls_callback_module_to_retrieve_attributes_for_presence(Config) ->
    Module = custom_callback_module("", #{"a" => 123}),
    set_sns_config(#{plugin_module => Module}),
    expect_attributes("a", 123),
    user_not_present_callback(Config),
    ?assert(meck:called(Module, message_attributes, ['_', '_', '_'])).

calls_callback_module_to_retrieve_attributes_for_message(Config) ->
    Module = custom_callback_module("", #{"b" => "abc"}),
    set_sns_config(#{plugin_module => Module}),
    expect_attributes("b", "abc"),
    send_packet_callback(Config, <<"groupchat">>, <<"message">>),
    ?assert(meck:called(Module, message_attributes, ['_', '_', '_', '_', '_'])).

%% Fixtures

init_per_suite(Config) ->
    stringprep:start(),
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    meck:new([gen_mod, erlcloud_sns, mongoose_wpool], [non_strict, passthrough]),
    meck:expect(erlcloud_sns, new, fun(_, _, _) -> mod_aws_sns_SUITE_erlcloud_sns_new end),
    set_sns_config(#{}),
    meck:expect(mongoose_wpool, cast, fun(_, _, _, {M, F, A}) -> erlang:apply(M, F, A) end),
    [{sender, jid:from_binary(<<"sender@localhost">>)},
     {recipient, jid:from_binary(<<"recipient@localhost">>)} |
     Config].

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

%% Wrapped callbacks

send_packet_callback(Config, Type, Body) ->
    Packet = message(Config, Type, Body),
    Sender = #jid{lserver = Host} = ?config(sender, Config),
    Recipient = ?config(recipient, Config),
    mod_event_pusher_sns:push_event(mongoose_acc:new(#{ location => ?LOCATION,
                                                        lserver => Host,
                                                        element => undefined }), Host,
                                    #chat_event{type = chat, direction = in,
                                                from = Sender, to = Recipient,
                                                packet = Packet}).

user_present_callback(Config) ->
    Jid = #jid{lserver = Host} = ?config(sender, Config),
    mod_event_pusher_sns:push_event(mongoose_acc:new(#{ location => ?LOCATION,
                                                        lserver => Host,
                                                        element => undefined }), Host,
                                    #user_status_event{jid = Jid, status = online}).

user_not_present_callback(Config) ->
    Jid = #jid{lserver = Host} = ?config(sender, Config),
    mod_event_pusher_sns:push_event(mongoose_acc:new(#{ location => ?LOCATION,
                                                        lserver => Host,
                                                        element => undefined }), Host,
                                    #user_status_event{jid = Jid, status = offline}).

%% Helpers

custom_callback_module(UserId, Attributes) ->
    Module = mod_aws_sns_SUITE_mockcb,
    set_sns_config(#{plugin_module => Module}),
    meck:new(Module, [non_strict]),
    meck:expect(Module, user_guid, fun(_) -> UserId end),
    meck:expect(Module, message_attributes, fun(_, _, _) -> Attributes end),
    meck:expect(Module, message_attributes, fun(_, _, _, _, _) -> Attributes end),
    Module.

craft_arn(Topic) ->
    craft_arn(proplists:get_value(region, ?DEFAULT_SNS_CONFIG),
              proplists:get_value(account_id, ?DEFAULT_SNS_CONFIG), Topic).

craft_arn(Region, UserId, Topic) ->
    "arn:aws:sns:" ++ Region ++ ":" ++ UserId ++ ":" ++ Topic.

expect_topic(ExpectedTopic) ->
    meck:expect(erlcloud_sns, publish,
                fun(topic, Topic, _, _, _, _) -> true = lists:suffix(ExpectedTopic, Topic) end).

expect_message_entry(Key, Value) ->
    meck:expect(
      erlcloud_sns, publish,
      fun(_, _, TupleList, _, _, _) ->
              MessageObject = maps:from_list(TupleList),
              Value = maps:get(Key, MessageObject)
      end).

expect_attributes(Key, Value) ->
    meck:expect(
      erlcloud_sns, publish,
      fun(_, _, _, _, Attrs, _) -> Value = proplists:get_value(Key, Attrs) end).

set_sns_config(Overrides) ->
    meck:expect(gen_mod, get_module_opt,
                fun(_, _, Key, _) ->
                        maps:get(Key, Overrides, proplists:get_value(Key, ?DEFAULT_SNS_CONFIG))
                end).

message(Config, Type, Body) ->
    message(?config(sender, Config), ?config(recipient, Config), Type, Body).

message(From, Recipient, Type, Body) ->
    Children =
        case Body of
            undefined -> [];
            _ -> [#xmlel{name = <<"body">>, children = [#xmlcdata{content = Body}]}]
        end,
    #xmlel{name = <<"message">>,
           attrs = [{<<"from">>, jid:to_binary(From)}, {<<"type">>, Type},
                    {<<"to">>, jid:to_binary(Recipient)}],
           children = Children}.
