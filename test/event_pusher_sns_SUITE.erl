-module(event_pusher_sns_SUITE).

-compile([export_all, nowarn_export_all]).

-include("mongoose.hrl").
-include("mod_event_pusher_events.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(ACC_PARAMS, #{location => ?LOCATION,
                      host_type => host_type(),
                      lserver => domain(),
                      element => undefined}).

-import(config_parser_helper, [config/2, mod_config/2]).

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
     calls_callback_module_to_retrieve_attributes_for_message,
     {group, web_identity}
    ].

groups() ->
    [{web_identity, [], [web_identity_exchanges_token_with_sts,
                         web_identity_signs_with_session_token,
                         web_identity_uses_refreshed_session_and_rotated_token,
                         web_identity_keeps_session_when_refresh_fails,
                         web_identity_fails_publish_without_session]}].

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
    ?assert(meck:called(erlcloud_sns, publish, [topic, ExpectedTopic, '_', '_', '_', '_'])).

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
    meck:expect(erlcloud_sns, publish, fun(_, _, _, _, _, _) -> ok end),
    send_packet_callback(Config, <<"chat">>, <<"message">>),
    ?assertNot(meck:called(erlcloud_sns, publish, '_')).

does_not_forward_presences_when_topic_is_unset(Config) ->
    meck:expect(erlcloud_sns, publish, fun(_, _, _, _, _, _) -> ok end),
    user_not_present_callback(Config),
    ?assertNot(meck:called(erlcloud_sns, publish, '_')).

calls_callback_module_to_get_user_id(Config) ->
    Module = custom_callback_module("customuserid", #{}),
    expect_message_entry(user_id, "customuserid"),
    user_not_present_callback(Config),
    ?assert(meck:called(Module, user_guid, '_')).

calls_callback_module_to_retrieve_attributes_for_presence(Config) ->
    Module = custom_callback_module("", #{"a" => 123}),
    expect_attributes("a", 123),
    user_not_present_callback(Config),
    ?assert(meck:called(Module, message_attributes, ['_', '_', '_'])).

calls_callback_module_to_retrieve_attributes_for_message(Config) ->
    Module = custom_callback_module("", #{"b" => "abc"}),
    expect_attributes("b", "abc"),
    send_packet_callback(Config, <<"groupchat">>, <<"message">>),
    ?assert(meck:called(Module, message_attributes, ['_', '_', '_', '_', '_'])).

web_identity_exchanges_token_with_sts(_Config) ->
    {Url, [], "application/x-www-form-urlencoded", Body} =
        meck:capture(first, httpc, request, '_', 2),
    ?assertEqual("https://sts.eu-west-1.amazonaws.com/", Url),
    Params = uri_string:dissect_query(Body),
    ?assertEqual("AssumeRoleWithWebIdentity", proplists:get_value("Action", Params)),
    ?assertEqual(role_arn(), proplists:get_value("RoleArn", Params)),
    ?assertEqual("mongooseim", proplists:get_value("RoleSessionName", Params)),
    ?assertEqual("token-1", proplists:get_value("WebIdentityToken", Params)),
    SslOpts = proplists:get_value(ssl, meck:capture(first, httpc, request, '_', 3)),
    ?assertEqual(verify_peer, proplists:get_value(verify, SslOpts)).

web_identity_signs_with_session_token(Config) ->
    meck:expect(erlcloud_sns, publish, fun(_, _, _, _, _, _) -> ok end),
    send_packet_callback(Config, <<"chat">>, <<"message">>),
    assert_signed_with("token-1").

web_identity_uses_refreshed_session_and_rotated_token(Config) ->
    meck:expect(erlcloud_sns, publish, fun(_, _, _, _, _, _) -> ok end),
    ok = file:write_file(?config(token_file, Config), <<"token-2\n">>),
    refresh_web_identity_session(),
    send_packet_callback(Config, <<"chat">>, <<"message">>),
    assert_signed_with("token-2").

web_identity_keeps_session_when_refresh_fails(Config) ->
    meck:expect(erlcloud_sns, publish, fun(_, _, _, _, _, _) -> ok end),
    meck:expect(httpc, request, fun(_, _, _, _) -> sts_error_response() end),
    refresh_web_identity_session(),
    send_packet_callback(Config, <<"chat">>, <<"message">>),
    assert_signed_with("token-1").

web_identity_fails_publish_without_session(_Config) ->
    ?assertError(#{what := sns_web_identity_session_unavailable},
                 mod_event_pusher_sns_web_identity:aws_config(host_type(),
                                                              "sns.eu-west-1.amazonaws.com")).

%% Fixtures

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    mongoose_config:set_opts(opts()),
    async_helper:start(Config, [{mongoose_instrument, start_link, []},
                                {mongooseim_helper, start_link_loaded_hooks, []},
                                {mim_ct_sup, start_link, [ejabberd_sup]}]).

end_per_suite(Config) ->
    async_helper:stop_all(Config),
    mongoose_config:erase_opts().

init_per_testcase(CaseName, Config0) ->
    Config = init_web_identity(CaseName, Config0),
    meck:new(erlcloud_sns, [non_strict, passthrough]),
    case is_web_identity_case(CaseName) of
        true -> ok; % uses the real erlcloud_sns:new/3 to check the signing config
        false -> meck:expect(erlcloud_sns, new,
                             fun(_, _, _) -> mod_aws_sns_SUITE_erlcloud_sns_new end)
    end,
    meck:expect(mongoose_wpool, start, fun(_, _, _, _) -> {ok, mocked} end),
    meck:expect(mongoose_wpool, cast, fun(_, _, _, {M, F, A}) -> erlang:apply(M, F, A) end),
    start_modules(sns_config(CaseName, Config)),
    [{sender, jid:from_binary(<<"sender@localhost">>)},
     {recipient, jid:from_binary(<<"recipient@localhost">>)} |
     Config].

init_web_identity(CaseName, Config) ->
    case is_web_identity_case(CaseName) of
        true ->
            TokenFile = filename:join(?config(priv_dir, Config), "web-identity-token"),
            ok = file:write_file(TokenFile, <<"token-1\n">>),
            meck:new(httpc, [passthrough]),
            meck:expect(httpc, request, sts_mock(CaseName)),
            [{token_file, TokenFile} | Config];
        false ->
            Config
    end.

is_web_identity_case(CaseName) ->
    lists:prefix("web_identity_", atom_to_list(CaseName)).

end_per_testcase(_, _Config) ->
    stop_modules(),
    meck:unload().

%% Wrapped callbacks

send_packet_callback(Config, Type, Body) ->
    Packet = message(Config, Type, Body),
    Sender = ?config(sender, Config),
    Recipient = ?config(recipient, Config),
    push_event(#msg_event{type = chat, direction = in,
                          from = Sender, to = Recipient,
                          packet = Packet}).

user_present_callback(Config) ->
    Jid = ?config(sender, Config),
    push_event(#user_status_event{jid = Jid, status = online}).

user_not_present_callback(Config) ->
    Jid = ?config(sender, Config),
    push_event(#user_status_event{jid = Jid, status = offline}).

push_event(Event) ->
    HookAcc = #{acc => mongoose_acc:new(?ACC_PARAMS), metadata => #{}},
    HookParams = #{event => Event},
    HookExtra = #{host_type => host_type()},
    mod_event_pusher_sns:push_event(HookAcc, HookParams, HookExtra).

%% Helpers

refresh_web_identity_session() ->
    Proc = gen_mod:get_module_proc(host_type(), mod_event_pusher_sns_web_identity),
    Proc ! refresh,
    _ = sys:get_state(Proc), % wait until the refresh is done
    ok.

assert_signed_with(WebIdentityToken) ->
    #aws_config{access_key_id = KeyId, security_token = SessionToken, sns_host = Host} =
        meck:capture(last, erlcloud_sns, publish, '_', 6),
    ?assertEqual("key-" ++ WebIdentityToken, KeyId),
    ?assertEqual("session-" ++ WebIdentityToken, SessionToken),
    ?assertEqual("sns.eu-west-1.amazonaws.com", Host).

sts_mock(web_identity_fails_publish_without_session) ->
    fun(_, _, _, _) -> sts_error_response() end;
sts_mock(_) ->
    fun(post, {_Url, _, _, Body}, _, _) ->
            Token = proplists:get_value("WebIdentityToken", uri_string:dissect_query(Body)),
            sts_response(Token)
    end.

sts_response(Token) ->
    Expiration = calendar:system_time_to_rfc3339(erlang:system_time(second) + 3600,
                                                 [{offset, "Z"}]),
    Xml = ["<AssumeRoleWithWebIdentityResponse xmlns=\"https://sts.amazonaws.com/doc/2011-06-15/\">"
           "<AssumeRoleWithWebIdentityResult><Credentials>"
           "<AccessKeyId>key-", Token, "</AccessKeyId>"
           "<SecretAccessKey>secret</SecretAccessKey>"
           "<SessionToken>session-", Token, "</SessionToken>"
           "<Expiration>", Expiration, "</Expiration>"
           "</Credentials></AssumeRoleWithWebIdentityResult>"
           "</AssumeRoleWithWebIdentityResponse>"],
    {ok, {{"HTTP/1.1", 200, "OK"}, [], iolist_to_binary(Xml)}}.

sts_error_response() ->
    {ok, {{"HTTP/1.1", 403, "Forbidden"}, [],
          <<"<ErrorResponse><Error><Code>AccessDenied</Code></Error></ErrorResponse>">>}}.

role_arn() ->
    "arn:aws:iam::251423380551:role/mongooseim".

custom_callback_module(UserId, Attributes) ->
    Module = custom_callback_module(),
    meck:new(Module, [non_strict]),
    meck:expect(Module, user_guid, fun(_) -> UserId end),
    meck:expect(Module, message_attributes, fun(_, _, _) -> Attributes end),
    meck:expect(Module, message_attributes, fun(_, _, _, _, _) -> Attributes end),
    Module.

custom_callback_module() ->
    mod_aws_sns_SUITE_mockcb.

craft_arn(Topic) ->
    #{region := Region, account_id := AccountId} = common_sns_opts(),
    craft_arn(Region, AccountId, Topic).

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

sns_config(CaseName, Config) ->
    case is_web_identity_case(CaseName) of
        true ->
            (maps:without([access_key_id, secret_access_key], common_sns_opts()))#{
              credentials => web_identity,
              role_arn => role_arn(),
              web_identity_token_file => ?config(token_file, Config)};
        false ->
            sns_config(CaseName)
    end.

sns_config(does_not_forward_messages_when_topic_is_unset) ->
    maps:remove(pm_messages_topic, common_sns_opts());
sns_config(does_not_forward_presences_when_topic_is_unset) ->
    maps:remove(presence_updates_topic, common_sns_opts());
sns_config(CN) when CN =:= calls_callback_module_to_get_user_id;
                    CN =:= calls_callback_module_to_retrieve_attributes_for_presence;
                    CN =:= calls_callback_module_to_retrieve_attributes_for_message ->
    (common_sns_opts())#{plugin_module => custom_callback_module()};
sns_config(_) ->
    common_sns_opts().

start_modules(SNSExtra) ->
    mongoose_config:set_opt({modules, host_type()}, modules(SNSExtra)),
    mongoose_modules:start().

stop_modules() ->
    mongoose_modules:stop().

opts() ->
    #{hosts => [host_type()],
      host_types => [],
      instrumentation => config_parser_helper:default_config([instrumentation])}.

modules(SNSExtra) ->
    gen_mod_deps:resolve_deps(host_type(), #{mod_event_pusher => module_opts(SNSExtra)}).

module_opts(SNSExtra) ->
    SNSOpts = config([modules, mod_event_pusher, sns], SNSExtra),
    mod_config(mod_event_pusher, #{sns => SNSOpts}).

common_sns_opts() ->
    #{sns_host => "sns.eu-west-1.amazonaws.com",
      region => "eu-west-1",
      access_key_id => "AKIAJAZYHOIPY6A2PESA",
      secret_access_key => "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",
      account_id => "251423380551",
      presence_updates_topic => "user_presence_updated-dev-1",
      pm_messages_topic => "user_message_sent-dev-1",
      muc_messages_topic => "user_messagegroup_sent-dev-1"}.

message(Config, Type, Body) ->
    message(?config(sender, Config), ?config(recipient, Config), Type, Body).

message(From, Recipient, Type, Body) ->
    Children =
        case Body of
            undefined -> [];
            _ -> [#xmlel{name = <<"body">>, children = [#xmlcdata{content = Body}]}]
        end,
    #xmlel{name = <<"message">>,
           attrs = #{<<"from">> => jid:to_binary(From),
                     <<"type">> => Type,
                     <<"to">> => jid:to_binary(Recipient)},
           children = Children}.

host_type() ->
    domain().

domain() ->
    <<"localhost">>.
