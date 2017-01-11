-module(mod_aws_sns).
-author("Rafal Slota").

-behavior(gen_mod).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(TOPIC_BASE, ["arn", "aws", "sns"]).

%% API
-export([start/2, stop/1]).

-export([user_send_packet/3, user_present/1, user_not_present/4]).



start(Host, _Opts) ->
    ejabberd_hooks:add(rest_user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(user_available_hook, Host, ?MODULE, user_present, 90),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, user_not_present, 90),

    application:ensure_all_started(erlcloud),

    ok.

stop(Host) ->
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, user_not_present, 90),
    ejabberd_hooks:delete(user_available_hook, Host, ?MODULE, user_present, 90),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:delete(rest_user_send_packet, Host, ?MODULE, user_send_packet, 90).


-spec user_send_packet(From :: ejabberd:jid(), To :: ejabberd:jid(),
                       Packet :: jlib:xmlel()) -> 'ok'.
user_send_packet(From = #jid{lserver = Host, luser = FromUser},
                 To = #jid{luser = ToUser}, Packet) ->
    ?WARNING_MSG("Send packet~n    from ~p ~n    to ~p~n    packet ~p.",
           [From, To, Packet]),

    case get_topic(Host, Packet) of
        undefined ->
            skip;
        Topic ->
%%            BareTo = jid:to_binary(jid:to_bare(To)),
%%            BareFrom = jid:to_binary(jid:to_bare(From)),

            FromGUID = user_guid(Host, From),
            ToGUID = user_guid(Host, To),

            MessageBody = xml:get_tag_cdata(xml:get_subtag(Packet, <<"body">>)),
            Content = #{from_user_id => FromGUID,
                       to_user_id => ToGUID,
                       message => MessageBody},

            TopicARN = make_topic_arn(Host, Topic),
            Attributes = message_attributes(Host, TopicARN, From, To, message_type(Packet), Packet),

            publish(Host, Topic, Content, Attributes)
    end,

    ok.


user_present(#jid{} = UserJID) ->
    user_precence_changed(UserJID, true).

user_not_present(User, Host, Resource, _Status) ->
    user_precence_changed(jid:make_noprep(User, Host, Resource), false).

user_precence_changed(#jid{lserver = Host, luser = User} = UserJID, IsPresent) ->

    Topic = gen_mod:get_module_opt(Host, ?MODULE, presence_updates_topic, undefined),
    ?WARNING_MSG("user_precence_changed topic: ~p", [Topic]),
    case Topic of
        undefined ->
            skip;
        Topic ->
            UserGUID = user_guid(Host, UserJID),
            Content = #{user_id => UserGUID, present => IsPresent},
            TopicARN = make_topic_arn(Host, Topic),
            Attributes = message_attributes(Host, TopicARN, UserJID, IsPresent),
            publish(Host, TopicARN, Content, Attributes)
    end.



aws_handle(Host) ->
    AccessKeyId = [_ | _] = gen_mod:get_module_opt(Host, ?MODULE, access_key_id, undefined),
    SecretKey = [_ | _] =  gen_mod:get_module_opt(Host, ?MODULE, secret_access_key, undefined),
    SNSHost = [_ | _] =  gen_mod:get_module_opt(Host, ?MODULE, sns_host, undefined),

    erlcloud_sns:new(AccessKeyId, SecretKey, SNSHost).


get_topic(Host, Packet) ->
    case message_type(Packet) of
        pm ->
            gen_mod:get_module_opt(Host, ?MODULE, pm_messages_topic, undefined);
        muc ->
            gen_mod:get_module_opt(Host, ?MODULE, muc_messages_topic, undefined);
        _ ->
            undefined
    end.

make_topic_arn(Host, Topic) ->
    AWSRegion = gen_mod:get_module_opt(Host, ?MODULE, region, undefined),
    AWSAccountId = gen_mod:get_module_opt(Host, ?MODULE, account_id, undefined),

    string:join(?TOPIC_BASE ++ [AWSRegion, AWSAccountId, Topic], ":").

publish(Host, TopicARN, Content, Attributes) ->
    EncodedContent = jiffy:encode(Content),
    Res = erlcloud_sns:publish(topic, TopicARN, EncodedContent, undefined, Attributes,
                               aws_handle(Host)),
    ?WARNING_MSG("OMG ~p", [Res]).

message_type(Packet) ->
    case xml:get_tag_attr_s(<<"type">>, Packet) of
        <<"chat">> -> pm;
        <<"groupchat">> -> muc;
        _ -> undefined
    end .

user_guid(Host, UserJID) ->
    CustomizationModule = gen_mod:get_module_opt(Host, ?MODULE, customization_module, undefined),
    CustomizationModule:user_guid(UserJID).


message_attributes(Host, TopicARN, UserJID, IsOnline) ->
    CustomizationModule = gen_mod:get_module_opt(Host, ?MODULE, customization_module, undefined),
    CustomizationModule:message_attributes(TopicARN, UserJID, IsOnline).

message_attributes(Host, TopicARN, From, To, MessageType, Packet) ->
    CustomizationModule = gen_mod:get_module_opt(Host, ?MODULE, customization_module, undefined),
    CustomizationModule:message_attributes(TopicARN, From, To, MessageType, Packet).