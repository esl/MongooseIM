%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Amazon SNS notifications. This module gathers all message send by users and all presence
%%% changes and publishes those events to AWS SNS.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_aws_sns).
-author("Rafal Slota").

-behavior(gen_mod).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-callback user_guid(UserJID :: ejabberd:jid()) -> mod_aws_sns:user_guid().
-callback message_attributes(TopicARN :: mod_aws_sns:topic_arn(), UserJID :: ejabberd:jid(),
                             IsOnline :: boolean()) -> mod_aws_sns:attributes().
-callback message_attributes(TopicARN :: mod_aws_sns:topic_arn(), From :: ejabberd:jid(),
                             To :: ejabberd:jid(), MessageType :: pm | muc,
                             Packet :: jlib:xmlel()) -> mod_aws_sns:attributes().

%%%===================================================================
%%% Types and definitions
%%%===================================================================

-define(TOPIC_BASE, ["arn", "aws", "sns"]).

-type user_guid() :: binary().
-type topic_arn() :: string(). %% Full topic ARN in format arn:aws:sns:{REGION}:{ACCOUNT_ID}:{TOPIC}
-type topic() :: string(). %% {TOPIC} part of topic_arn() type
-type attributes() :: erlcloud:sns_message_attributes().


%%%===================================================================
%%% Exports
%%%===================================================================

%% MIM module callbacks
-export([start/2, stop/1]).

%% Hooks
-export([user_send_packet/3, user_present/1, user_not_present/4, filter_room_packet/2]).

%% Types
-export_type([user_guid/0, topic_arn/0, topic/0, attributes/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start(Host :: ejabberd:server(), Opts :: proplists:proplist()) -> ok.
start(Host, Opts) ->
    MUCHost = gen_mod:get_opt_subhost(Host, muc_host, Opts, mod_muc:default_host()),
    ejabberd_hooks:add(filter_room_packet, MUCHost, ?MODULE, filter_room_packet, 90),
    ejabberd_hooks:add(rest_user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:add(user_available_hook, Host, ?MODULE, user_present, 90),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, user_not_present, 90),

    application:ensure_all_started(erlcloud),

    %% Check for required options
    RequiredOptions = [access_key_id, secret_access_key, region, account_id, sns_host],
    lists:foreach(
        fun(Option) ->
            case opt(Host, Option) of
                undefined ->
                    error({missing_required_option, Option});
                _ -> ok
            end
         end, RequiredOptions),

    ok.

-spec stop(Host :: ejabberd:server()) -> ok.
stop(Host) ->
    MUCHost = gen_mod:get_module_opt_subhost(Host, ?MODULE, mod_muc:default_host()),
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, user_not_present, 90),
    ejabberd_hooks:delete(user_available_hook, Host, ?MODULE, user_present, 90),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:delete(rest_user_send_packet, Host, ?MODULE, user_send_packet, 90),
    ejabberd_hooks:delete(filter_room_packet, MUCHost, ?MODULE, filter_room_packet, 90),

    ok.


%% Handle user_send_packet hook and muc messages
-spec user_send_packet(From :: ejabberd:jid(), To :: ejabberd:jid(),
                       Packet :: jlib:xmlel()) -> 'ok'.
user_send_packet(From = #jid{lserver = Host}, To, Packet) ->
    ?DEBUG("SNS Packet handle~n    from ~p ~n    to ~p~n    packet ~p.", [From, To, Packet]),

    case {get_topic(Host, Packet), xml:get_subtag(Packet, <<"body">>)} of
        {undefined, _} -> %% Skip if there is no topic set in configuration for the packet type
            skip;
        {_, false} -> %% Skip if there is no message body in the packet
            skip;
        {Topic, BodyTag} ->
            FromGUID = user_guid(Host, From),
            ToGUID = user_guid(Host, To),

            MessageBody = xml:get_tag_cdata(BodyTag),
            Content = #{from_user_id => FromGUID,
                        to_user_id => ToGUID,
                        message => MessageBody},

            TopicARN = make_topic_arn(Host, Topic),
            Attributes = message_attributes(Host, TopicARN, From, To, message_type(Packet), Packet),

            publish(Host, TopicARN, Content, Attributes)
    end,

    ok.

%% Handle filter_room_packet
-spec filter_room_packet(Packet :: jlib:xmlel(), EventData :: proplists:proplist()) ->
    Packet :: jlib:xmlel().
filter_room_packet(Packet, EventData) ->
    {_, FromJID} = lists:keyfind(from_jid, 1, EventData),
    {_, RoomJID} = lists:keyfind(room_jid, 1, EventData),
    catch user_send_packet(FromJID, RoomJID, Packet),
    Packet.

%% Handle user_available_hook
-spec user_present(UserJID :: ejabberd:jid()) -> ok.
user_present(#jid{} = UserJID) ->
    user_presence_changed(UserJID, true).

%% Handle unset_presence_hook
-spec user_not_present(User     :: ejabberd:luser(),
                       Server   :: ejabberd:lserver(),
                       Resource :: ejabberd:lresource(),
                       _Status :: any()) -> ok.
user_not_present(User, Host, Resource, _Status) ->
    user_presence_changed(jid:make_noprep(User, Host, Resource), false).

-spec user_presence_changed(UserJID :: ejabberd:jid(), IsOnline :: boolean()) -> ok.
user_presence_changed(#jid{lserver = Host} = UserJID, IsOnline) ->
    Topic = opt(Host, presence_updates_topic),
    case Topic of
        undefined ->
            skip;
        Topic ->
            UserGUID = user_guid(Host, UserJID),
            Content = #{user_id => UserGUID, present => IsOnline},
            TopicARN = make_topic_arn(Host, Topic),
            Attributes = message_attributes(Host, TopicARN, UserJID, IsOnline),
            publish(Host, TopicARN, Content, Attributes)
    end,

    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Publish notification to AWS SNS service. Content should be valid JSON term
-spec publish(Host :: ejabberd:lserver(), topic_arn(), Content :: #{}, attributes()) ->
                     MessageId :: string().
publish(Host, TopicARN, Content, Attributes) ->
    EncodedContent = jiffy:encode(Content),
    erlcloud_sns:publish(topic, TopicARN, unicode:characters_to_list(EncodedContent),
                         undefined, maps:to_list(Attributes), aws_handle(Host)).

%% @doc Returns AWS SNS handle base on configured AWS credentials
-spec aws_handle(Host :: ejabberd:lserver()) -> erlcloud:sns_handle().
aws_handle(Host) ->
    AccessKeyId = opt(Host, access_key_id),
    SecretKey = opt(Host, secret_access_key),
    SNSHost = opt(Host, sns_host),

    erlcloud_sns:new(AccessKeyId, SecretKey, SNSHost).

%% @doc Returns notification topic based on packet type and module configuration
-spec get_topic(Host :: ejabberd:lserver(), Packet :: jlib:xmlel()) -> topic() | undefined.
get_topic(Host, Packet) ->
    case message_type(Packet) of
        pm ->
            opt(Host, pm_messages_topic);
        muc ->
            opt(Host, muc_messages_topic);
        _ ->
            undefined
    end.


%% @doc Constructs SNS TopicArn from given topic suffix
-spec make_topic_arn(Host :: ejabberd:lserver(), Topic :: topic()) -> topic_arn().
make_topic_arn(Host, Topic) ->
    AWSRegion = opt(Host, region),
    AWSAccountId = opt(Host, account_id),

    string:join(?TOPIC_BASE ++ [AWSRegion, AWSAccountId, Topic], ":").

%% @doc Returns message type
-spec message_type(Packet :: jlib:xmlel()) -> pm | muc | undefined.
message_type(Packet) ->
    case xml:get_tag_attr_s(<<"type">>, Packet) of
        <<"chat">> -> pm;
        <<"groupchat">> -> muc;
        _ -> undefined
    end.

%% Getter for module options
-spec opt(Host :: ejabberd:lserver(), Option :: atom()) -> Value :: term() | undefined.
opt(Host, Option) ->
    opt(Host, Option, undefined).

%% Getter for module options with default value
-spec opt(Host :: ejabberd:lserver(), Option :: atom(), Default :: term()) ->
    Value :: term().
opt(Host, Option, Default) ->
    gen_mod:get_module_opt(Host, ?MODULE, Option, Default).

%% ----------------------------------------------------------------------
%% Callbacks

-spec user_guid(Host :: ejabberd:lserver(), UserJID :: ejabberd:jid()) -> mod_aws_sns:user_guid().
user_guid(Host, UserJID) ->
    PluginModule = opt(Host, plugin_module, mod_aws_sns_defaults),
    PluginModule:user_guid(UserJID).

-spec message_attributes(Host :: ejabberd:lserver(), TopicARN :: mod_aws_sns:topic_arn(),
                         UserJID :: ejabberd:jid(), IsOnline :: boolean()) ->
                                mod_aws_sns:attributes().
message_attributes(Host, TopicARN, UserJID, IsOnline) ->
    PluginModule = opt(Host, plugin_module, mod_aws_sns_defaults),
    PluginModule:message_attributes(TopicARN, UserJID, IsOnline).

-spec message_attributes(Host :: ejabberd:lserver(), TopicARN :: mod_aws_sns:topic_arn(),
                         From :: ejabberd:jid(), To :: ejabberd:jid(), MessageType :: pm | muc,
                         Packet :: jlib:xmlel()) -> mod_aws_sns:attributes().
message_attributes(Host, TopicARN, From, To, MessageType, Packet) ->
    PluginModule = opt(Host, plugin_module, mod_aws_sns_defaults),
    PluginModule:message_attributes(TopicARN, From, To, MessageType, Packet).
