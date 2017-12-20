-module(mod_event_pusher_sns).

-behaviour(gen_mod).
-behaviour(mod_event_pusher).

-include("mod_event_pusher_events.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-callback user_guid(UserJID :: ejabberd:jid()) -> user_guid().
-callback message_attributes(TopicARN :: topic_arn(), UserJID :: ejabberd:jid(),
                             IsOnline :: boolean()) -> attributes().
-callback message_attributes(TopicARN :: topic_arn(), From :: ejabberd:jid(),
                             To :: ejabberd:jid(), MessageType :: pm | muc,
                             Packet :: jlib:xmlel()) -> attributes().

%%%===================================================================
%%% Types and definitions
%%%===================================================================

-define(TOPIC_BASE, ["arn", "aws", "sns"]).

-type user_guid() :: binary().
-type topic_arn() :: string(). %% Full topic ARN in format arn:aws:sns:{REGION}:{ACCOUNT_ID}:{TOPIC}
-type topic() :: string(). %% {TOPIC} part of topic_arn() type
-type attributes() :: #{string() => string() | binary() | number()}.


%%%===================================================================
%%% Exports
%%%===================================================================

%% MIM module callbacks
-export([start/2, stop/1]).

%% API
-export([try_publish/5, push_event/2]).

%% Types
-export_type([user_guid/0, topic_arn/0, topic/0, attributes/0]).

-spec start(Host :: ejabberd:server(), Opts :: proplists:proplist()) -> ok.
start(Host, Opts) ->
    application:ensure_all_started(erlcloud),
    application:ensure_all_started(worker_pool),

    WorkerNum = gen_mod:get_opt(pool_size, Opts, 100),
    wpool:start_sup_pool(pool_name(Host), [{workers, WorkerNum}]),

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
    wpool:stop(pool_name(Host)),
    ok.

push_event(_, #user_status_event{jid = UserJID, status = Status}) ->
    user_presence_changed(UserJID, Status == online);
push_event(_, #chat_event{direction = in, from = From, to = To, packet = Packet}) ->
    handle_packet(From, To, Packet);
push_event(_, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
            async_publish(Host, TopicARN, Content, Attributes)
    end,
    ok.

%% @doc Handles the packet and if needed publishes an SNS notification
-spec handle_packet(From :: ejabberd:jid(), To :: ejabberd:jid(),
                    Packet :: jlib:xmlel()) -> ok | skip.
handle_packet(From = #jid{lserver = Host}, To, Packet) ->
    ?DEBUG("SNS Packet handle~n    from ~p ~n    to ~p~n    packet ~p.", [From, To, Packet]),

    case {get_topic(Host, Packet), exml_query:subelement(Packet, <<"body">>)} of
        {undefined, _} -> %% Skip if there is no topic set in configuration for the packet type
            skip;
        {_, undefined} -> %% Skip if there is no message body in the packet
            skip;
        {Topic, BodyTag} ->
            FromGUID = user_guid(Host, From),
            ToGUID = user_guid(Host, To),
            MessageBody = exml_query:cdata(BodyTag),
            Content = #{from_user_id => FromGUID,
                        to_user_id => ToGUID,
                        message => MessageBody},

            TopicARN = make_topic_arn(Host, Topic),
            Attributes = message_attributes(Host, TopicARN, From, To, message_type(Packet), Packet),

            async_publish(Host, TopicARN, Content, Attributes)
    end.

%% @doc Start publish process notification to AWS SNS service. Content should be a valid JSON term
-spec async_publish(Host :: ejabberd:lserver(), topic_arn(), Content :: jiffy:json_value(),
              attributes()) -> ok.
async_publish(Host, TopicARN, Content, Attributes) ->
    Retry = opt(Host, publish_retry_count, 2),
    wpool:cast(pool_name(Host),
               {?MODULE, try_publish, [Host, TopicARN, Content, Attributes, Retry]},
               available_worker).

%% @doc Publish notification to AWS SNS service. Content should be a valid JSON term
-spec try_publish(Host :: ejabberd:lserver(), topic_arn(), Content :: jiffy:json_value(),
              attributes(), TryCount :: integer()) -> MessageId :: string() | dropped | scheduled.
try_publish(Host, TopicARN, Content, _Attributes, Retry) when Retry < 0 ->
    ?WARNING_MSG("Dropped SNS notification ~p", [{Host, TopicARN, Content}]),
    dropped;
try_publish(Host, TopicARN, Content, Attributes, Retry) ->
    try publish(Host, TopicARN, Content, Attributes)
    catch
        Type:Error ->
            BackoffTime = calc_backoff_time(Host, Retry),
            timer:apply_after(BackoffTime, wpool, cast,
                              [pool_name(Host),
                               {?MODULE, try_publish,
                                [Host, TopicARN, Content, Attributes, Retry - 1]},
                               available_worker]),
            ?WARNING_MSG("Retrying SNS notification ~p after ~p ms due to ~p~n~p",
                         [{Host, TopicARN, Content}, BackoffTime,
                          {Type, Error}, erlang:get_stacktrace()]),
            scheduled
    end.

%% @doc Publish notification to AWS SNS service. Content should be a valid JSON term
-spec publish(Host :: ejabberd:lserver(), topic_arn(), Content :: jiffy:json_value(),
              attributes()) -> MessageId :: string().
publish(Host, TopicARN, Content, Attributes) ->
    EncodedContent = jiffy:encode(Content),
    erlcloud_sns:publish(topic, TopicARN, unicode:characters_to_list(EncodedContent),
                         undefined, maps:to_list(Attributes), aws_handle(Host)).

%% @doc Returns AWS SNS handle base on configured AWS credentials
-spec aws_handle(Host :: ejabberd:lserver()) -> aws_config().
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
    case exml_query:attr(Packet, <<"type">>) of
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

-spec user_guid(Host :: ejabberd:lserver(), UserJID :: ejabberd:jid()) -> user_guid().
user_guid(Host, UserJID) ->
    PluginModule = opt(Host, plugin_module, mod_event_pusher_sns_defaults),
    PluginModule:user_guid(UserJID).

-spec message_attributes(Host :: ejabberd:lserver(), TopicARN :: topic_arn(),
                         UserJID :: ejabberd:jid(), IsOnline :: boolean()) ->
                                attributes().
message_attributes(Host, TopicARN, UserJID, IsOnline) ->
    PluginModule = opt(Host, plugin_module, mod_event_pusher_sns_defaults),
    PluginModule:message_attributes(TopicARN, UserJID, IsOnline).

-spec message_attributes(Host :: ejabberd:lserver(), TopicARN :: topic_arn(),
                         From :: ejabberd:jid(), To :: ejabberd:jid(), MessageType :: pm | muc,
                         Packet :: jlib:xmlel()) -> attributes().
message_attributes(Host, TopicARN, From, To, MessageType, Packet) ->
    PluginModule = opt(Host, plugin_module, mod_event_pusher_sns_defaults),
    PluginModule:message_attributes(TopicARN, From, To, MessageType, Packet).

-spec calc_backoff_time(Host :: ejabberd:lserver(), integer()) -> integer().
calc_backoff_time(Host, Retry) ->
    MaxRetry = opt(Host, publish_retry_count),
    BaseTime = opt(Host, publish_retry_time_ms, 50),
    BackoffMaxTime = round(math:pow(2, MaxRetry - Retry)) * BaseTime,
    crypto:rand_uniform(BackoffMaxTime - BaseTime, BackoffMaxTime).

-spec pool_name(Host :: ejabberd:lserver()) -> atom().
pool_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).
