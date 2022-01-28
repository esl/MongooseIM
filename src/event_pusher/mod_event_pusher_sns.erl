-module(mod_event_pusher_sns).

-behaviour(gen_mod).
-behaviour(mod_event_pusher).

-include("mod_event_pusher_events.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include("mongoose_config_spec.hrl").

-callback user_guid(UserJID :: jid:jid()) -> user_guid().
-callback message_attributes(TopicARN :: topic_arn(), UserJID :: jid:jid(),
                             IsOnline :: boolean()) -> attributes().
-callback message_attributes(TopicARN :: topic_arn(), From :: jid:jid(),
                             To :: jid:jid(), MessageType :: pm | muc,
                             Packet :: exml:element()) -> attributes().

%%%===================================================================
%%% Types and definitions
%%%===================================================================

-define(TOPIC_BASE, ["arn", "aws", "sns"]).
-define(PUBLISH_RETRY_COUNT_DEFAULT, 2).

-type user_guid() :: binary().
-type topic_arn() :: string(). %% Full topic ARN in format arn:aws:sns:{REGION}:{ACCOUNT_ID}:{TOPIC}
-type topic() :: string(). %% {TOPIC} part of topic_arn() type
-type attributes() :: #{string() => string() | binary() | number()}.


%%%===================================================================
%%% Exports
%%%===================================================================

%% MIM module callbacks
-export([start/2, stop/1, config_spec/0]).

%% API
-export([try_publish/5, push_event/3]).

-ignore_xref([behaviour_info/1, try_publish/5]).

%% Types
-export_type([user_guid/0, topic_arn/0, topic/0, attributes/0]).

-spec start(Host :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> ok.
start(Host, Opts) ->
    application:ensure_all_started(erlcloud),
    application:ensure_all_started(worker_pool),
    start_pool(Host, Opts),
    ok.

-spec start_pool(Host :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) ->
    term().
start_pool(Host, Opts) ->
    {ok, _} = mongoose_wpool:start(generic, Host, pusher_sns, pool_opts(Opts)).

-spec pool_opts(gen_mod:module_opts()) -> mongoose_wpool:pool_opts().
pool_opts(Opts) ->
    WorkerNum = get_worker_num(Opts),
    [{workers, WorkerNum}, {strategy, available_worker}].

-spec get_worker_num(gen_mod:module_opts()) -> pos_integer().
get_worker_num(Opts) ->
    gen_mod:get_opt(pool_size, Opts, 100).

-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    mongoose_wpool:stop(generic, Host, pusher_sns),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"presence_updates_topic">> => #option{type = string},
                 <<"pm_messages_topic">> => #option{type = string},
                 <<"muc_messages_topic">> => #option{type = string},
                 <<"plugin_module">> => #option{type = atom,
                                                validate = module},
                 <<"sns_host">> => #option{type = string},
                 <<"region">> => #option{type = string},
                 <<"access_key_id">> => #option{type = string},
                 <<"secret_access_key">> => #option{type = string},
                 <<"account_id">> => #option{type = string},
                 <<"pool_size">> => #option{type = integer,
                                            validate = positive},
                 <<"publish_retry_count">> => #option{type = integer,
                                                      validate = non_negative},
                 <<"publish_retry_time_ms">> => #option{type = integer,
                                                        validate = non_negative}
                },
       required = [<<"access_key_id">>, <<"secret_access_key">>,
                   <<"region">>, <<"account_id">>, <<"sns_host">>]
      }.

push_event(Acc, _, #user_status_event{jid = UserJID, status = Status}) ->
    user_presence_changed(UserJID, Status == online),
    Acc;
push_event(Acc, _, #chat_event{direction = in, from = From, to = To, packet = Packet}) ->
    handle_packet(From, To, Packet),
    Acc;
push_event(Acc, _, _) ->
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec user_presence_changed(UserJID :: jid:jid(), IsOnline :: boolean()) -> ok.
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

%% @doc Handles packet and if needed publishes SNS notification
-spec handle_packet(From :: jid:jid(), To :: jid:jid(),
                    Packet :: exml:element()) -> ok | skip.
handle_packet(From = #jid{lserver = Host}, To, Packet) ->
    ?LOG_DEBUG(#{what => sns_handle_packet,
                 from => From, to => To, packet => exml_Packet}),

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

%% @doc Start publish process notification to AWS SNS service. Content should be valid JSON term
-spec async_publish(Host :: jid:lserver(), topic_arn(), Content :: jiffy:json_value(),
              attributes()) -> ok.
async_publish(Host, TopicARN, Content, Attributes) ->
    Retry = opt(Host, publish_retry_count, ?PUBLISH_RETRY_COUNT_DEFAULT),
    mongoose_wpool:cast(generic, Host, pusher_sns,
                        {?MODULE, try_publish, [Host, TopicARN, Content, Attributes, Retry]}).

%% @doc Publish notification to AWS SNS service. Content should be a valid JSON term
-spec try_publish(Host :: jid:lserver(), topic_arn(), Content :: jiffy:json_value(),
              attributes(), TryCount :: integer()) -> MessageId :: string() | dropped | scheduled.
try_publish(Host, TopicARN, Content, Attributes, Retry) when Retry < 0 ->
    ?LOG_WARNING(#{what => sns_notification_dropped,
                   server => Host, topic_arn => TopicARN,
                   attributes => Attributes, content => Content}),
    dropped;
try_publish(Host, TopicARN, Content, Attributes, Retry) ->
    try publish(Host, TopicARN, Content, Attributes)
    catch
        Type:Error:StackTrace ->
            BackoffTime = calc_backoff_time(Host, Retry),
            timer:apply_after(BackoffTime, mongoose_wpool, cast,
                              [generic, Host, pusher_sns,
                               {?MODULE, try_publish,
                                [Host, TopicARN, Content, Attributes, Retry - 1]}]),
            ?LOG_WARNING(#{what => sns_notification_retry,
                           text => <<"Retrying SNS notification after {backoff_time} ms">>,
                           server => Host, topic_arn => TopicARN,
                           attributes => Attributes, content => Content,
                           backoff_time => BackoffTime,
                           class => Type, reason => Error, stacktrace => StackTrace}),
            scheduled
    end.

%% @doc Publish notification to AWS SNS service. Content should be a valid JSON term
-spec publish(Host :: jid:lserver(), topic_arn(), Content :: jiffy:json_value(),
              attributes()) -> MessageId :: string().
publish(Host, TopicARN, Content, Attributes) ->
    erlcloud_sns:publish(topic, TopicARN, maps:to_list(Content),
                         undefined, maps:to_list(Attributes), aws_handle(Host)).

%% @doc Returns AWS SNS handle base on configured AWS credentials
-spec aws_handle(Host :: jid:lserver()) -> aws_config().
aws_handle(Host) ->
    AccessKeyId = opt(Host, access_key_id),
    SecretKey = opt(Host, secret_access_key),
    SNSHost = opt(Host, sns_host),

    erlcloud_sns:new(AccessKeyId, SecretKey, SNSHost).

%% @doc Returns notification topic based on packet type and module configuration
-spec get_topic(Host :: jid:lserver(), Packet :: exml:element()) -> topic() | undefined.
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
-spec make_topic_arn(Host :: jid:lserver(), Topic :: topic()) -> topic_arn().
make_topic_arn(Host, Topic) ->
    AWSRegion = opt(Host, region),
    AWSAccountId = opt(Host, account_id),

    string:join(?TOPIC_BASE ++ [AWSRegion, AWSAccountId, Topic], ":").

%% @doc Returns message type
-spec message_type(Packet :: exml:element()) -> pm | muc | undefined.
message_type(Packet) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"chat">> -> pm;
        <<"groupchat">> -> muc;
        _ -> undefined
    end.

%% Getter for module options
-spec opt(Host :: jid:lserver(), Option :: atom()) -> Value :: term() | undefined.
opt(Host, Option) ->
    opt(Host, Option, undefined).

%% Getter for module options with default value
-spec opt(Host :: jid:lserver(), Option :: atom(), Default :: term()) ->
    Value :: term().
opt(Host, Option, Default) ->
    gen_mod:get_module_opt(Host, ?MODULE, Option, Default).

%% ----------------------------------------------------------------------
%% Callbacks

-spec user_guid(Host :: jid:lserver(), UserJID :: jid:jid()) -> user_guid().
user_guid(Host, UserJID) ->
    PluginModule = opt(Host, plugin_module, mod_event_pusher_sns_defaults),
    PluginModule:user_guid(UserJID).

-spec message_attributes(Host :: jid:lserver(), TopicARN :: topic_arn(),
                         UserJID :: jid:jid(), IsOnline :: boolean()) ->
                                attributes().
message_attributes(Host, TopicARN, UserJID, IsOnline) ->
    PluginModule = opt(Host, plugin_module, mod_event_pusher_sns_defaults),
    PluginModule:message_attributes(TopicARN, UserJID, IsOnline).

-spec message_attributes(Host :: jid:lserver(), TopicARN :: topic_arn(),
                         From :: jid:jid(), To :: jid:jid(), MessageType :: pm | muc,
                         Packet :: exml:element()) -> attributes().
message_attributes(Host, TopicARN, From, To, MessageType, Packet) ->
    PluginModule = opt(Host, plugin_module, mod_event_pusher_sns_defaults),
    PluginModule:message_attributes(TopicARN, From, To, MessageType, Packet).

-spec calc_backoff_time(Host :: jid:lserver(), integer()) -> integer().
calc_backoff_time(Host, Retry) ->
    MaxRetry = opt(Host, publish_retry_count, ?PUBLISH_RETRY_COUNT_DEFAULT),
    BaseTime = opt(Host, publish_retry_time_ms, 50),
    BackoffMaxTime = round(math:pow(2, MaxRetry - Retry)) * BaseTime,
    Random = rand:uniform(BaseTime),
    BackoffMaxTime - Random.
