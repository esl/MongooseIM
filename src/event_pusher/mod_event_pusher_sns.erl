-module(mod_event_pusher_sns).

-behaviour(gen_mod).

-include("mod_event_pusher_events.hrl").
-include("mongoose.hrl").
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

-type user_guid() :: binary().
-type topic_arn() :: string(). %% Full topic ARN in format arn:aws:sns:{REGION}:{ACCOUNT_ID}:{TOPIC}
-type topic() :: string(). %% {TOPIC} part of topic_arn() type
-type attributes() :: #{string() => string() | binary() | number()}.


%%%===================================================================
%%% Exports
%%%===================================================================

%% MIM module callbacks
-export([start/2, stop/1, hooks/1, config_spec/0]).

%% API
-export([try_publish/5]).

%% hook handlers
-export([push_event/3]).

-ignore_xref([behaviour_info/1, try_publish/5]).

%% Types
-export_type([user_guid/0, topic_arn/0, topic/0, attributes/0]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    {ok, _} = application:ensure_all_started([erlcloud], permanent),
    start_pool(HostType, Opts),
    ok.

-spec start_pool(mongooseim:host_type(), gen_mod:module_opts()) -> term().
start_pool(HostType, Opts) ->
    {ok, _} = mongoose_wpool:start(generic, HostType, pusher_sns, pool_opts(Opts)).

-spec pool_opts(gen_mod:module_opts()) -> mongoose_wpool:pool_opts().
pool_opts(Opts) ->
    WorkerNum = get_worker_num(Opts),
    [{workers, WorkerNum}, {strategy, available_worker}].

-spec get_worker_num(gen_mod:module_opts()) -> pos_integer().
get_worker_num(Opts) ->
    gen_mod:get_opt(pool_size, Opts).

-spec stop(HostType :: jid:server()) -> ok.
stop(HostType) ->
    mongoose_wpool:stop(generic, HostType, pusher_sns),
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{push_event, HostType, fun ?MODULE:push_event/3, #{}, 50}].

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
       required = [<<"sns_host">>, <<"region">>, <<"access_key_id">>, <<"secret_access_key">>,
                   <<"account_id">>],
       defaults = #{<<"plugin_module">> => mod_event_pusher_sns_defaults,
                    <<"pool_size">> => 100,
                    <<"publish_retry_count">> => 2,
                    <<"publish_retry_time_ms">> => 50
                   }
    }.

-spec push_event(mod_event_pusher:push_event_acc(), mod_event_pusher:push_event_params(),
                 gen_hook:extra()) -> {ok, mod_event_pusher:push_event_acc()}.
push_event(HookAcc, #{event := #user_status_event{jid = UserJID, status = Status}},
           #{host_type := HostType}) ->
    user_presence_changed(HostType, UserJID, Status == online),
    {ok, HookAcc};
push_event(HookAcc, #{event := #chat_event{direction = in, from = From, to = To, packet = Packet}},
           #{host_type := HostType}) ->
    handle_packet(HostType, From, To, Packet),
    {ok, HookAcc};
push_event(HookAcc, _Params, _Extra) ->
    {ok, HookAcc}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec user_presence_changed(mongooseim:host_type(), UserJID :: jid:jid(),
                            IsOnline :: boolean()) -> ok.
user_presence_changed(HostType, UserJID, IsOnline) ->
    case gen_mod:lookup_module_opt(HostType, ?MODULE, presence_updates_topic) of
        {error, not_found} ->
            skip;
        {ok, Topic} ->
            UserGUID = user_guid(HostType, UserJID),
            Content = #{user_id => UserGUID, present => IsOnline},
            TopicARN = make_topic_arn(HostType, Topic),
            Attributes = message_attributes(HostType, TopicARN, UserJID, IsOnline),
            async_publish(HostType, TopicARN, Content, Attributes)
    end,
    ok.

%% @doc Handles packet and if needed publishes SNS notification
-spec handle_packet(mongooseim:host_type(), From :: jid:jid(), To :: jid:jid(),
                    Packet :: exml:element()) -> ok | skip.
handle_packet(HostType, From, To, Packet) ->
    ?LOG_DEBUG(#{what => sns_handle_packet,
                 from => From, to => To, packet => exml_Packet}),

    case {get_topic(HostType, Packet), exml_query:subelement(Packet, <<"body">>)} of
        {undefined, _} -> %% Skip if there is no topic set in configuration for the packet type
            skip;
        {_, undefined} -> %% Skip if there is no message body in the packet
            skip;
        {Topic, BodyTag} ->
            FromGUID = user_guid(HostType, From),
            ToGUID = user_guid(HostType, To),
            MessageBody = exml_query:cdata(BodyTag),
            Content = #{from_user_id => FromGUID,
                        to_user_id => ToGUID,
                        message => MessageBody},

            TopicARN = make_topic_arn(HostType, Topic),
            Attributes = message_attributes(HostType, TopicARN, From, To, message_type(Packet), Packet),

            async_publish(HostType, TopicARN, Content, Attributes)
    end.

%% @doc Start publish process notification to AWS SNS service. Content should be valid JSON term
-spec async_publish(mongooseim:host_type(), topic_arn(), Content :: jiffy:json_value(),
              attributes()) -> ok.
async_publish(HostType, TopicARN, Content, Attributes) ->
    Retry = gen_mod:get_module_opt(HostType, ?MODULE, publish_retry_count),
    mongoose_wpool:cast(generic, HostType, pusher_sns,
                        {?MODULE, try_publish, [HostType, TopicARN, Content, Attributes, Retry]}).

%% @doc Publish notification to AWS SNS service. Content should be a valid JSON term
-spec try_publish(mongooseim:host_type(), topic_arn(), Content :: jiffy:json_value(),
              attributes(), TryCount :: integer()) -> MessageId :: string() | dropped | scheduled.
try_publish(HostType, TopicARN, Content, Attributes, Retry) when Retry < 0 ->
    ?LOG_WARNING(#{what => sns_notification_dropped,
                   server => HostType, topic_arn => TopicARN,
                   attributes => Attributes, content => Content}),
    dropped;
try_publish(HostType, TopicARN, Content, Attributes, Retry) ->
    try publish(HostType, TopicARN, Content, Attributes)
    catch
        Type:Error:StackTrace ->
            BackoffTime = calc_backoff_time(HostType, Retry),
            timer:apply_after(BackoffTime, mongoose_wpool, cast,
                              [generic, HostType, pusher_sns,
                               {?MODULE, try_publish,
                                [HostType, TopicARN, Content, Attributes, Retry - 1]}]),
            ?LOG_WARNING(#{what => sns_notification_retry,
                           text => <<"Retrying SNS notification after {backoff_time} ms">>,
                           host_type => HostType, topic_arn => TopicARN,
                           attributes => Attributes, content => Content,
                           backoff_time => BackoffTime,
                           class => Type, reason => Error, stacktrace => StackTrace}),
            scheduled
    end.

%% @doc Publish notification to AWS SNS service. Content should be a valid JSON term
-spec publish(mongooseim:host_type(), topic_arn(), Content :: jiffy:json_value(),
              attributes()) -> MessageId :: string().
publish(HostType, TopicARN, Content, Attributes) ->
    erlcloud_sns:publish(topic, TopicARN, maps:to_list(Content),
                         undefined, maps:to_list(Attributes), aws_handle(HostType)).

%% @doc Returns AWS SNS handle base on configured AWS credentials
-spec aws_handle(mongooseim:host_type()) -> aws_config().
aws_handle(HostType) ->
    Opts = gen_mod:get_loaded_module_opts(HostType, ?MODULE),
    #{access_key_id := AccessKeyId, secret_access_key := SecretKey, sns_host := SNSHost} = Opts,
    erlcloud_sns:new(AccessKeyId, SecretKey, SNSHost).

%% @doc Returns notification topic based on packet type and module configuration
-spec get_topic(mongooseim:host_type(), Packet :: exml:element()) -> topic() | undefined.
get_topic(HostType, Packet) ->
    case message_type(Packet) of
        pm ->
            gen_mod:get_module_opt(HostType, ?MODULE, pm_messages_topic, undefined);
        muc ->
            gen_mod:get_module_opt(HostType, ?MODULE, muc_messages_topic, undefined);
        _ ->
            undefined
    end.

%% @doc Constructs SNS TopicArn from given topic suffix
-spec make_topic_arn(mongooseim:host_type(), topic()) -> topic_arn().
make_topic_arn(HostType, Topic) ->
    Opts = gen_mod:get_loaded_module_opts(HostType, ?MODULE),
    #{region := AWSRegion, account_id := AWSAccountId} = Opts,
    string:join(?TOPIC_BASE ++ [AWSRegion, AWSAccountId, Topic], ":").

%% @doc Returns message type
-spec message_type(Packet :: exml:element()) -> pm | muc | undefined.
message_type(Packet) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"chat">> -> pm;
        <<"groupchat">> -> muc;
        _ -> undefined
    end.

%% ----------------------------------------------------------------------
%% Callbacks

-spec user_guid(mongooseim:host_type(), UserJID :: jid:jid()) -> user_guid().
user_guid(HostType, UserJID) ->
    PluginModule = gen_mod:get_module_opt(HostType, ?MODULE, plugin_module),
    PluginModule:user_guid(UserJID).

-spec message_attributes(mongooseim:host_type(), TopicARN :: topic_arn(),
                         UserJID :: jid:jid(), IsOnline :: boolean()) ->
                                attributes().
message_attributes(HostType, TopicARN, UserJID, IsOnline) ->
    PluginModule = gen_mod:get_module_opt(HostType, ?MODULE, plugin_module),
    PluginModule:message_attributes(TopicARN, UserJID, IsOnline).

-spec message_attributes(mongooseim:host_type(), TopicARN :: topic_arn(),
                         From :: jid:jid(), To :: jid:jid(), MessageType :: pm | muc,
                         Packet :: exml:element()) -> attributes().
message_attributes(HostType, TopicARN, From, To, MessageType, Packet) ->
    PluginModule = gen_mod:get_module_opt(HostType, ?MODULE, plugin_module),
    PluginModule:message_attributes(TopicARN, From, To, MessageType, Packet).

-spec calc_backoff_time(mongooseim:host_type(), integer()) -> integer().
calc_backoff_time(HostType, Retry) ->
    Opts = gen_mod:get_loaded_module_opts(HostType, ?MODULE),
    #{publish_retry_count := MaxRetry, publish_retry_time_ms := BaseTime} = Opts,
    BackoffMaxTime = round(math:pow(2, MaxRetry - Retry)) * BaseTime,
    Random = rand:uniform(BaseTime),
    BackoffMaxTime - Random.
