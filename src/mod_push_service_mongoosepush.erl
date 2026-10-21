%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Adapter for MongoosePush service.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_push_service_mongoosepush).
-author('rafal.slota@erlang-solutions.com').
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).
-behaviour(mongoose_batch_worker).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

%% gen_mod handlers
-export([start/2, stop/1, config_spec/0, instrumentation/1, supported_features/0]).

%% mongoose_batch_worker
-export([flush/2]).

%% Hooks and IQ handlers
-export([push_notifications/3]).

-export([http_notification/5]).

-export([config_metrics/1]).

-ignore_xref([http_notification/5]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type task() :: {[mod_event_pusher_push_content:content()],
                 mod_event_pusher_push:form(),
                 fun((ok | {error, term()}) -> ok)}.

%%--------------------------------------------------------------------
%% Module callbacks
%%--------------------------------------------------------------------

-spec start(Host :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> any().
start(HostType, #{async_sender := SenderOpts}) ->
    ?LOG_INFO(#{what => push_service_starting, host_type => HostType}),
    maybe_start_pool(HostType, SenderOpts),
    %% Hooks
    gen_hook:add_handlers(hooks(HostType)),
    ok.

-spec maybe_start_pool(mongooseim:host_type(), gen_mod:module_opts()) -> term().
maybe_start_pool(HostType, #{enabled := true} = SenderOpts) ->
    PoolOpts = pool_opts(SenderOpts, #{host_type => HostType}),
    {ok, _} = mongoose_async_pools:start_pool(HostType, mongoosepush_service, PoolOpts);
maybe_start_pool(_HostType, _Opts) ->
    ok.

-spec pool_opts(gen_mod:module_opts(), mongoose_async_pools:pool_extra()) -> mongoose_async_pools:pool_opts().
pool_opts(SenderOpts, Extra) ->
    SenderOpts#{pool_type => batch,
                flush_callback => fun ?MODULE:flush/2,
                flush_extra => Extra}.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    gen_hook:delete_handlers(hooks(HostType)),
    maybe_stop_pool(HostType).

-spec maybe_stop_pool(mongooseim:host_type()) -> ok.
maybe_stop_pool(HostType) ->
    case async_sender(HostType) of
        true -> mongoose_async_pools:stop_pool(HostType, mongoosepush_service);
        false -> ok
    end.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"pool_name">> => #option{type = atom,
                                            validate = pool_name},
                 <<"api_version">> => #option{type = binary,
                                              validate = {enum, [<<"v2">>, <<"v3">>]}},
                 <<"async_sender">> => async_sender_config_spec()
                },
       defaults = #{<<"pool_name">> => undefined,
                    <<"api_version">> => <<"v3">>}
      }.

async_sender_config_spec() ->
    #section{
        items = #{<<"enabled">> => #option{type = boolean},
                  <<"flush_interval">> => #option{type = integer, validate = non_negative},
                  <<"batch_size">> => #option{type = integer, validate = non_negative},
                  <<"pool_size">> => #option{type = integer, validate = non_negative}},
        defaults = #{<<"enabled">> => true,
                     <<"flush_interval">> => 2000,
                     <<"batch_size">> => 30,
                     <<"pool_size">> => 4 * erlang:system_info(schedulers_online)},
        include = always
    }.

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) ->
    [{mod_push_service_mongoosepush_sent, #{host_type => HostType},
      #{metrics => #{success_count => spiral, time => histogram, failed_count => spiral}}}].

-spec flush([task()], mongoose_async_pools:pool_extra()) -> ok.
flush(Tasks, #{host_type := HostType}) ->
    lists:foreach(fun({Notifications, Options, DisableFun}) ->
                      Res = send_push_notifications(HostType, Notifications, Options),
                      DisableFun(Res)
                  end, Tasks).

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{push_notifications, HostType, fun ?MODULE:push_notifications/3, #{}, 10}].

%% Hook 'push_notifications'
-spec push_notifications(Acc, Params, Extra) -> {ok, ok | {error, Reason :: term()}} when
    Acc :: ok | mongoose_acc:t(),
    Params :: #{notification_forms := [mod_event_pusher_push_content:content()],
                options := mod_event_pusher_push:form(),
                disable_fun := fun((ok | {error, term()}) -> ok)},
    Extra :: gen_hook:extra().
push_notifications(AccIn,
                   #{notification_forms := Notifications, options := Options, disable_fun := DisableFun},
                   #{host_type := HostType}) ->
    ?LOG_DEBUG(#{what => push_notifications, notifications => Notifications,
                 opts => Options, acc => AccIn}),
    case async_sender(HostType) of
        true ->
            {ok, mongoose_async_pools:put_task(HostType, mongoosepush_service,
                                               {Notifications, Options, DisableFun})};
        false ->
            Result = send_push_notifications(HostType, Notifications, Options),
            DisableFun(Result),
            {ok, Result}
    end.

%%--------------------------------------------------------------------
%% Module API
%%--------------------------------------------------------------------

-spec send_push_notifications(mongooseim:host_type(),
                              [mod_event_pusher_push_content:content()],
                              mod_event_pusher_push:form()) -> ok | {error, term()}.

send_push_notifications(HostType, Notifications, #{<<"device_id">> := DeviceId} = Options) ->
    ProtocolVersion = gen_mod:get_module_opt(HostType, ?MODULE, api_version),
    URL = <<ProtocolVersion/binary, "/notification/", DeviceId/binary>>,
    %% The publish IQ allows to put more notifications into one request
    %% but this is currently not used in MongooseIM.
    %% In case it's used we try sending other notifications
    lists:foldl(
        fun(_Notification, {error, device_not_registered} = Err) ->
            %% In this case there is no point in sending other push notifications
            %% so we can finish immediately
            Err;
           (Notification, _Result) ->
            send_push_notification(HostType, Notification, Options, URL)
        end,
        ok,
        Notifications
    ).

send_push_notification(HostType, Notification, Options, URL) ->
    ReqHeaders = [{<<"content-type">>, <<"application/json">>}],
    try make_notification(Notification, Options) of
        Payload ->
            http_notification(HostType, post, URL, ReqHeaders, Payload)
    catch
        C:R:S ->
            Info = #{what => make_notification_failed,
                     text => <<"Error making push notification">>,
                     notification => Notification,
                     options => Options},
            Error = Info#{class => C, reason => R, stacktrace => S},
            ?LOG_ERROR(Error),
            {error, make_notification_failed}
    end.

-spec http_notification(mongooseim:host_type(), post,
                        binary(), proplists:proplist(), binary()) ->
    ok | {error, Reason :: term()}.
http_notification(HostType, post, URL, ReqHeaders, Payload) ->
    PoolName = gen_mod:get_module_opt(HostType, ?MODULE, pool_name),
    mongoose_instrument:span(mod_push_service_mongoosepush_sent, #{host_type => HostType},
                             fun http_notification_impl/5, [HostType, PoolName, URL, ReqHeaders, Payload],
                             fun http_notification_result/2).

http_notification_impl(HostType, PoolName, URL, ReqHeaders, Payload) ->
    case mongoose_http_client:post(HostType, PoolName, URL, ReqHeaders, Payload) of
        {ok, {BinStatusCode, Body}} ->
            case binary_to_integer(BinStatusCode) of
                StatusCode when StatusCode >= 200 andalso StatusCode < 300 ->
                    ok;
                410 ->
                    ?LOG_WARNING(#{what => push_send_failed,
                                   text => <<"Unable to send push notification. "
                                             "Device not registered.">>,
                                   code => 410, reason => device_not_registered}),
                    {error, device_not_registered};
                StatusCode when StatusCode >= 400 andalso StatusCode < 500  ->
                    ?LOG_ERROR(#{what => push_send_failed,
                                 text => <<"Unable to send push notification. "
                                           "Possible API mismatch">>,
                                 code => StatusCode, url => URL,
                                 response => Body, payload => Payload}),
                    {error, {invalid_status_code, StatusCode}};
                StatusCode ->
                    ?LOG_ERROR(#{what => push_send_failed,
                                 text => <<"Unable to send push notification">>,
                                 code => StatusCode, response => Body}),
                    {error, {invalid_status_code, StatusCode}}
            end;
        {error, Reason} ->
            ?LOG_ERROR(#{what => connection_error, reason => Reason}),
            {error, Reason}
    end.

http_notification_result(Time, ok) -> #{success_count => 1, time => Time};
http_notification_result(Time, _Error) -> #{failed_count => 1, time => Time}.

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

%% Create notification for API v2 and v3
make_notification(Notification0, Options) ->
    RequiredParameters = #{service => maps:get(<<"service">>, Options)},
    %% The full list of parameters can be found here:
    %% doc/modules/mod_push_service_mongoosepush.md#request-parameters
    %%
    %% Note that ~"tags" parameter is explicitly excluded to avoid any
    %% security issues. User should not be allowed to select pools other than
    %% prod and dev (see ~"mode" parameter description).
    OptionalKeys = [~"mode", ~"priority", ~"topic", ~"mutable_content", ~"time_to_live"],
    OptionalParameters = maps:with(OptionalKeys, Options),
    NotificationParams = maps:merge(RequiredParameters, OptionalParameters),

    Notification = normalize_notification(Notification0),
    DataOrAlert = data_or_alert(Notification, Options),
    JSON = maps:merge(NotificationParams, DataOrAlert),
    jiffy:encode(JSON).

-spec data_or_alert(map(), mod_event_pusher_push:form()) -> #{data | alert := map()}.
data_or_alert(#{~"type" := ~"jmi"} = Notification, _Options) ->
    #{data => Notification};
data_or_alert(Notification, #{~"silent" := ~"true"}) ->
    #{data => Notification};
data_or_alert(Notification, Options) ->
    BasicAlert = #{body => maps:get(~"last-message-body", Notification),
                   title => maps:get(~"last-message-sender", Notification),
                   tag => maps:get(~"last-message-sender", Notification),
                   badge => maps:get(~"message-count", Notification)},
    OptionalAlert = maps:with([~"click_action", ~"sound"], Options),
    #{alert => maps:merge(BasicAlert, OptionalAlert)}.

%% at the moment there's only one field to normalize
normalize_notification(#{~"message-count" := MC} = N) when is_binary(MC) ->
    N#{~"message-count" := binary_to_integer(MC)};
normalize_notification(Notification) -> Notification.

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(Host) ->
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, [api_version]).

-spec async_sender(mongooseim:host_type()) -> boolean().
async_sender(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, [async_sender, enabled]).
