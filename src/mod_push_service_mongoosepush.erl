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
-behavior(gen_mod).
-behaviour(mongoose_module_metrics).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

%% gen_mod handlers
-export([start/2, stop/1, config_spec/0]).

%% Hooks and IQ handlers
-export([push_notifications/3]).

-export([http_notification/5]).

-export([config_metrics/1]).

-ignore_xref([http_notification/5]).

%%--------------------------------------------------------------------
%% Module callbacks
%%--------------------------------------------------------------------

-spec start(Host :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> any().
start(Host, Opts) ->
    ?LOG_INFO(#{what => push_service_starting, server => Host}),
    start_pool(Host, Opts),
    %% Hooks
    gen_hook:add_handlers(hooks(Host)),
    ok.

-spec start_pool(mongooseim:host_type(), gen_mod:module_opts()) -> term().
start_pool(Host, Opts) ->
    {ok, _} = mongoose_wpool:start(generic, Host, mongoosepush_service, pool_opts(Opts)).

-spec pool_opts(gen_mod:module_opts()) -> mongoose_wpool:pool_opts().
pool_opts(#{max_http_connections := MaxHTTPConnections}) ->
    [{strategy, available_worker},
     {workers, MaxHTTPConnections}].

-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    gen_hook:delete_handlers(hooks(Host)),
    mongoose_wpool:stop(generic, Host, mongoosepush_service),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"pool_name">> => #option{type = atom,
                                            validate = pool_name},
                 <<"api_version">> => #option{type = binary,
                                              validate = {enum, [<<"v2">>, <<"v3">>]}},
                 <<"max_http_connections">> => #option{type = integer,
                                                       validate = non_negative}
                },
       defaults = #{<<"pool_name">> => undefined,
                    <<"api_version">> => <<"v3">>,
                    <<"max_http_connections">> => 100}
      }.

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{push_notifications, HostType, fun ?MODULE:push_notifications/3, #{}, 10}].

%% Hook 'push_notifications'
-spec push_notifications(Acc, Params, Extra) -> {ok, ok | {error, Reason :: term()}} when
    Acc :: ok | mongoose_acc:t(),
    Params :: #{notification_forms := [#{atom() => binary()}], options := #{binary() => binary()}},
    Extra :: gen_hook:extra().
push_notifications(AccIn,
                   #{notification_forms := Notifications, options := Options = #{<<"device_id">> := DeviceId}},
                   #{host_type := Host}) ->
    ?LOG_DEBUG(#{what => push_notifications, notifications => Notifications,
                 opts => Options, acc => AccIn}),

    ProtocolVersion = gen_mod:get_module_opt(Host, ?MODULE, api_version),
    Path = <<ProtocolVersion/binary, "/notification/", DeviceId/binary>>,
    Fun = fun(Notification) ->
            ReqHeaders = [{<<"content-type">>, <<"application/json">>}],
            {ok, JSON} =
                make_notification(Notification, Options),
            Payload = jiffy:encode(JSON),
            call(Host, ?MODULE, http_notification, [Host, post, Path, ReqHeaders, Payload])
        end,
    {ok, send_push_notifications(Notifications, Fun, ok)}.

send_push_notifications([], _, Result) ->
    Result;
send_push_notifications([Notification | Notifications], Fun, Result) ->
    case Fun(Notification) of
        {ok, ok} ->
            send_push_notifications(Notifications, Fun, Result);
        {ok, {error, device_not_registered} = Err} ->
            %% In this case there is no point in sending other push notifications
            %% so we can finish immediately
            Err;
        {ok, Other} ->
            %% The publish IQ allows to put more notifications into one request
            %% but this is currently not used in MongooseIM.
            %% In case it's used we try sending other notifications
            send_push_notifications(Notifications, Fun, Other)
    end.


%%--------------------------------------------------------------------
%% Module API
%%--------------------------------------------------------------------

-spec http_notification(Host :: jid:server(), post,
                        binary(), proplists:proplist(), binary()) ->
    ok | {error, Reason :: term()}.
http_notification(Host, Method, URL, ReqHeaders, Payload) ->
    PoolName = gen_mod:get_module_opt(Host, ?MODULE, pool_name),
    case mongoose_http_client:Method(Host, PoolName, URL, ReqHeaders, Payload) of
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

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

%% Create notification for API v2 and v3
make_notification(Notification, Options) ->
    RequiredParameters = #{service => maps:get(<<"service">>, Options)},
    %% The full list of supported optional parameters can be found here:
    %%    https://github.com/esl/MongoosePush/blob/master/README.md#request
    %%
    %% Note that <<"tags">> parameter is explicitly excluded to avoid any
    %% security issues. User should not be allowed to select pools other than
    %% prod and dev (see <<"mode">> parameter description).
    OptionalKeys = [<<"mode">>, <<"priority">>, <<"topic">>,
                    <<"mutable_content">>, <<"time_to_live">>],
    OptionalParameters = maps:with(OptionalKeys, Options),
    NotificationParams = maps:merge(RequiredParameters, OptionalParameters),

    MessageCount = binary_to_integer(maps:get(<<"message-count">>, Notification)),

    DataOrAlert = case Options of
                      #{<<"silent">> := <<"true">>} ->
                          Data = Notification#{<<"message-count">> := MessageCount},
                          #{data => Data};
                      _ ->
                          BasicAlert = #{body => maps:get(<<"last-message-body">>, Notification),
                                         title => maps:get(<<"last-message-sender">>, Notification),
                                         tag => maps:get(<<"last-message-sender">>, Notification),
                                         badge => MessageCount},
                          OptionalAlert = maps:with([<<"click_action">>, <<"sound">>], Options),
                          #{alert => maps:merge(BasicAlert, OptionalAlert)}
                  end,
    {ok, maps:merge(NotificationParams, DataOrAlert)}.

-spec call(Host :: jid:server(), M :: atom(), F :: atom(), A :: [any()]) -> any().
call(Host, M, F, A) ->
    mongoose_wpool:call(generic, Host, mongoosepush_service, {M, F, A}).

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(Host) ->
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, [api_version]).
