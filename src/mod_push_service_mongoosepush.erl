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
-include("jlib.hrl").

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

%% gen_mod handlers
-export([start/2, stop/1]).

%% Hooks and IQ handlers
-export([push_notifications/4]).

-export([http_notification/5]).

%% Types
-export_type([]).

-export([config_metrics/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(DEFAULT_API_VERSION, "v3").

-callback init(term(), term()) -> ok.

%% Types

%%--------------------------------------------------------------------
%% Module callbacks
%%--------------------------------------------------------------------

-spec start(Host :: jid:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?INFO_MSG("mod_push_service starting on host ~p", [Host]),

    MaxHTTPConnections = gen_mod:get_opt(max_http_connections, Opts, 100),
    {ok, _} = mongoose_wpool:start(generic, Host, mongoosepush_service,
                                   [{strategy, available_worker},
                                    {workers, MaxHTTPConnections}]),

    %% Hooks
    ejabberd_hooks:add(push_notifications, Host, ?MODULE, push_notifications, 10),

    ok.

-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(push_notifications, Host, ?MODULE, push_notifications, 10),
    mongoose_wpool:stop(generic, Host, mongoosepush_service),

    ok.

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------

%% Hook 'push_notifications'
-spec push_notifications(AccIn :: ok, Host :: jid:server(),
                         Notifications :: [#{binary() => binary()}],
                         Options :: #{binary() => binary()}) ->
    ok | {error, Reason :: term()}.
push_notifications(_AccIn, Host, Notifications, Options) ->
    ?DEBUG("push_notifications ~p", [{Notifications, Options}]),

    DeviceId = maps:get(<<"device_id">>, Options),
    ProtocolVersionOpt = gen_mod:get_module_opt(Host, ?MODULE, api_version, ?DEFAULT_API_VERSION),
    {ok, ProtocolVersion} = parse_api_version(ProtocolVersionOpt),
    Path = <<ProtocolVersion/binary, "/notification/", DeviceId/binary>>,
    Fun = fun(Notification) ->
            ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
            {ok, JSON} =
                make_notification(Notification, Options),
            Payload = jiffy:encode(JSON),
            call(Host, ?MODULE, http_notification, [Host, post, Path, ReqHeaders, Payload])
        end,
    send_push_notifications(Notifications, Fun, ok).

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
    PoolName = gen_mod:get_module_opt(Host, ?MODULE, pool_name, undefined),
    case mongoose_http_client:Method(Host, PoolName, URL, ReqHeaders, Payload) of
        {ok, {BinStatusCode, Body}} ->
            case binary_to_integer(BinStatusCode) of
                StatusCode when StatusCode >= 200 andalso StatusCode < 300 ->
                    ok;
                410 ->
                    ?WARNING_MSG("issue=unable_to_submit_push_notification, https_status=410, reason=device_not_registered", []),
                    {error, device_not_registered};
                StatusCode when StatusCode >= 400 andalso StatusCode < 500  ->
                    ?ERROR_MSG("issue=unable_to_submit_push_notification, http_status=~p, url=~p, response=~p, "
                               "details=\"Possible API mismatch\", payload=~p",
                               [StatusCode, URL, Body, Payload]),
                    {error, {invalid_status_code, StatusCode}};
                StatusCode ->
                    ?ERROR_MSG("issue=unable_to_submit_push_notification, http_status=~p, response=~p",
                               [StatusCode, Body]),
                    {error, {invalid_status_code, StatusCode}}
            end;
        {error, Reason} ->
            ?ERROR_MSG("Unable to communicate to MongoosePush service due to ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

parse_api_version("v3") ->
    {ok, <<"v3">>};
parse_api_version("v2") ->
    {ok, <<"v2">>};
parse_api_version(_) ->
    {error, not_supported}.

%% Create notification for API v2 and v3
make_notification(Notification, Options = #{<<"silent">> := <<"true">>}) ->
    MessageCount = binary_to_integer(maps:get(<<"message-count">>, Notification)),
    {ok, #{
        service => maps:get(<<"service">>, Options),
        mode => maps:get(<<"mode">>, Options, <<"prod">>),
        topic => maps:get(<<"topic">>, Options, null),
        data => Notification#{<<"message-count">> => MessageCount},
        priority => maps:get(<<"priority">>, Options, normal)
    }};
make_notification(Notification, Options) ->
    {ok, #{
        service => maps:get(<<"service">>, Options),
        mode => maps:get(<<"mode">>, Options, <<"prod">>),
        alert => #{
            body => maps:get(<<"last-message-body">>, Notification),
            title => maps:get(<<"last-message-sender">>, Notification),
            tag => maps:get(<<"last-message-sender">>, Notification),
            badge => binary_to_integer(maps:get(<<"message-count">>, Notification)),
            click_action => maps:get(<<"click_action">>, Options, null)
        },
        topic => maps:get(<<"topic">>, Options, null),
        priority => maps:get(<<"priority">>, Options, normal)
    }}.

-spec call(Host :: jid:server(), M :: atom(), F :: atom(), A :: [any()]) -> any().
call(Host, M, F, A) ->
    mongoose_wpool:call(generic, Host, mongoosepush_service, {M, F, A}).

config_metrics(Host) ->
    OptsToReport = [{api_version, ?DEFAULT_API_VERSION}], %list of tuples {option, defualt_value}
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).
