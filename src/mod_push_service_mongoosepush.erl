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

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

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

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(DEFAULT_API_VERSION, "v2").

-callback init(term(), term()) -> ok.

%% Types

%%--------------------------------------------------------------------
%% Module callbacks
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?INFO_MSG("mod_push_service starting on host ~p", [Host]),

    MaxHTTPConnections = gen_mod:get_opt(max_http_connections, Opts, 100),
    wpool_sup:start_pool(pool_name(Host, wpool), [{workers, MaxHTTPConnections}]),

    %% Hooks
    ejabberd_hooks:add(push_notifications, Host, ?MODULE, push_notifications, 10),

    ok.

-spec stop(Host :: ejabberd:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(push_notifications, Host, ?MODULE, push_notifications, 10),
    wpool_sup:stop_pool(pool_name(Host, wpool)),

    ok.

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------

%% Hook 'push_notifications'
-spec push_notifications(AccIn :: term(), Host :: ejabberd:server(),
                         Notifications :: [#{binary() => binary()}],
                         Options :: #{binary() => binary()}) -> ok.
push_notifications(AccIn, Host, Notifications, Options) ->
    ?DEBUG("push_notifications ~p", [{Notifications, Options}]),

    DeviceId = maps:get(<<"device_id">>, Options),
    ProtocolVersion = list_to_binary(gen_mod:get_module_opt(Host, ?MODULE, api_version,
                                                            ?DEFAULT_API_VERSION)),
    Path = <<ProtocolVersion/binary, "/notification/", DeviceId/binary>>,
    lists:foreach(
        fun(Notification) ->
            ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
            {ok, JSON} =
                make_notification(binary_to_atom(ProtocolVersion, utf8), Notification, Options),
            Payload = jiffy:encode(JSON),
            cast(Host, ?MODULE, http_notification, [Host, post, Path, ReqHeaders, Payload])
        end, Notifications),

    AccIn.

%%--------------------------------------------------------------------
%% Module API
%%--------------------------------------------------------------------

-spec http_notification(Host :: ejabberd:server(), post,
                        binary(), proplists:proplist(), binary()) ->
    ok | {error, Reason :: term()}.
http_notification(Host, Method, URL, ReqHeaders, Payload) ->
    PoolName = gen_mod:get_module_opt(Host, ?MODULE, pool_name, undefined),
    Pool = mongoose_http_client:get_pool(PoolName),
    case mongoose_http_client:Method(Pool, URL, ReqHeaders, Payload) of
        {ok, {BinStatusCode, Body}} ->
            StatusCode = binary_to_integer(BinStatusCode),
            case StatusCode >= 200 andalso StatusCode < 300 of
                true -> ok;
                false ->
                    ?WARNING_MSG("Unable to submit push notification. ErrorCode ~p, Payload ~p",
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

%% Create notification for API v2
make_notification(v2, Notification, Options = #{<<"silent">> := <<"true">>}) ->
    MessageCount = binary_to_integer(maps:get(<<"message-count">>, Notification)),
    {ok, #{
        service => maps:get(<<"service">>, Options),
        mode => maps:get(<<"mode">>, Options, <<"prod">>),
        topic => maps:get(<<"topic">>, Options, null),
        data => Notification#{<<"message-count">> => MessageCount}
    }};
make_notification(v2, Notification, Options) ->
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
        topic => maps:get(<<"topic">>, Options, null)
    }}.

-spec cast(Host :: ejabberd:server(), M :: atom(), F :: atom(), A :: [any()]) -> any().
cast(Host, M, F, A) ->
    wpool:cast(pool_name(Host, wpool), {M, F, A}, available_worker).

-spec pool_name(Host :: ejabberd:server(), Base0 :: atom()) -> atom().
pool_name(Host, Base0) ->
    Base = list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Base0)),
    gen_mod:get_module_proc(Host, Base).

