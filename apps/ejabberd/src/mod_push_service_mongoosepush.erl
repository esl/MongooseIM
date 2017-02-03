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

-callback init(term(), term()) -> ok.

%% Types

%%--------------------------------------------------------------------
%% Module callbacks
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?INFO_MSG("mod_push_service starting on host ~p", [Host]),

    ok = application:ensure_started(worker_pool),
    MaxHTTPConnections = gen_mod:get_opt(max_http_connections, Opts, 100),
    wpool:start_sup_pool(pool_name(Host, wpool), [{workers, MaxHTTPConnections}]),
    hackney:start(),


    PoolName = pool_name(Host, hackney),
    Options = [{timeout, gen_mod:get_opt(http_timeout, Opts, timer:minutes(3))},
               {max_connections, MaxHTTPConnections}],
    ok = hackney_pool:start_pool(PoolName, Options),

    ejabberd_hooks:add(push_notifications, Host, ?MODULE, push_notifications, 10),


    ok.

-spec stop(Host :: ejabberd:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(push_notifications, Host, ?MODULE, push_notifications, 10),
    hackney_pool:stop_pool(pool_name(Host, hackney)),
    wpool:stop_pool(pool_name(Host, wpool)),

    ok.

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------

%% Hook 'push_notifications'
-spec push_notifications(AccIn :: term(), Host :: ejabberd:server(),
                         Notifications :: [#{binary() => binary()}],
                         Options :: #{binary() => binary()}) -> ok.
push_notifications(AccIn, Host, Notifications, Options) ->
    ?WARNING_MSG("push_notifications ~p", [{Notifications, Options}]),

    ShortURL = list_to_binary(gen_mod:get_module_opt(Host, ?MODULE, endpoint,
                                                     "https://localhost:8080")),
    ProtocolVersion = list_to_binary(gen_mod:get_module_opt(Host, ?MODULE, endpoint_version, "v1")),
    DeviceId = maps:get(<<"device_id">>, Options),
    URL = <<ShortURL/binary, "/", ProtocolVersion/binary, "/notification/", DeviceId/binary>>,
    lists:foreach(
        fun(Notification) ->
            ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
            Payload = jiffy:encode(
                #{
                    service => maps:get(<<"service">>, Options),
                    body => maps:get(<<"last-message-body">>, Notification),
                    title => maps:get(<<"last-message-sender">>, Notification),
                    tag => maps:get(<<"last-message-sender">>, Notification)
                }),
            AdditionalOpts = gen_mod:get_module_opt(Host, ?MODULE, http_options, []),
            HTTPOptions = AdditionalOpts ++ [{pool, pool_name(Host, hackney)}],
            cast(Host, ?MODULE, http_notification, [post, URL, ReqHeaders, Payload, HTTPOptions])
        end, Notifications),

    AccIn.

%%--------------------------------------------------------------------
%% Module API
%%--------------------------------------------------------------------

-spec http_notification(post, binary(), proplists:proplist(), binary(), [any()]) ->
    ok | {error, Reason :: term()}.
http_notification(Method, URL, ReqHeaders, Payload, HTTPOptions) ->
    case hackney:request(Method, URL, ReqHeaders, Payload, HTTPOptions) of
        {ok, 200, _, _} -> ok;
        {ok, ErrorCode, ErrorDetails, _} ->
            ?WARNING_MSG("Unable to submit push notification. ErrorCode ~p, Details ~p",
                         [ErrorCode, ErrorDetails]),
            {error, {ErrorCode, ErrorCode}};
        {error, Reason} ->
            ?ERROR_MSG("Unable to communicate to MongoosePush service due to ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

-spec cast(Host :: ejabberd:server(), M :: atom(), F :: atom(), A :: [any()]) -> any().
cast(Host, M, F, A) ->
    wpool:cast(pool_name(Host, wpool), {M, F, A}, available_worker).

-spec pool_name(Host :: ejabberd:server(), Base0 :: atom()) -> atom().
pool_name(Host, Base0) ->
    Base = list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Base0)),
    gen_mod:get_module_proc(Host, Base).
