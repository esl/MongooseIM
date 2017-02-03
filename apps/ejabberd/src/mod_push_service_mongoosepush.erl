%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @todo: write me!
%%% @end
%%%-------------------------------------------------------------------
-module(mod_push_service_mongoosepush).
-author('rafal.slota@erlang-solutions.com').
-behavior(gen_mod).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("apns4erl/include/apns.hrl").

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

%% gen_mod handlers
-export([start/2, stop/1]).

%% Hooks and IQ handlers
-export([push_notifications/3]).

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
    wpool:start_sup_pool(?MODULE, gen_mod:get_opt(wpool, Opts, [])),

    ejabberd_hooks:add(push_notifications, Host, ?MODULE, push_notifications, 10),

    ok.

-spec stop(Host :: ejabberd:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(push_notifications, Host, ?MODULE, push_notifications, 10),

    ok.

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------

%% Hook 'push_notifications'
-spec push_notifications(AccIn :: term(), LUser :: binary(), LServer :: binary()) -> ok.
push_notifications(AccIn, Notifications, Options) ->
    ?WARNING_MSG("push_notifications ~p", [Notifications, Options]),
    AccIn.

%%--------------------------------------------------------------------
%% Module API
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

-spec cast(F :: atom(), A :: [any()]) -> any().
cast(F, A) ->
    cast(?MODULE, F, A).

-spec cast(M :: atom(), F :: atom(), A :: [any()]) -> any().
cast(M, F, A) ->
    wpool:cast(?MODULE, {M, F, A}, available_worker).
