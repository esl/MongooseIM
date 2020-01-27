%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin behaviour module for mod_event_pusher_push.
%%% This module defines API for some dynamic customizations.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push_plugin).
-author('rafal.slota@erlang-solutions.com').

-include("jlib.hrl").
-include("mongoose.hrl").

-define(DEFAULT_PLUGIN_MODULE, mod_event_pusher_push_plugin_defaults).

%% API
-export([init/1,
         should_publish/4,
         prepare_notification/3,
         publish_notification/5]).


%% @doc used for filtering push notifications. A push notification is triggered for a given
%% message only if this callback returns `true`.
-callback should_publish(Acc :: mongooseim_acc:t(),
                         Event :: mod_event_pusher:event(),
                         Services :: [mod_event_pusher_push:publish_service()]) ->
    [mod_event_pusher_push:publish_service()].

%% @doc a separate interface for rejecting the event publishing (e.g. when
%% message doesn't have a body) or creating push notification payload.
-callback prepare_notification(Acc :: mongooseim_acc:t(),
                               Event :: mod_event_pusher:event()) ->
    push_payload() | skip.

%% @doc does the actual push. By default it pushes to the registered pubsub
%% nodes (or executes the internal hook in case of a publish to a virtual domain).
-callback publish_notification(Acc :: mongooseim_acc:t(),
                               Event :: mod_event_pusher:event(),
                               Payload :: push_payload(),
                               Services :: [mod_event_pusher_push:publish_service()]) ->
    mongooseim_acc:t().

-optional_callbacks([should_publish/3, prepare_notification/2, publish_notification/4]).

-type push_payload() :: mod_event_pusher_push:form().
-export_type([push_payload/0]).
%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-spec init(Host :: jid:server()) -> ok.
init(Host) ->
    PluginModule = plugin_module(Host),
    ensure_loaded(PluginModule),
    ok.

-spec should_publish(Host :: jid:server(), Acc :: mongooseim_acc:t(),
                     Event :: mod_event_pusher:event(),
                     [mod_event_pusher_push:publish_service()]) -> [mod_event_pusher_push:publish_service()].
should_publish(Host, From, To, Packet) ->
    PluginModule = plugin_module(Host, should_publish, 3),
    PluginModule:should_publish(From, To, Packet).

-spec prepare_notification(Host :: jid:server(), Acc :: mongooseim_acc:t(),
                           Event :: mod_event_pusher:event()) -> push_payload() | skip.
prepare_notification(Host, Acc, Event) ->
    PluginModule = plugin_module(Host, prepare_notification, 2),
    PluginModule:prepare_notification(Acc, Event).

-spec publish_notification(Host :: jid:server(), Acc :: mongooseim_acc:t(),
                           Event :: mod_event_pusher:event(), Payload :: push_payload(),
                           Services :: [mod_event_pusher_push:publish_service()]) -> mongooseim_acc:t().
publish_notification(_Host, Acc, _Event, _Payload, []) ->
    Acc;
publish_notification(Host, Acc, Event, Payload, Services) ->
    PluginModule = plugin_module(Host, publish_notification, 4),
    PluginModule:publish_notification(Acc, Event, Payload, Services).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------
-spec plugin_module(Host :: jid:server(), atom(), arity()) -> Module :: atom().
plugin_module(Host, Func, Arity) ->
    Mod = plugin_module(Host),
    case erlang:function_exported(Mod, Func, Arity) of
        true -> Mod;
        false -> ?DEFAULT_PLUGIN_MODULE
    end.

-spec plugin_module(Host :: jid:server()) -> Module :: atom().
plugin_module(Host) ->
    gen_mod:get_module_opt(Host, mod_event_pusher_push, plugin_module,
                           ?DEFAULT_PLUGIN_MODULE).

-spec ensure_loaded(module()) -> {module, module()}.
ensure_loaded(PluginModule) ->
    {module, PluginModule} = code:ensure_loaded(PluginModule).
