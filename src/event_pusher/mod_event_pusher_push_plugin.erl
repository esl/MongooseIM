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

%% API
-export([init/2,
         should_publish/3,
         prepare_notification/2,
         publish_notification/4,
         default_plugin_module/0]).

-callback should_publish(Acc :: mongoose_acc:t(),
                         Event :: mod_event_pusher:event(),
                         Services :: [mod_event_pusher_push:publish_service()]) ->
    [mod_event_pusher_push:publish_service()].

-callback prepare_notification(Acc :: mongoose_acc:t(),
                               Event :: mod_event_pusher:event()) ->
    push_payload() | skip.

-callback publish_notification(Acc :: mongoose_acc:t(),
                               Event :: mod_event_pusher:event(),
                               Payload :: push_payload(),
                               Services :: [mod_event_pusher_push:publish_service()]) ->
    mongoose_acc:t().

-optional_callbacks([should_publish/3, prepare_notification/2, publish_notification/4]).

-type push_payload() :: [{binary(), binary()}].
-export_type([push_payload/0]).
%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_HostType, #{plugin_module := PluginModule}) ->
    ensure_loaded(PluginModule),
    ok.

%% @doc used for filtering push notifications. A push notification is triggered for a given
%% message only if this callback returns `true'.
-spec should_publish(mongoose_acc:t(), mod_event_pusher:event(),
                     [mod_event_pusher_push:publish_service()]) ->
          [mod_event_pusher_push:publish_service()].
should_publish(Acc, Event, Services) ->
    HostType = mongoose_acc:host_type(Acc),
    PluginModule = plugin_module(HostType, should_publish, 3),
    PluginModule:should_publish(Acc, Event, Services).

%% @doc a separate interface for rejecting the event publishing (e.g. when
%% message doesn't have a body) or creating push notification payload.
-spec prepare_notification(Acc :: mongoose_acc:t(),
                           Event :: mod_event_pusher:event()) -> push_payload() | skip.
prepare_notification(Acc, Event) ->
    HostType = mongoose_acc:host_type(Acc),
    PluginModule = plugin_module(HostType, prepare_notification, 2),
    PluginModule:prepare_notification(Acc, Event).

%% @doc does the actual push. By default it pushes to the registered pubsub
%% nodes (or executes the internal hook in case of a publish to a virtual domain).
-spec publish_notification(Acc :: mongoose_acc:t(),
                           Event :: mod_event_pusher:event(), Payload :: push_payload(),
                           Services :: [mod_event_pusher_push:publish_service()]) -> mongoose_acc:t().
publish_notification(Acc, _Event, _Payload, []) ->
    Acc;
publish_notification(Acc, Event, Payload, Services) ->
    HostType = mongoose_acc:host_type(Acc),
    PluginModule = plugin_module(HostType, publish_notification, 4),
    PluginModule:publish_notification(Acc, Event, Payload, Services).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------
-spec plugin_module(mongooseim:host_type(), atom(), arity()) -> module().
plugin_module(HostType, Func, Arity) ->
    Mod = plugin_module(HostType),
    case erlang:function_exported(Mod, Func, Arity) of
        true -> Mod;
        false -> default_plugin_module()
    end.

-spec plugin_module(mongooseim:host_type()) -> module().
plugin_module(HostType) ->
    gen_mod:get_module_opt(HostType, mod_event_pusher_push, plugin_module).

default_plugin_module() ->
    mod_event_pusher_push_plugin_defaults.

-spec ensure_loaded(module()) -> {module, module()}.
ensure_loaded(PluginModule) ->
    {module, PluginModule} = code:ensure_loaded(PluginModule).
