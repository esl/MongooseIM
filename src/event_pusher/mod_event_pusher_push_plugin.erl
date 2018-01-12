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
-include("ejabberd.hrl").

%% API
-export([should_publish/4, sender_id/3]).


-callback should_publish(From :: jlib:jid(), To :: jlib:jid(), Packet :: exml:element()) ->
    boolean().
-callback sender_id(From :: jlib:jid(), Packet :: exml:element()) -> SenderId :: binary().

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec should_publish(Host :: jlib:server(), From :: jlib:jid(),
                     To :: jlib:jid(), Packet :: exml:element()) -> boolean().
should_publish(Host, From, To, Packet) ->
    PluginModule = plugin_module(Host),
    PluginModule:should_publish(From, To, Packet).

-spec sender_id(Host :: jlib:server(), From :: jlib:jid(), Packet :: exml:element()) ->
    SenderId :: binary().
sender_id(Host, From, Packet) ->
    PluginModule = plugin_module(Host),
    PluginModule:sender_id(From, Packet).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

-spec plugin_module(Host :: jlib:server()) -> Module :: atom().
plugin_module(Host) ->
    gen_mod:get_module_opt(Host, mod_event_pusher_push, plugin_module,
                           mod_event_pusher_push_plugin_defaults).
