%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Default plugin module for mod_event_pusher_push.
%%% This module allows for some dynamic customizations.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push_plugin_defaults).
-behaviour(mod_event_pusher_push_plugin).
-author('rafal.slota@erlang-solutions.com').

-include("mod_event_pusher_events.hrl").

%% Callback API
-export([prepare_notification/2,
         should_publish/3,
         publish_notification/4]).

%%--------------------------------------------------------------------
%% mod_event_pusher_push_plugin callbacks
%%--------------------------------------------------------------------
-spec should_publish(Acc :: mongoose_acc:t(),
                     Event :: mod_event_pusher:event(),
                     Services :: [mod_event_pusher_push:publish_service()]) ->
                        [mod_event_pusher_push:publish_service()].
should_publish(Acc, #msg_event{user_status = UserStatus}, Services) ->
    PublishedServices = mongoose_acc:get(event_pusher, published_services, [], Acc),
    case should_publish(UserStatus) of
        true -> Services -- PublishedServices;
        false -> []
    end;
should_publish(_Acc, _Event, _Services) -> [].

-spec prepare_notification(Acc :: mongoose_acc:t(),
                           Event :: mod_event_pusher:event()) ->
                              mod_event_pusher_push_plugin:push_payload() | skip.
prepare_notification(Acc, _) ->
    case mod_event_pusher_push_content:build(message, Acc) of
        {error, _} -> skip;
        {ok, Content} -> Content
    end.

-spec publish_notification(Acc :: mongoose_acc:t(),
                           Event :: mod_event_pusher:event(),
                           Payload :: mod_event_pusher_push_plugin:push_payload(),
                           Services :: [mod_event_pusher_push:publish_service()]) ->
                              mongoose_acc:t().
publish_notification(Acc, _Event, Payload, Services) ->
    mod_event_pusher_push_publisher:publish_notification(Acc, Payload, Services).

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------

-spec should_publish(ejabberd_sm:user_status()) -> boolean().
should_publish(offline) ->
    true;
should_publish({online, #{client_state := inactive}}) ->
    true;
should_publish({online, _}) ->
    false.
