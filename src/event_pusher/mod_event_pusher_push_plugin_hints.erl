-module(mod_event_pusher_push_plugin_hints).
-behaviour(mod_event_pusher_push_plugin).

-include("mod_event_pusher_events.hrl").

%% API
-export([should_publish/3, prepare_notification/2]).

%%--------------------------------------------------------------------
%% mod_event_pusher_push_plugin callbacks
%%--------------------------------------------------------------------

-spec prepare_notification(Acc :: mongoose_acc:t(),
                           Event :: mod_event_pusher:event()) ->
                              mod_event_pusher_push_plugin:push_payload() | skip.
prepare_notification(Acc, Event) ->
    {_From, _To, Packet} = mongoose_acc:packet(Acc),
    NoStore = exml_query:path(Packet, [{element_with_ns, <<"no-store">>, ?NS_HINTS}], false),
    case NoStore of
        false -> maybe_prepare_notification(Acc, Event);
        _ -> skip
    end.

-spec should_publish(Acc :: mongoose_acc:t(),
                     Event :: mod_event_pusher:event(),
                     Services :: [mod_event_pusher_push:publish_service()]) ->
                        [mod_event_pusher_push:publish_service()].
should_publish(Acc, Event, Services) ->
    mod_event_pusher_push_plugin_enhanced:should_publish(Acc, Event, Services).

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------

maybe_prepare_notification(Acc, Event) ->
    case mod_event_pusher_push_plugin_defaults:prepare_notification(Acc, Event) of
        %% if message is skipped by default plugin then it must be bodiless
        skip -> maybe_prepare_bodiless_notification(Acc, Event);
        Notification -> Notification
    end.

maybe_prepare_bodiless_notification(Acc, _Event) ->
    %% send notification for bodiless message only if it has store hint
    {_From, _To, Packet} = mongoose_acc:packet(Acc),
    maybe
        #xmlel{} ?= exml_query:subelement_with_name_and_ns(Packet, ~"store", ?NS_HINTS),
        {ok, Content} ?= mod_event_pusher_push_content:build(jingle, Acc),
        Content
    else
        _ -> skip
    end.
