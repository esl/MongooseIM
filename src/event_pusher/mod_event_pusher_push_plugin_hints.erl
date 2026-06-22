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

maybe_prepare_bodiless_notification(Acc, Event) ->
    %% send notification for bodiless message only if it has store hint
    {_From, _To, Packet} = mongoose_acc:packet(Acc),
    Store = exml_query:path(Packet, [{element_with_ns, <<"store">>, ?NS_HINTS}], false),
    case Store of
        false -> skip;
        _ ->
            Processors = bodiless_message_notification_processors(),
            maybe_prepare_bodiless_notification(Processors, Acc, Event)
    end.

bodiless_message_notification_processors() ->
    [   %% add custom processors for different message types here
        fun maybe_jingle_message_notification/1
    ].

maybe_prepare_bodiless_notification(Processors, Acc, _Event) ->
    {From, _To, Packet} = mongoose_acc:packet(Acc),
    SenderId = jid:to_bare_binary(jid:to_lower(From)),
    Processors = bodiless_message_notification_processors(),
    case prepare_bodiless_notification(Processors, Packet) of
        skip -> skip;
        ContentFields -> [{<<"message-sender">>, SenderId} | ContentFields]
    end.

prepare_bodiless_notification([], _Packet) -> skip;
prepare_bodiless_notification([Processor | T], Packet) ->
    case Processor(Packet) of
        skip -> prepare_bodiless_notification(T, Packet);
        ContentFields -> ContentFields
    end.

maybe_jingle_message_notification(Packet) ->
    case exml_query:path(Packet, [{element_with_ns, ?JINGLE_MSG_NS}]) of
        #xmlel{name = Action, attrs = #{<<"id">> := Id}} ->
            %% This is the bare minimum that each jingle message has
            %% add sub-elements processing if necessary.
            [{<<"jingle-message">>, Action},
             {<<"jingle-session-id">>, Id}];
        _ -> skip
    end.
