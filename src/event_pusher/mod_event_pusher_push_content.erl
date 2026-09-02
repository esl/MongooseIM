-module(mod_event_pusher_push_content).
-moduledoc "Builds push notification content".

-include("jlib.hrl").

-export([build/2]).

-type content() :: [{binary(), binary()}].
-export_type([content/0]).

-spec build(message | jingle, mongoose_acc:t()) -> content() | skip.
build(message, Acc) ->
    {From, To, Packet} = mongoose_acc:packet(Acc),
    case exml_query:subelement(Packet, <<"body">>) of
        undefined -> skip;
        Body ->
            BodyCData = exml_query:cdata(Body),
            MessageCount = get_unread_count(Acc, To),
            SenderId = sender_id(From, Packet),
            push_content_fields(SenderId, BodyCData, MessageCount)
    end;
build(jingle, Acc) ->
    {From, _To, Packet} = mongoose_acc:packet(Acc),
    SenderId = jid:to_bare_binary(jid:to_lower(From)),
    case exml_query:path(Packet, [{element_with_ns, ?JINGLE_MSG_NS}]) of
        #xmlel{name = Action, attrs = #{<<"id">> := Id}} ->
            [{<<"message-sender">>, SenderId},
             {<<"jingle-message">>, Action},
             {<<"jingle-session-id">>, Id}];
        _ -> skip
    end.

-spec get_unread_count(mongoose_acc:t(), jid:jid()) -> pos_integer().
get_unread_count(Acc, To) ->
    NewAcc = mongoose_hooks:inbox_unread_count(Acc, To),
    mongoose_acc:get(inbox, unread_count, 1, NewAcc).

-spec sender_id(jid:jid(), exml:element()) -> binary().
sender_id(From, Packet) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"chat">> ->
            jid:to_bare_binary(jid:to_lower(From));
        <<"groupchat">> ->
            jid:to_binary(jid:to_lower(From))
    end.

-spec push_content_fields(binary(), binary(), non_neg_integer()) -> content() | skip.
push_content_fields(_SenderId, <<"">>, _MessageCount) ->
    skip;
push_content_fields(SenderId, BodyCData, MessageCount) ->
    [
        {<<"message-count">>, integer_to_binary(MessageCount)},
        {<<"last-message-sender">>, SenderId},
        {<<"last-message-body">>, BodyCData}
    ].
