-module(mod_event_pusher_push_content).
-moduledoc "Builds push notification content".

-include("jlib.hrl").

-export([build/2]).

-type content() :: #{binary() => binary()}.
-type result() :: {ok, content()} | {error, term()}.
-export_type([content/0]).

-spec build(message | jingle, mongoose_acc:t()) -> result().
build(message, Acc) ->
    {From, To, Packet} = mongoose_acc:packet(Acc),
    case exml_query:subelement(Packet, ~"body") of
        undefined ->
            {error, missing_message_body};
        Body ->
            BodyCData = exml_query:cdata(Body),
            MessageCount = get_unread_count(Acc, To),
            SenderId = sender_id(From, Packet),
            message_content(SenderId, BodyCData, MessageCount)
    end;
build(jingle, Acc) ->
    {From, _To, Packet} = mongoose_acc:packet(Acc),
    SenderId = jid:to_bare_binary(jid:to_lower(From)),
    case exml_query:path(Packet, [{element_with_ns, ?JINGLE_MSG_NS}]) of
        #xmlel{name = Action, attrs = #{~"id" := Id}} ->
            {ok, #{~"message-sender" => SenderId,
                   ~"jingle-message" => Action,
                   ~"jingle-session-id" => Id}};
        #xmlel{} ->
            {error, invalid_jingle_element};
        _ ->
            {error, missing_jingle_element}
    end.

-spec get_unread_count(mongoose_acc:t(), jid:jid()) -> pos_integer().
get_unread_count(Acc, To) ->
    NewAcc = mongoose_hooks:inbox_unread_count(Acc, To),
    mongoose_acc:get(inbox, unread_count, 1, NewAcc).

-spec sender_id(jid:jid(), exml:element()) -> binary().
sender_id(From, Packet) ->
    case exml_query:attr(Packet, ~"type") of
        ~"chat" ->
            jid:to_bare_binary(jid:to_lower(From));
        ~"groupchat" ->
            jid:to_binary(jid:to_lower(From))
    end.

-spec message_content(binary(), binary(), non_neg_integer()) -> result().
message_content(_SenderId, ~"", _MessageCount) ->
    {error, empty_message_body};
message_content(SenderId, BodyCData, MessageCount) ->
    {ok, #{~"message-count" => integer_to_binary(MessageCount),
           ~"last-message-sender" => SenderId,
           ~"last-message-body" => BodyCData}}.
