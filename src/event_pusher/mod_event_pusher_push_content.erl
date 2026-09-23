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
    case message_body(Packet) of
        {ok, Body} ->
            MessageCount = get_unread_count(Acc, To),
            SenderId = sender_id(From, Packet),
            {ok, content(SenderId, Body, MessageCount)};
        Error ->
            Error
    end;
build(jingle, Acc) ->
    {From, _To, Packet} = mongoose_acc:packet(Acc),
    case jingle_session_id(Packet) of
        {ok, SessionId} ->
            {ok, jingle_content(jid:to_binary(From), SessionId)};
        Error ->
            Error
    end.

-spec message_body(exml:element()) -> {ok, binary()} | {error, missing_message_body}.
message_body(Packet) ->
    case exml_query:subelement(Packet, ~"body") of
        undefined ->
            {error, missing_message_body};
        Body ->
            {ok, exml_query:cdata(Body)}
    end.

-spec jingle_session_id(exml:element()) ->
          {ok, binary()} | {error, invalid_jingle_element | missing_jingle_element}.
jingle_session_id(Packet) ->
    case exml_query:subelement_with_ns(Packet, ?JINGLE_MSG_NS) of
        #xmlel{attrs = #{~"id" := Id}} ->
            {ok, Id};
        #xmlel{} ->
            {error, invalid_jingle_element};
        _ ->
            {error, missing_jingle_element}
    end.

-spec get_unread_count(mongoose_acc:t(), jid:jid()) -> non_neg_integer().
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

-spec content(binary(), binary(), non_neg_integer()) -> content().
content(SenderId, BodyCData, MessageCount) ->
    #{~"message-count" => integer_to_binary(MessageCount),
      ~"last-message-sender" => SenderId,
      ~"last-message-body" => BodyCData}.

-spec jingle_content(binary(), binary()) -> content().
jingle_content(SenderId, SessionId) ->
    #{~"type" => ~"jmi",
      ~"jmi-sid" => SessionId,
      ~"jmi-from" => SenderId}.
