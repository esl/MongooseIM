%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox_one2one).
-author("ludwikbukowski").

-export([handle_outgoing_message/5, handle_incoming_message/5]).

-type packet() :: exml:element().

funfun(_HostType, false, false) ->
    ok;
funfun(HostType, {true, SenderKey, Id}, DoWriteToInbox) ->
    ok = mod_inbox_backend:reset_unread(HostType, SenderKey, Id),
    funfun(HostType, false, DoWriteToInbox);
funfun(HostType, DoResetUnread, {true, SenderKey, ReceiverKey, MsgId, Content, Timestamp, Count}) ->
    ok = mod_inbox_backend:set_inbox(HostType, SenderKey, Content, Count, MsgId, Timestamp),
    ok = mod_inbox_backend:set_inbox_incr_unread(HostType, ReceiverKey, Content, MsgId, Timestamp),
    funfun(HostType, DoResetUnread, false).

-spec handle_outgoing_message(HostType :: mongooseim:host_type(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet(),
                              Acc :: mongoose_acc:t()) -> mongoose_acc:t().
handle_outgoing_message(HostType, User, Remote, Packet, Acc) ->
    ResetMarkers = mod_inbox_utils:get_reset_markers(HostType),
    SenderKey = mod_inbox_utils:build_inbox_entry_key(User, Remote),
    DoResetUnread = case mod_inbox_utils:if_chat_marker_get_id(Packet, ResetMarkers) of
        undefined -> false;
        Id -> {true, SenderKey, Id}
    end,
    DoWriteToInbox = case mod_inbox_utils:has_chat_marker(Packet) of
        true ->
            false;
        false ->
            MsgId = mod_inbox_utils:get_msg_id(Packet),
            Packet2 = mod_inbox_utils:fill_from_attr(Packet, User),
            Content = exml:to_binary(Packet2),
            Timestamp = mongoose_acc:timestamp(Acc),
            Count = 0, %% no unread for a user because he writes new messages which assumes he read all previous messages.
            ReceiverKey = mod_inbox_utils:build_inbox_entry_key(Remote, User),
            {true, SenderKey, ReceiverKey, MsgId, Content, Timestamp, Count}
    end,
    F = fun () -> funfun(HostType, DoResetUnread, DoWriteToInbox) end,
    mongoose_rdbms:sql_dirty(HostType, F),
    mongoose_acc:set_permanent(inbox, stored, true, Acc).

-spec handle_incoming_message(HostType :: mongooseim:host_type(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet(),
                              Acc :: mongoose_acc:t()) -> mongoose_acc:t().
handle_incoming_message(HostType, User, Remote, Packet, Acc) ->
    maybe_write_to_inbox(HostType, User, Remote, Packet, Acc,
                         fun mod_inbox_utils:write_to_receiver_inbox/5).

maybe_write_to_inbox(HostType, User, Remote, Packet, Acc, WriteF) ->
    mod_inbox_utils:maybe_write_to_inbox(HostType, User, Remote, Packet, Acc, WriteF).
