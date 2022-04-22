%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox_one2one).

-export([handle_outgoing_message/5, handle_incoming_message/5]).

-spec handle_outgoing_message(HostType :: mongooseim:host_type(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: exml:element(),
                              Acc :: mongoose_acc:t()) ->
    mod_inbox:count_res().
handle_outgoing_message(HostType, User, Remote, Packet, Acc) ->
    mod_inbox_utils:maybe_reset_unread_count(HostType, User, Remote, Packet, Acc),
    mod_inbox_utils:maybe_write_to_inbox(
      HostType, User, Remote, Packet, Acc, fun mod_inbox_utils:write_to_sender_inbox/5).

-spec handle_incoming_message(HostType :: mongooseim:host_type(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: exml:element(),
                              Acc :: mongoose_acc:t()) ->
    mod_inbox:count_res().
handle_incoming_message(HostType, User, Remote, Packet, Acc) ->
    mod_inbox_utils:maybe_write_to_inbox(
      HostType, User, Remote, Packet, Acc, fun mod_inbox_utils:write_to_receiver_inbox/5).
