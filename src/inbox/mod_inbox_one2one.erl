%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox_one2one).

-include("mod_inbox.hrl").
-include("jlib.hrl").
-include("mongoose_ns.hrl").

-export([handle_outgoing_message/5, handle_incoming_message/5]).

-type packet() :: exml:element().

-spec handle_outgoing_message(HostType :: mongooseim:host_type(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet(),
                              Acc :: mongoose_acc:t()) -> ok.
handle_outgoing_message(HostType, User, Remote, Packet, Acc) ->
    maybe_reset_unread_count(HostType, User, Remote, Packet),
    maybe_write_to_inbox(HostType, User, Remote, Packet, Acc, fun write_to_sender_inbox/5).

-spec handle_incoming_message(HostType :: mongooseim:host_type(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet(),
                              Acc :: mongoose_acc:t()) -> ok | {ok, integer()}.
handle_incoming_message(HostType, User, Remote, Packet, Acc) ->
    maybe_write_to_inbox(HostType, User, Remote, Packet, Acc, fun write_to_receiver_inbox/5).

maybe_reset_unread_count(HostType, User, Remote, Packet) ->
    mod_inbox_utils:maybe_reset_unread_count(HostType, User, Remote, Packet).

maybe_write_to_inbox(HostType, User, Remote, Packet, Acc, WriteF) ->
    mod_inbox_utils:maybe_write_to_inbox(HostType, User, Remote, Packet, Acc, WriteF).

write_to_sender_inbox(HostType, User, Remote, Packet, Acc) ->
    mod_inbox_utils:write_to_sender_inbox(HostType, User, Remote, Packet, Acc).

write_to_receiver_inbox(HostType, User, Remote, Packet, Acc) ->
    mod_inbox_utils:write_to_receiver_inbox(HostType, User, Remote, Packet, Acc).
