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
-include("mod_inbox.hrl").
-include("jlib.hrl").
-include("mongoose_ns.hrl").

-export([handle_outgoing_message/5, handle_incoming_message/5]).

-type packet() :: exml:element().

-spec handle_outgoing_message(Host :: jid:server(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet(),
                              TS :: integer()) -> ok.
handle_outgoing_message(Host, User, Remote, Packet, TS) ->
    maybe_reset_unread_count(Host, User, Remote, Packet),
    maybe_write_to_inbox(Host, User, Remote, Packet, TS, fun write_to_sender_inbox/5).

-spec handle_incoming_message(Host :: jid:server(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet(),
                              TS :: integer()) -> ok | {ok, integer()}.
handle_incoming_message(Host, User, Remote, Packet, TS) ->
    maybe_write_to_inbox(Host, User, Remote, Packet, TS, fun write_to_receiver_inbox/5).

maybe_reset_unread_count(Host, User, Remote, Packet) ->
    mod_inbox_utils:maybe_reset_unread_count(Host, User, Remote, Packet).

maybe_write_to_inbox(Host, User, Remote, Packet, TS, WriteF) ->
    mod_inbox_utils:maybe_write_to_inbox(Host, User, Remote, Packet, TS, WriteF).

write_to_sender_inbox(Server, User, Remote, Packet, TS) ->
    mod_inbox_utils:write_to_sender_inbox(Server, User, Remote, Packet, TS).

write_to_receiver_inbox(Server, User, Remote, Packet, TS) ->
    mod_inbox_utils:write_to_receiver_inbox(Server, User, Remote, Packet, TS).
