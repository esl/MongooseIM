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

-export([handle_outgoing_message/4, handle_incoming_message/4]).

-type packet() :: exml:element().

-spec handle_outgoing_message(Host :: jid:server(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet()) -> ok.
handle_outgoing_message(Host, User, Remote, Packet) ->
    maybe_reset_unread_count(Host, User, Remote, Packet),
    maybe_write_to_inbox(Host, User, Remote, Packet, fun write_to_sender_inbox/4).

-spec handle_incoming_message(Host :: jid:server(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet()) -> ok | {ok, integer()}.
handle_incoming_message(Host, User, Remote, Packet) ->
    maybe_write_to_inbox(Host, User, Remote, Packet, fun write_to_receiver_inbox/4).

maybe_reset_unread_count(Host, User, Remote, Packet) ->
    mod_inbox_utils:maybe_reset_unread_count(Host, User, Remote, Packet).

maybe_write_to_inbox(Host, User, Remote, Packet, WriteF) ->
    mod_inbox_utils:maybe_write_to_inbox(Host, User, Remote, Packet, WriteF).

write_to_sender_inbox(Server, User, Remote, Packet) ->
    mod_inbox_utils:write_to_sender_inbox(Server, User, Remote, Packet).

write_to_receiver_inbox(Server, User, Remote, Packet) ->
    mod_inbox_utils:write_to_receiver_inbox(Server, User, Remote, Packet).
