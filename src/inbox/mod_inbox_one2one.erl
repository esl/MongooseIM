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
    Markers = mod_inbox_utils:get_reset_markers(Host),
    case mod_inbox_utils:if_chat_marker_get_id(Packet, Markers) of
        undefined ->
            FromBin = jid:to_binary(User),
            Packet2 = mod_inbox_utils:fill_from_attr(Packet, FromBin),
            Server = User#jid.lserver,
            mod_inbox_utils:write_to_sender_inbox(Server, User, Remote, Packet2);
        Id ->
            mod_inbox_utils:reset_unread_count(User, Remote, Id)
    end.
-spec handle_incoming_message(Host :: jid:server(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet()) -> ok | {ok, integer()}.
handle_incoming_message(Host, User, Remote, Packet) ->
    Markers = mod_inbox_utils:get_reset_markers(Host),
    case mod_inbox_utils:if_chat_marker_get_id(Packet, Markers) of
        undefined ->
            FromBin = jid:to_binary(User),
            Packet2 = mod_inbox_utils:fill_from_attr(Packet, FromBin),
            mod_inbox_utils:write_to_receiver_inbox(Host, User, Remote, Packet2);
        _Id ->
            %% do not store chat markers in inbox
            ok
    end.

