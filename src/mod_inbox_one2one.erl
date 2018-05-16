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

-export([handle_outgoing_message/4, handle_incomming_message/4]).

-type packet() :: exml:element().

-spec handle_outgoing_message(Host :: jid:server(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet()) -> any().
handle_outgoing_message(Host, User, Remote, Packet) ->
  Markers = mod_inbox_utils:get_reset_markers(Host),
  case mod_inbox_utils:has_chat_marker(Packet, Markers) of
    true ->
      maybe_reset_unread_count(User, Remote, Packet);
    false ->
      FromBin = jid:to_binary(User),
      Packet2 = mod_inbox_utils:fill_from_attr(Packet, FromBin),
      write_to_inbox(User, Remote, Packet2)
  end.

handle_incomming_message(Host, User, Remote, Packet) ->
  Markers = mod_inbox_utils:get_reset_markers(Host),
  case mod_inbox_utils:has_chat_marker(Packet, Markers) of
    true ->
      %% do not save chat markers
      ok;
    false ->
      MsgId = mod_inbox_utils:get_msg_id(Packet),
      FromBin = jid:to_binary(User),
      Packet2 = mod_inbox_utils:fill_from_attr(Packet, FromBin),
      mod_inbox_utils:write_to_receiver_inbox(Host, User, Remote, MsgId, Packet2)
  end.

-spec maybe_reset_unread_count(User :: jid:jid(),
                               Remote :: jid:jid(),
                               Packet :: exml:element()) -> ok.
maybe_reset_unread_count(User, Remote, Packet) ->
  Id = mod_inbox_utils:get_markered_msg_id(Packet),
  case Id of
    no_id ->
      ok;
    _ ->
      mod_inbox_utils:reset_unread_count(User, Remote, Id)
  end.

-spec write_to_inbox(User :: jid:jid(),
                     Remote :: jid:jid(),
                     Packet :: exml:element()) -> ok.
write_to_inbox(User, Remote, Packet) ->
  Server = User#jid.lserver,
  MsgId = mod_inbox_utils:get_msg_id(Packet),
  mod_inbox_utils:write_to_sender_inbox(Server, User, Remote, MsgId, Packet).