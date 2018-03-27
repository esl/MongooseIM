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

-export([handle_message/4]).

handle_message(Host, User, Remote, Packet) ->
  Markers = mod_inbox_utils:get_reset_markers(Host),
  case mod_inbox_utils:has_chat_marker(Packet, Markers) of
    true ->
      maybe_reset_unread_count(User, Remote, Packet);
    false ->
      FromBin = jid:to_binary(User),
      Packet2 = mod_inbox_utils:add_from(Packet, FromBin),
      write_to_inbox(User, Remote, Packet2)
  end.


maybe_reset_unread_count(User, Remote, Packet) ->
  Id = mod_inbox_utils:get_markered_msg_id(Packet),
  case Id of
    no_id ->
      ok;
    _ ->
      mod_inbox_utils:reset_unread_count(User, Remote, Id)
  end.


write_to_inbox(User, Remote, Packet) ->
  Server = User#jid.lserver,
  MsgId = mod_inbox_utils:get_msg_id(Packet),
  BareLocJID = jid:to_bare(User),
  mod_inbox_utils:write_to_sender_inbox(Server, User, Remote, BareLocJID, MsgId, Packet),
  mod_inbox_utils:write_to_receiver_inbox(Server, User, Remote, BareLocJID, MsgId, Packet).