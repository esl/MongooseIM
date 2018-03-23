%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox_operations).
-author("ludwikbukowski").
-include("jlib.hrl").

-export([reset_unread_count/3, write_to_inbox/3, write_to_sender_inbox/6, write_to_receiver_inbox/6, clear_inbox/2]).

reset_unread_count(From, To, MsgId) ->
  FromJid = jid:to_binary(jid:to_bare(From)),
  Server = From#jid.lserver,
  ToBareJid = jid:to_binary(jid:to_bare(To)),
  mod_inbox_backend:reset_unread(FromJid, Server, ToBareJid, MsgId).

write_to_inbox(LocJID, RemJID, Packet) ->
  Server = LocJID#jid.lserver,
  MsgId = mod_inbox_utils:get_msg_id(Packet),
  BareLocJID = jid:to_bare(LocJID),
  write_to_sender_inbox(Server, LocJID, RemJID, BareLocJID, MsgId, Packet),
  write_to_receiver_inbox(Server, LocJID, RemJID, BareLocJID, MsgId, Packet).

write_to_sender_inbox(Server, From, To, Sender, MsgId, Packet) ->
  Content = exml:to_binary(Packet),
  FromJid = jid:to_binary(jid:to_bare(From)),
  ToBareJid = jid:to_binary(jid:to_bare(To)),
  SenderBin = jid:to_binary(Sender),
  %% no unread for a user because he writes new messages which assumes he read all previous messages.
  Count = integer_to_binary(0),
  mod_inbox_backend:set_inbox(FromJid, Server, ToBareJid, SenderBin, Content, Count, MsgId).

write_to_receiver_inbox(Server, From, To, Sender, MsgId, Packet) ->
  Content = exml:to_binary(Packet),
  FromJid = jid:to_binary(jid:to_bare(To)),
  ToBareJid = jid:to_binary(jid:to_bare(From)),
  SenderBin = jid:to_binary(Sender),
  mod_inbox_backend:set_inbox_incr_unread(FromJid, Server, ToBareJid, SenderBin, Content, MsgId).

clear_inbox(Username, Server) ->
  mod_inbox_backend:clear_inbox(Username, Server).
