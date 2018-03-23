%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox_muclight).
-author("ludwikbukowski").
-include("mod_muc_light.hrl").
-include("mod_inbox.hrl").
-include("jlib.hrl").

-export([maybe_handle_chat_marker/4, write_to_inbox/3]).


maybe_handle_chat_marker(Host, User, Remote, Packet) ->
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
  RoomSender = jid:nameprep(Remote#jid.resource),
  Receiver = jid:nameprep(jid:to_binary(jid:to_bare(User))),
  case RoomSender of
    Receiver ->
      Id = mod_inbox_utils:get_markered_msg_id(Packet),
      mod_inbox_operations:reset_unread_count(User, Remote, Id);
    _ ->
      %% do not reset because another user sent chat marker
      ok
  end.

write_to_inbox(User, UserFromRoom, Packet) ->
  Server = User#jid.lserver,
  MsgId = mod_inbox_utils:get_msg_id(Packet),
  Sender = jid:from_binary(UserFromRoom#jid.lresource),
  case check_system_message(Packet) of
    true ->
      case check_invitation_message(User, Packet) of
        true ->
          BareRemJID = jid:to_bare(UserFromRoom),
          mod_inbox_operations:write_to_receiver_inbox(Server, UserFromRoom, User, BareRemJID, MsgId, Packet);
        _ ->
          case check_banned_message(User, Packet) of
            true ->
              %% TODO make configurable removing conversation from inbox
              remove_inbox_row(User, Server, UserFromRoom);
            _ ->
              %% some other system muc light message
              ok
          end
      end;
    _ ->
      muclight_write_to_inbox(Server, User, UserFromRoom, Sender, MsgId, Packet)
  end.

remove_inbox_row(Username, Server, RemoteJid) ->
  UBin = jid:to_binary(jid:to_bare(Username)),
  RemoteBin = jid:to_binary(RemoteJid),
  mod_inbox_backend:remove_inbox(UBin, Server, RemoteBin).

check_system_message(Packet) ->
  NSs = [?NS_MUC_LIGHT_CONFIGURATION, ?NS_MUC_LIGHT_AFFILIATIONS],
  Filtered = [exml_query:path(Packet, [{element_with_ns, NS}], undefined) || NS <- NSs],
  lists:any(fun(undefined) -> false;(_) -> true end, Filtered).


check_invitation_message(User, Packet) ->
  AffItems = exml_query:paths(Packet, [{element_with_ns, ?NS_MUC_LIGHT_AFFILIATIONS},
    {element, <<"user">>}]),
  MList = [M || #xmlel{name = <<"user">>, attrs=[{<<"affiliation">>, <<"member">>}]} = M <- AffItems],
  OList = [O || #xmlel{name = <<"user">>, attrs=[{<<"affiliation">>, <<"owner">>}]} = O <- AffItems],
  MembersAndOwners = MList ++ OList,
  Jids = [Jid || #xmlel{children = [#xmlcdata{content = Jid}]} <- MembersAndOwners],
  UserBin = jid:to_binary(jid:to_lower(jid:to_bare(User))),
  lists:member(UserBin, Jids).

check_banned_message(User, Packet) ->
  AffItems = exml_query:paths(Packet, [{element_with_ns, ?NS_MUC_LIGHT_AFFILIATIONS},
    {element, <<"user">>}]),
  KickedList = [M || #xmlel{name = <<"user">>, attrs=[{<<"affiliation">>, <<"none">>}]} = M <- AffItems],
  Jids = [Jid || #xmlel{children = [#xmlcdata{content = Jid}]} <- KickedList],
  UserBin = jid:to_binary(jid:to_lower(jid:to_bare(User))),
  lists:member(UserBin, Jids).


muclight_write_to_inbox(Server, RemJID, LocJID, RemJID, MsgId, Packet) ->
  BareRemJID = jid:to_bare(RemJID),
  mod_inbox_operations:write_to_sender_inbox(Server, RemJID, LocJID, BareRemJID, MsgId, Packet);
muclight_write_to_inbox(Server, RemJID, LocJID, Sender, MsgId, Packet) ->
  mod_inbox_operations:write_to_receiver_inbox(Server, LocJID, RemJID, Sender, MsgId, Packet).

