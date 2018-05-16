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

-export([handle_incoming_message/4]).
-type packet() :: exml:element().
-type role() :: r_member() | r_owner() | r_none().
-type r_member() :: binary().
-type r_owner() :: binary().
-type r_none() :: binary().


-spec handle_incoming_message(Host :: jid:server(),
                     User :: jid:jid(),
                     Remote :: jid:jid(),
                     Packet :: packet()) -> any().
handle_incoming_message(Host, RoomUser, Remote, Packet) ->
  Markers = mod_inbox_utils:get_reset_markers(Host),
  case mod_inbox_utils:has_chat_marker(Packet, Markers) of
    true ->
      maybe_reset_unread_count(RoomUser, Remote, Packet);
    false ->
      maybe_handle_system_message(Host, RoomUser, Remote, Packet)
  end.


maybe_reset_unread_count(RoomUser, Remote, Packet) ->
  RoomSender = jid:nameprep(RoomUser#jid.resource),
  Room = jid:to_bare(RoomUser),
  RemoteBin = jid:nameprep(jid:to_binary(jid:to_bare(Remote))),
  Id = mod_inbox_utils:get_markered_msg_id(Packet),
  case {RoomSender, Id} of
    {_, no_id} ->
      ok;
    {RemoteBin, _} ->
      mod_inbox_utils:reset_unread_count(Remote, Room, Id);
    _ ->
      %% do not reset because another user sent chat marker
      ok
  end.


maybe_handle_system_message(Host, RoomUser, Remote, Packet) ->
  MsgId = mod_inbox_utils:get_msg_id(Packet),
  Sender = jid:from_binary(RoomUser#jid.lresource),
  case is_system_message(Packet) of
    true ->
%%      ok;
      WriteAffChanges = mod_inbox_utils:get_option_write_aff_changes(Host),
      handle_system_message(Host, RoomUser, Remote, Packet, MsgId, WriteAffChanges);
    _ ->
      write_to_inbox(Host, RoomUser, Remote, Sender, MsgId, Packet)
  end.


handle_system_message(_Host, _Room, _Remote, _Packet, _MsgId, false) ->
  ok;
handle_system_message(Host, Room, Remote, Packet, MsgId, true) ->
  case {is_invitation_message(Remote, Packet), is_new_owner_message(Remote, Packet)} of
    {false, false} ->
      case is_kicked_message(Remote, Packet) of
        true ->
          handle_kicked_message(Host, Remote, Room);
        false ->
          %% some other system muc light message. Do not write to inbox.
          ok
      end;
    _ ->
      handle_invitation_message(Host, Room, Remote, Packet, MsgId)
  end.

-spec handle_invitation_message(User :: jid:jid(),
                                Server :: host(),
                                UserFromRoom ::  jid:jid(),
                                Packet ::  exml:element(),
                                MsgId ::  id()) -> ok.
handle_invitation_message(Host, Room, Remote, Packet, MsgId) ->
  mod_inbox_utils:write_to_receiver_inbox(Host, Room, Remote, MsgId, Packet).

%%-spec handle_kicked_message(User :: jid:jid(),
%%                            Server :: host(),
%%                            UserFromRoom ::  jid:jid()) -> ok.
handle_kicked_message(Host, User, Room) ->
  CheckRemove = mod_inbox_utils:get_option_remove_on_kicked(Host),
  maybe_remove_inbox_row(Host, User, Room, CheckRemove).


maybe_remove_inbox_row(_, _, _, false) ->
  ok;
maybe_remove_inbox_row(Host, User, Room, true) ->
  UserBin = (jid:to_bare(User))#jid.luser,
  RoomBin = jid:to_binary(Room),
  ok = mod_inbox_backend:remove_inbox(UserBin, Host, RoomBin).

-spec write_to_inbox(Server :: host(),
                     RemoteIsSender :: jid:jid(),
                     User :: jid:jid(),
                     RemoteIsSender :: jid:jid(),
                     MsgId :: id(),
                     Packet :: exml:packet()) -> ok.
write_to_inbox(Server, FullRoomJid, RemoteIsSender, RemoteIsSender, MsgId, Packet) ->
  mod_inbox_utils:write_to_sender_inbox(Server, RemoteIsSender, FullRoomJid, MsgId, Packet);
write_to_inbox(Server, FullRoomJid, Remote, _Sender, MsgId, Packet) ->
  mod_inbox_utils:write_to_receiver_inbox(Server, FullRoomJid, Remote, MsgId, Packet).

%%%%%%%
%% Predicate funs
-spec is_system_message(exml:packet()) -> boolean().
is_system_message(Packet) ->
  NSs = [?NS_MUC_LIGHT_CONFIGURATION, ?NS_MUC_LIGHT_AFFILIATIONS,
    ?NS_MUC_LIGHT_INFO, ?NS_MUC_LIGHT_BLOCKING, ?NS_MUC_LIGHT_DESTROY],
  Filtered = [exml_query:path(Packet, [{element_with_ns, NS}], undefined) || NS <- NSs],
  lists:any(fun(undefined) -> false;(_) -> true end, Filtered).

-spec is_change_aff_message(jid:jid(), exml:element(), role()) -> binary().
is_change_aff_message(User, Packet, Role) ->
  AffItems = exml_query:paths(Packet, [{element_with_ns, ?NS_MUC_LIGHT_AFFILIATIONS},
    {element, <<"user">>}]),
  AffList = get_users_with_affiliation(AffItems, Role),
  Jids = [Jid || #xmlel{children = [#xmlcdata{content = Jid}]} <- AffList],
  UserBin = jid:to_binary(jid:to_lower(jid:to_bare(User))),
  lists:member(UserBin, Jids).

-spec is_invitation_message(jid:jid(), exml:element()) -> true.
is_invitation_message(User, Packet) ->
  is_change_aff_message(User, Packet, <<"member">>).

-spec is_new_owner_message(jid:jid(), exml:element()) -> true.
is_new_owner_message(User, Packet) ->
  is_change_aff_message(User, Packet, <<"owner">>).

-spec is_kicked_message(jid:jid(), exml:element()) -> true.
is_kicked_message(User, Packet) ->
  is_change_aff_message(User, Packet, <<"none">>).

-spec get_users_with_affiliation(list(exml:element()), role()) -> list(exml:element()).
get_users_with_affiliation(AffItems, Role) ->
  [M || #xmlel{name = <<"user">>, attrs=[{<<"affiliation">>, R}]} = M <- AffItems, R == Role].