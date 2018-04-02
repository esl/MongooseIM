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

-export([handle_message/4]).
-type packet() :: exml:element().
-type role() :: r_member() | r_owner() | r_none().
-type r_member() :: binary().
-type r_owner() :: binary().
-type r_none() :: binary().


-spec handle_message(Host :: jid:server(),
                     User :: jid:jid(),
                     Remote :: jid:jid(),
                     Packet :: packet()) -> any().
handle_message(Host, User, Remote, Packet) ->
  Markers = mod_inbox_utils:get_reset_markers(Host),
  case mod_inbox_utils:has_chat_marker(Packet, Markers) of
    true ->
      maybe_reset_unread_count(User, Remote, Packet);
    false ->
      FromBin = jid:to_binary(User),
      Packet2 = mod_inbox_utils:add_from(Packet, FromBin),
      message_check(User, Remote, Packet2)
  end.

-spec maybe_reset_unread_count(User :: jid:jid(),
                               Remote :: jid:jid(),
                               Packet :: exml:element()) -> ok.
maybe_reset_unread_count(User, Remote, Packet) ->
  RoomSender = jid:nameprep(Remote#jid.resource),
  Receiver = jid:nameprep(jid:to_binary(jid:to_bare(User))),
  Id = mod_inbox_utils:get_markered_msg_id(Packet),
  case {RoomSender, Id} of
    {_, no_id} ->
      ok;
    {Receiver, _} ->
      mod_inbox_utils:reset_unread_count(User, Remote, Id);
    _ ->
      %% do not reset because another user sent chat marker
      ok
  end.

-spec message_check(User :: jid:jid(),
                    UserFromRoom :: jid:jid(),
                    Packet :: exml:element()) -> ok.
message_check(User, UserFromRoom, Packet) ->
  Server = User#jid.lserver,
  MsgId = mod_inbox_utils:get_msg_id(Packet),
  Sender = jid:from_binary(UserFromRoom#jid.lresource),
  case check_system_message(Packet) of
    true ->
      WriteAffChanges = mod_inbox_utils:check_write_aff_changes(Server),
      handle_system_message(User, Server, UserFromRoom, Packet, MsgId, WriteAffChanges);
    _ ->
      write_to_inbox(Server, User, UserFromRoom, Sender, MsgId, Packet)
  end.

-spec handle_system_message(User :: jid:jid(),
                            Server :: host(),
                            UserFromRoom :: jid:jid(),
                            Packet :: exml:element(),
                            MsgId :: id(),
                            WriteAffChanges :: boolean()) -> ok.
handle_system_message(_User, _Server, _UserFromRoom, _Packet, _MsgId, false) ->
  ok;
handle_system_message(User, Server, UserFromRoom, Packet, MsgId, true) ->
  case {check_invitation_message(User, Packet),
        check_new_owner_message(User, Packet)} of
    {false, false} ->
      case check_kicked_message(User, Packet) of
        true ->
          handle_kicked_message(User, Server, UserFromRoom);
        false ->
          %% some other system muc light message. Do not write to inbox.
          ok
      end;
    _ ->
      handle_invitation_message(User, Server, UserFromRoom, Packet, MsgId)
  end.

-spec handle_invitation_message(User :: jid:jid(),
                                Server :: host(),
                                UserFromRoom ::  jid:jid(),
                                Packet ::  exml:element(),
                                MsgId ::  id()) -> ok.
handle_invitation_message(User, Server, UserFromRoom, Packet, MsgId) ->
  BareRemJID = jid:to_bare(UserFromRoom),
  mod_inbox_utils:write_to_receiver_inbox(Server, UserFromRoom, User, BareRemJID, MsgId, Packet).

-spec handle_kicked_message(User :: jid:jid(),
                            Server :: host(),
                            UserFromRoom ::  jid:jid()) -> ok.
handle_kicked_message(User, Server, UserFromRoom) ->
  CheckRemove = mod_inbox_utils:check_remove_on_kicked(Server),
  maybe_remove_inbox_row(User, Server, UserFromRoom, CheckRemove).

-spec maybe_remove_inbox_row(Username :: jid:jid(),
                             Server :: host(),
                             RemoteJid :: jid:jid(),
                             CheckRemote :: boolean()) -> ok.
maybe_remove_inbox_row(_, _, _ , false) ->
  ok;
maybe_remove_inbox_row(Username, Server, RemoteJid, true) ->
  UBin = jid:to_binary(jid:to_bare(Username)),
  RemoteBin = jid:to_binary(RemoteJid),
  mod_inbox_backend:remove_inbox(UBin, Server, RemoteBin).

-spec write_to_inbox(Server :: host(),
                     RemoteIsSender :: jid:jid(),
                     User :: jid:jid(),
                     RemoteIsSender :: jid:jid(),
                     MsgId :: id(),
                     Packet :: exml:packet()) -> ok.
write_to_inbox(Server, RemoteIsSender, User, RemoteIsSender, MsgId, Packet) ->
  BareRemJID = jid:to_bare(RemoteIsSender),
  mod_inbox_utils:write_to_sender_inbox(Server, RemoteIsSender, User, BareRemJID, MsgId, Packet);
write_to_inbox(Server, Remote, User, Sender, MsgId, Packet) ->
  mod_inbox_utils:write_to_receiver_inbox(Server, User, Remote, Sender, MsgId, Packet).

%%%%%%%
%% Predicate funs
-spec check_system_message(exml:packet()) -> boolean().
check_system_message(Packet) ->
  NSs = [?NS_MUC_LIGHT_CONFIGURATION, ?NS_MUC_LIGHT_AFFILIATIONS,
    ?NS_MUC_LIGHT_INFO, ?NS_MUC_LIGHT_BLOCKING, ?NS_MUC_LIGHT_DESTROY],
  Filtered = [exml_query:path(Packet, [{element_with_ns, NS}], undefined) || NS <- NSs],
  lists:any(fun(undefined) -> false;(_) -> true end, Filtered).

-spec check_change_aff_message(jid:jid(), exml:element(), role()) -> binary().
check_change_aff_message(User, Packet, Role) ->
  AffItems = exml_query:paths(Packet, [{element_with_ns, ?NS_MUC_LIGHT_AFFILIATIONS},
    {element, <<"user">>}]),
  AffList = get_users_with_affiliation(AffItems, Role),
  Jids = [Jid || #xmlel{children = [#xmlcdata{content = Jid}]} <- AffList],
  UserBin = jid:to_binary(jid:to_lower(jid:to_bare(User))),
  lists:member(UserBin, Jids).

-spec check_invitation_message(jid:jid(), exml:element()) -> true.
check_invitation_message(User, Packet) ->
  check_change_aff_message(User, Packet, <<"member">>).

-spec check_new_owner_message(jid:jid(), exml:element()) -> true.
check_new_owner_message(User, Packet) ->
  check_change_aff_message(User, Packet, <<"owner">>).

-spec check_kicked_message(jid:jid(), exml:element()) -> true.
check_kicked_message(User, Packet) ->
  check_change_aff_message(User, Packet, <<"none">>).

-spec get_users_with_affiliation(list(exml:element()), role()) -> list(exml:element()).
get_users_with_affiliation(AffItems, Role) ->
  [M || #xmlel{name = <<"user">>, attrs=[{<<"affiliation">>, Role}]} = M <- AffItems].