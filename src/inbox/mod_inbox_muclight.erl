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
-include("jlib.hrl").
-include("mongoose.hrl").
% -include("mod_inbox.hrl").
% -include("mongoose_ns.hrl").

-export([handle_outgoing_message/5, handle_incoming_message/5]).

-ignore_xref([{mod_inbox_backend, remove_inbox_row, 2}]).

-type packet() :: exml:element().
-type role() :: r_member() | r_owner() | r_none().
-type r_member() :: binary().
-type r_owner() :: binary().
-type r_none() :: binary().

funfun(_HostType, false, false) ->
    ok;
funfun(HostType, {true, SenderKey, Id}, DoWriteToInbox) ->
    ok = mod_inbox_backend:reset_unread(HostType, SenderKey, Id),
    funfun(HostType, false, DoWriteToInbox);
funfun(HostType, DoResetUnread, {true, Affs, Sender, From, Packet, Acc}) ->
    lists:foreach(
      fun({{U, S}, _}) ->
              Receiver = jid:make_noprep(U, S, <<>>),
              Attrs1 = lists:keydelete(<<"to">>, 1, Packet#xmlel.attrs),
              Packet1 = Packet#xmlel{attrs = [{<<"to">>, jid:to_binary(Receiver)} | Attrs1]},
              write_to_inbox(HostType, From, Receiver, Sender, Packet1, Acc)
      end, Affs),
    funfun(HostType, DoResetUnread, false).

-spec handle_outgoing_message(HostType :: mongooseim:host_type(),
                              User :: jid:jid(),
                              Room :: jid:jid(),
                              Packet :: packet(),
                              Acc :: mongoose_acc:t()) -> mongoose_acc:t().
handle_outgoing_message(HostType, User, Room, Packet, Acc1) ->
    ResetMarkers = mod_inbox_utils:get_reset_markers(HostType),
    SenderKey = mod_inbox_utils:build_inbox_entry_key(User, Room),
    {Acc2, {ok, Affs, _Version}} = mod_muc_light:get_room_affiliations(Acc1, Room),
    DoResetUnread = case mod_inbox_utils:if_chat_marker_get_id(Packet, ResetMarkers) of
                        undefined -> false;
                        Id -> {true, SenderKey, Id}
                    end,
    DoWriteToInbox = case mod_inbox_utils:has_chat_marker(Packet) of
                         true -> false;
                         false ->
                             From = jid:replace_resource(Room, jid:to_binary(jid:to_lus(User))),
                             Packet2 = mod_inbox_utils:fill_from_attr(Packet, From),
                             {true, Affs, User, From, Packet2, Acc2}
                     end,
    F = fun() -> funfun(HostType, DoResetUnread, DoWriteToInbox) end,
    mongoose_rdbms:sql_dirty(HostType, F),
    mongoose_acc:set_permanent(inbox, stored, true, Acc2).

-spec handle_incoming_message(HostType :: mongooseim:host_type(),
                              RoomUser :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet(),
                              Acc :: mongoose_acc:t()) -> mongoose_acc:t().
handle_incoming_message(HostType, RoomUser, Remote, Packet, Acc) ->
    case mod_inbox_utils:has_chat_marker(Packet) of
        true ->
            Acc; %% don't store chat markers in inbox
        false ->
            maybe_handle_system_message(HostType, RoomUser, Remote, Packet, Acc)
    end.

-spec maybe_handle_system_message(HostType :: mongooseim:host_type(),
                                  RoomOrUser :: jid:jid(),
                                  Receiver :: jid:jid(),
                                  Packet :: exml:element(),
                                  Acc :: mongoose_acc:t()) -> mongoose_acc:t().
maybe_handle_system_message(HostType, RoomOrUser, Receiver, Packet, Acc) ->
    case is_system_message(HostType, RoomOrUser, Receiver, Packet) of
        true ->
            handle_system_message(HostType, RoomOrUser, Receiver, Packet, Acc);
        _ ->
            Sender = jid:from_binary(RoomOrUser#jid.lresource),
            write_to_inbox(HostType, RoomOrUser, Receiver, Sender, Packet, Acc)
    end.

-spec handle_system_message(HostType :: mongooseim:host_type(),
                            Room :: jid:jid(),
                            Remote :: jid:jid(),
                            Packet :: exml:element(),
                            Acc :: mongoose_acc:t()) -> mongoose_acc:t().
handle_system_message(HostType, Room, Remote, Packet, Acc) ->
    case system_message_type(Remote, Packet) of
        kick ->
            handle_kicked_message(HostType, Room, Remote, Packet, Acc);
        invite ->
            handle_invitation_message(HostType, Room, Remote, Packet, Acc);
        other ->
            ?LOG_DEBUG(#{what => irrelevant_system_message_for_mod_inbox_muclight,
                         room => Room, exml_packet => Packet}),
            Acc
    end.

-spec handle_invitation_message(HostType :: mongooseim:host_type(),
                                Room :: jid:jid(),
                                Remote :: jid:jid(),
                                Packet :: exml:element(),
                                Acc :: mongoose_acc:t()) -> mongoose_acc:t().
handle_invitation_message(HostType, Room, Remote, Packet, Acc) ->
    maybe_store_system_message(HostType, Room, Remote, Packet, Acc).

-spec handle_kicked_message(HostType :: mongooseim:host_type(),
                            Room :: jid:jid(),
                            Remote :: jid:jid(),
                            Packet :: exml:element(),
                            Acc :: mongoose_acc:t()) -> mongoose_acc:t().
handle_kicked_message(HostType, Room, Remote, Packet, Acc) ->
    CheckRemove = mod_inbox_utils:get_option_remove_on_kicked(HostType),
    maybe_store_system_message(HostType, Room, Remote, Packet, Acc),
    maybe_remove_inbox_row(HostType, Room, Remote, CheckRemove),
    Acc.

-spec maybe_store_system_message(HostType :: mongooseim:host_type(),
                                 Room :: jid:jid(),
                                 Remote :: jid:jid(),
                                 Packet :: exml:element(),
                                 Acc :: mongoose_acc:t()) -> mongoose_acc:t().
maybe_store_system_message(HostType, Room, Remote, Packet, Acc) ->
    WriteAffChanges = mod_inbox_utils:get_option_write_aff_changes(HostType),
    case WriteAffChanges of
        true ->
            write_to_inbox(HostType, Room, Remote, Room, Packet, Acc);
        false ->
            Acc
    end.

-spec maybe_remove_inbox_row(HostType :: mongooseim:host_type(),
                             Room :: jid:jid(),
                             Remote :: jid:jid(),
                             WriteAffChanges :: boolean()) -> ok.
maybe_remove_inbox_row(_, _, _, false) ->
    ok;
maybe_remove_inbox_row(HostType, Room, Remote, true) ->
    InboxEntryKey = mod_inbox_utils:build_inbox_entry_key(Remote, Room),
    ok = mod_inbox_backend:remove_inbox_row(HostType, InboxEntryKey).

-spec write_to_inbox(HostType :: mongooseim:host_type(),
                     RoomUser :: jid:jid(),
                     Remote :: jid:jid(),
                     Sender :: jid:jid(),
                     Packet :: exml:element(),
                     Acc :: mongoose_acc:t()) -> mongoose_acc:t().
write_to_inbox(HostType, RoomUser, Remote, Sender, Packet, Acc) ->
    case jid:are_bare_equal(Remote, Sender) of
        true ->
            mod_inbox_utils:write_to_sender_inbox(HostType, Remote, RoomUser, Packet, Acc);
        false ->
            mod_inbox_utils:write_to_receiver_inbox(HostType, RoomUser, Remote, Packet, Acc)
    end.

%%%%%%%
%% Predicate funs

%% @doc Check if sender is just 'roomname@muclight.domain' with no resource
%% TODO: Replace sender domain check with namespace check - current logic won't handle all cases!
-spec  is_system_message(HostType :: mongooseim:host_type(),
                         Sender :: jid:jid(),
                         Receiver :: jid:jid(),
                         Packet :: exml:element()) -> boolean().
is_system_message(_HostType, #jid{lresource = <<>>}, _Receiver, _Packet) ->
    true;
is_system_message(_HostType, #jid{lresource = _RoomUser}, _Receiver, _Packet) ->
    false.

-spec is_change_aff_message(jid:jid(), exml:element(), role()) -> boolean().
is_change_aff_message(User, Packet, Role) ->
    AffItems = exml_query:paths(Packet, [{element_with_ns, ?NS_MUC_LIGHT_AFFILIATIONS},
        {element, <<"user">>}]),
    AffList = get_users_with_affiliation(AffItems, Role),
    Jids = [Jid || #xmlel{children = [#xmlcdata{content = Jid}]} <- AffList],
    UserBin = jid:to_binary(jid:to_lower(jid:to_bare(User))),
    lists:member(UserBin, Jids).

-spec system_message_type(User :: jid:jid(), Packet :: exml:element()) -> invite | kick | other.
system_message_type(User, Packet) ->
    IsInviteMsg = is_invitation_message(User, Packet),
    IsNewOwnerMsg = is_new_owner_message(User, Packet),
    IsKickedMsg = is_kicked_message(User, Packet),
    if IsInviteMsg orelse IsNewOwnerMsg ->
        invite;
       IsKickedMsg ->
            kick;
       true ->
            other
    end.

-spec is_invitation_message(jid:jid(), exml:element()) -> boolean().
is_invitation_message(User, Packet) ->
    is_change_aff_message(User, Packet, <<"member">>).

-spec is_new_owner_message(jid:jid(), exml:element()) -> boolean().
is_new_owner_message(User, Packet) ->
    is_change_aff_message(User, Packet, <<"owner">>).

-spec is_kicked_message(jid:jid(), exml:element()) -> boolean().
is_kicked_message(User, Packet) ->
    is_change_aff_message(User, Packet, <<"none">>).

-spec get_users_with_affiliation(list(exml:element()), role()) -> list(exml:element()).
get_users_with_affiliation(AffItems, Role) ->
    [M || #xmlel{name = <<"user">>, attrs = [{<<"affiliation">>, R}]} = M <- AffItems, R == Role].
