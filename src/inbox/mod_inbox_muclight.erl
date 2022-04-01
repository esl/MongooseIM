%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox_muclight).

-include("mod_muc_light.hrl").
-include("jlib.hrl").
-include("mongoose.hrl").

-export([handle_outgoing_message/5, handle_incoming_message/5]).

-type role() :: r_member() | r_owner() | r_none().
-type r_member() :: binary().
-type r_owner() :: binary().
-type r_none() :: binary().

-spec handle_outgoing_message(HostType :: mongooseim:host_type(),
                              User :: jid:jid(),
                              Room :: jid:jid(),
                              Packet :: exml:element(),
                              Acc :: mongoose_acc:t()) ->
    mod_inbox:count_res().
handle_outgoing_message(HostType, User, Room, Packet, Acc) ->
    mod_inbox_utils:maybe_reset_unread_count(HostType, User, Room, Packet, Acc).

-spec handle_incoming_message(HostType :: mongooseim:host_type(),
                              RoomUser :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: exml:element(),
                              Acc :: mongoose_acc:t()) ->
    mod_inbox:count_res().
handle_incoming_message(HostType, RoomUser, Remote, Packet, Acc) ->
    case mod_inbox_utils:has_chat_marker(Packet) of
        true ->
            %% don't store chat markers in inbox
            ok;
        false ->
            maybe_handle_system_message(HostType, RoomUser, Remote, Packet, Acc)
    end.

-spec maybe_handle_system_message(HostType :: mongooseim:host_type(),
                                  RoomOrUser :: jid:jid(),
                                  Receiver :: jid:jid(),
                                  Packet :: exml:element(),
                                  Acc :: mongoose_acc:t()) ->
    mod_inbox:count_res().
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
                            Acc :: mongoose_acc:t()) -> ok.
handle_system_message(HostType, Room, Remote, Packet, Acc) ->
    case system_message_type(Remote, Packet) of
        kick ->
            handle_kicked_message(HostType, Room, Remote, Packet, Acc);
        invite ->
            handle_invitation_message(HostType, Room, Remote, Packet, Acc);
        other ->
            ?LOG_DEBUG(#{what => irrelevant_system_message_for_mod_inbox_muclight,
                         room => Room, exml_packet => Packet}),
            ok
    end.

-spec handle_invitation_message(HostType :: mongooseim:host_type(),
                                Room :: jid:jid(),
                                Remote :: jid:jid(),
                                Packet :: exml:element(),
                                Acc :: mongoose_acc:t()) -> ok.
handle_invitation_message(HostType, Room, Remote, Packet, Acc) ->
    maybe_store_system_message(HostType, Room, Remote, Packet, Acc).

-spec handle_kicked_message(HostType :: mongooseim:host_type(),
                            Room :: jid:jid(),
                            Remote :: jid:jid(),
                            Packet :: exml:element(),
                            Acc :: mongoose_acc:t()) -> ok.
handle_kicked_message(HostType, Room, Remote, Packet, Acc) ->
    CheckRemove = mod_inbox_utils:get_option_remove_on_kicked(HostType),
    maybe_store_system_message(HostType, Room, Remote, Packet, Acc),
    maybe_remove_inbox_row(HostType, Room, Remote, CheckRemove).

-spec maybe_store_system_message(HostType :: mongooseim:host_type(),
                                 Room :: jid:jid(),
                                 Remote :: jid:jid(),
                                 Packet :: exml:element(),
                                 Acc :: mongoose_acc:t()) -> ok.
maybe_store_system_message(HostType, Room, Remote, Packet, Acc) ->
    WriteAffChanges = mod_inbox_utils:get_option_write_aff_changes(HostType),
    case WriteAffChanges of
        true ->
            write_to_inbox(HostType, Room, Remote, Room, Packet, Acc);
        false ->
            ok
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
                     Acc :: mongoose_acc:t()) ->
    mod_inbox:count_res().
write_to_inbox(HostType, RoomUser, Remote, Sender, Packet, Acc) ->
    case jid:are_equal(Remote, Sender) of
        true -> mod_inbox_utils:write_to_sender_inbox(HostType, Remote, RoomUser, Packet, Acc);
        false -> mod_inbox_utils:write_to_receiver_inbox(HostType, RoomUser, Remote, Packet, Acc)
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
    case {is_invitation_message(User, Packet),
          is_new_owner_message(User, Packet),
          is_kicked_message(User, Packet)} of
        {true, _, _} -> invite;
        {_, true, _} -> invite;
        {_, _, true} -> kick;
        _ -> other
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
