%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox_muc).
-author("ludwikbukowski").
-include("mod_muc_light.hrl").
-include("mod_inbox.hrl").
-include("jlib.hrl").
-include("jid.hrl").
-include("mongoose_ns.hrl").
-include("mongoose.hrl").

-export([update_inbox/5]).
-type packet() :: exml:element().
-type role() :: r_member() | r_owner() | r_none().
-type r_member() :: binary().
-type r_owner() :: binary().
-type r_none() :: binary().

update_inbox(Acc, Room, {From, FromRoomJid}, AffsDict, Packet) ->
    Affs = dict:to_list(AffsDict),
    Users = affs_to_allowed_users(Affs),
    FromBare = jid:to_bare(From),
    ?WARNING_MSG("Update inbox hook:~n From: ~p~nUsers: ~p,~n Acc: ~p", 
                 [FromBare, Users, Acc]),
    lists:foreach(fun(User) -> update_inbox(Room, 
                                            {FromBare, FromRoomJid}, 
                                            User, 
                                            Packet) end,
                  Users),
    Acc.

affs_to_allowed_users(Users) ->
    AllowedUsers = lists:filter(fun({_User, outcast}) -> false;
                    (_) -> true end, Users),
    lists:map(fun({{User, Domain, Res}, _Aff}) -> jid:make(User, Domain, Res) end,
              AllowedUsers).


update_inbox(Room, {From, FromRoomJid}, To, Packet) ->
    Host = To#jid.server,
    case jid:are_equal(From, To) of
        true ->
            mod_inbox_utils:write_to_sender_inbox(Host, From, Room, 
                                                  alter_senders_packet(Packet, FromRoomJid, To));
        false ->
            mod_inbox_utils:write_to_receiver_inbox(Host, Room, To, 
                                                    alter_receivers_packet(Packet, FromRoomJid, To))
    end.

% TODO this logic probably should be in mod_muc_room before routing
% and put into the routing function.
alter_senders_packet(Packet, FromRoomJid, To) ->
    P2 = change_from_el(Packet, FromRoomJid),
    change_to_el(P2, To).

alter_receivers_packet(Packet, Room, To) ->
    Packet2 = change_from_el(Packet, Room),
    change_to_el(Packet2, To).

% TODO refactor
change_from_el(#xmlel{name = <<"message">>, attrs = Attrs} = Packet, NewFrom) ->
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs),
    Attrs3 = [{<<"from">>, jid:to_binary(NewFrom)} | Attrs2],
    Packet#xmlel{attrs = Attrs3}.

change_to_el(#xmlel{name = <<"message">>, attrs = Attrs} = Packet, NewTo) ->
    Attrs2 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs3 = [{<<"to">>, jid:to_binary(NewTo)} | Attrs2],
    Packet#xmlel{attrs = Attrs3}.




-spec handle_outgoing_message(Host :: jid:server(),
                              User :: jid:jid(),
                              Room :: jid:jid(),
                              Packet :: packet()) -> any().
handle_outgoing_message(Host, User, Room, Packet) ->
    Markers = mod_inbox_utils:get_reset_markers(Host),
    case mod_inbox_utils:if_chat_marker_get_id(Packet, Markers) of
        undefined ->
            %% we store in inbox only on incoming messages
            ok;
        Id ->
            mod_inbox_utils:reset_unread_count(User, Room, Id)
    end.

-spec handle_incoming_message(Host :: jid:server(),
                              RoomUser :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet()) -> any().
handle_incoming_message(Host, RoomUser, Remote, Packet) ->
    Markers = mod_inbox_utils:get_reset_markers(Host),
    case mod_inbox_utils:has_chat_marker(Packet, Markers) of
        true ->
            %% don't store chat markers in inbox
            ok;
        false ->
            maybe_handle_system_message(Host, RoomUser, Remote, Packet)
    end.

-spec maybe_handle_system_message(Host :: host(),
                                  RoomUser :: jid:jid(),
                                  Receiver :: jid:jid(),
                                  Packet :: exml:element()) -> ok.
maybe_handle_system_message(Host, RoomUser, Receiver, Packet) ->
    case is_system_message(RoomUser, Receiver, Packet) of
        true ->
            handle_system_message(Host, RoomUser, Receiver, Packet);
        _ ->
            Sender = jid:from_binary(RoomUser#jid.lresource),
            ?WARNING_MSG("Storing: ~p for ~p", [Packet, RoomUser]),
            write_to_inbox(Host, RoomUser, Receiver, Sender, Packet)
    end.

-spec handle_system_message(Host :: host(),
                            Room :: jid:jid(),
                            Remote :: jid:jid(),
                            Packet :: exml:element()) -> ok.
handle_system_message(Host, Room, Remote, Packet) ->
    case system_message_type(Remote, Packet) of
        kick ->
            handle_kicked_message(Host, Room, Remote, Packet);
        invite->
            handle_invitation_message(Host, Room, Remote, Packet);
        other ->
            ?WARNING_MSG("muc) unknown system messasge for mod_inbox_muclight='~p' with error ~p", [Packet]),
            ok
    end.

-spec handle_invitation_message(Host :: host(),
                                Room :: jid:jid(),
                                Remote :: jid:jid(),
                                Packet :: exml:element()) -> ok.
handle_invitation_message(Host, Room, Remote, Packet) ->
    maybe_store_system_message(Host, Room, Remote, Packet).

-spec handle_kicked_message(Host :: host(),
                            Room :: jid:jid(),
                            Remote :: jid:jid(),
                            Packet :: exml:element()) -> ok.
handle_kicked_message(Host, Room, Remote, Packet) ->
    CheckRemove = mod_inbox_utils:get_option_remove_on_kicked(Host),
    maybe_store_system_message(Host, Room, Remote, Packet),
    maybe_remove_inbox_row(Host, Room, Remote, CheckRemove).

-spec maybe_store_system_message(Host :: host(),
                                  Room :: jid:jid(),
                                  Remote :: jid:jid(),
                                  Packet :: exml:element()) -> ok.
maybe_store_system_message(Host, Room, Remote, Packet) ->
    WriteAffChanges = mod_inbox_utils:get_option_write_aff_changes(Host),
    case WriteAffChanges of
        true ->
            mod_inbox_utils:write_to_receiver_inbox(Host, Room, Remote, Packet);
        false ->
            ok
    end.

-spec maybe_remove_inbox_row(Host :: host(),
                             Room :: jid:jid(),
                             Remote :: jid:jid(),
                             WriteAffChanges :: boolean()) -> ok.
maybe_remove_inbox_row(_, _, _, false) ->
    ok;
maybe_remove_inbox_row(Host, Room, Remote, true) ->
    UserBin = (jid:to_bare(Remote))#jid.luser,
    RoomBin = jid:to_binary(Room),
    ok = mod_inbox_backend:remove_inbox(UserBin, Host, RoomBin).

-spec write_to_inbox(Server :: host(),
                     RoomUser :: jid:jid(),
                     Remote :: jid:jid(),
                     Sender :: jid:jid(),
                     Packet :: exml:element()) -> ok.
write_to_inbox(Server, RoomUser, Remote, Remote, Packet) ->
    mod_inbox_utils:write_to_sender_inbox(Server, Remote, RoomUser, Packet);
write_to_inbox(Server, RoomUser, Remote, _Sender, Packet) ->
    mod_inbox_utils:write_to_receiver_inbox(Server, RoomUser, Remote, Packet).

%%%%%%%
%% Predicate funs

%% check if sender is just 'roomname@muclight.domain' with no resource
-spec  is_system_message(Sender :: jid:jid(),
                         Receiver :: jid:jid(),
                         Packet :: exml:element()) -> boolean().
is_system_message(Sender, Receiver, Packet) ->
    ReceiverDomain = Receiver#jid.lserver,
    MUCLightDomain = list_to_binary(gen_mod:get_module_opt(ReceiverDomain, mod_muc_light, host, undefined)),
    is_subject_msg(Packet) orelse
    case {Sender#jid.lserver, Sender#jid.lresource} of
        {MUCLightDomain, <<>>} ->
            true;
        {MUCLightDomain, _RoomUser} ->
            false;
        true -> true;
        Other ->
            ?WARNING_MSG("unknown messasge for mod_inbox_muclight='~p' with error ~p", [Packet, Other])
    end.


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

is_subject_msg(Packet) ->
    case exml_query:subelement(Packet, <<"body">>, undefined) of
        undefined -> true;
        #xmlel{children = []} -> true;
        _ -> false
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
