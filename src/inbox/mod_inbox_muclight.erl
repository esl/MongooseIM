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
-include("jid.hrl").
-include("mongoose_ns.hrl").
-include("mongoose.hrl").

-export([handle_outgoing_message/4, handle_incoming_message/4]).
-type packet() :: exml:element().
-type role() :: r_member() | r_owner() | r_none().
-type r_member() :: binary().
-type r_owner() :: binary().
-type r_none() :: binary().


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
    User :: jid:jid(),
    User :: jid:jid(),
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


maybe_handle_system_message(Host, RoomUser, Receiver, Packet) ->
    case is_system_message(RoomUser, Receiver, Packet) of
        true ->
            WriteAffChanges = mod_inbox_utils:get_option_write_aff_changes(Host),
            handle_system_message(Host, RoomUser, Receiver, Packet, WriteAffChanges);
        _ ->
            Sender = jid:from_binary(RoomUser#jid.lresource),
            write_to_inbox(Host, RoomUser, Receiver, Sender, Packet)
    end.


handle_system_message(_Host, _Room, _Remote, _Packet, false) ->
    ok;
handle_system_message(Host, Room, Remote, Packet, true) ->
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
            handle_invitation_message(Host, Room, Remote, Packet)
    end.

-spec handle_invitation_message(User :: jid:jid(),
    Server :: host(),
    UserFromRoom :: jid:jid(),
    Packet :: exml:element()) -> ok.
handle_invitation_message(Host, Room, Remote, Packet) ->
    mod_inbox_utils:write_to_receiver_inbox(Host, Room, Remote, Packet).

handle_kicked_message(Host, User, Room) ->
    CheckRemove = mod_inbox_utils:get_option_remove_on_kicked(Host),
    maybe_remove_inbox_row(Host, User, Room, CheckRemove).


maybe_remove_inbox_row(_, _, _, false) ->
    ok;
maybe_remove_inbox_row(Host, User, Room, true) ->
    UserBin = (jid:to_bare(User))#jid.luser,
    RoomBin = jid:to_binary(Room),
    ok = mod_inbox_backend:remove_inbox(UserBin, Host, RoomBin).


write_to_inbox(Server, RoomUser, Remote, Remote, Packet) ->
    mod_inbox_utils:write_to_sender_inbox(Server, Remote, RoomUser, Packet);
write_to_inbox(Server, RoomUser, Remote, _Sender, Packet) ->
    mod_inbox_utils:write_to_receiver_inbox(Server, RoomUser, Remote, Packet).

%%%%%%%
%% Predicate funs

%% check if sender is just 'roomname@muclight.domain' with no resource
is_system_message(Sender, Receiver, Packet) ->
    ReceiverDomain = Receiver#jid.lserver,
    MUCLightDomain = list_to_binary(gen_mod:get_module_opt(ReceiverDomain, mod_muc_light, host, undefined)),
    case {Sender#jid.lserver, Sender#jid.lresource} of
        {MUCLightDomain, <<>>} ->
            true;
        {MUCLightDomain, _RoomUser} ->
            false;
        Other ->
            ?WARNING_MSG("unknown messasge for mod_inbox_muclight='~p' with error ~p", [Packet, Other])
    end.


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
    [M || #xmlel{name = <<"user">>, attrs = [{<<"affiliation">>, R}]} = M <- AffItems, R == Role].