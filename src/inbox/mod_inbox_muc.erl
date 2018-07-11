%%%-------------------------------------------------------------------
%%% @author Dominik Stanaszek dominik.stanaszek@erlang-solutions.com
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 6.07.2018 
%%%-------------------------------------------------------------------
-module(mod_inbox_muc).
-author("dominik.stanaszek@erlang-solutions.com").
-include("jlib.hrl").

-export([update_inbox/5, start/1, stop/1]).

start(Host) ->
    ejabberd_hooks:add(update_inbox, Host, ?MODULE, update_inbox, 90),
    % TODO check ooptions: if system messages stored -> 
    % add hook handler for system messages on hook ie. invitation_sent
    ok.

stop(Host) ->
    ejabberd_hooks:delete(update_inbox, Host, ?MODULE, update_inbox, 90),
    ok.

update_inbox(Acc, Room, {From, FromRoomJid}, AffsDict, Packet) ->
    Affs = dict:to_list(AffsDict),
    Users = affs_to_allowed_users(Affs),
    FromBare = jid:to_bare(From),
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
    case direction(From, To) of
        outgoing ->
            NewPacket = alter_senders_packet(Packet, FromRoomJid, To),
            handle_outgoing_message(Host, From, Room, NewPacket);
        incoming ->
            NewPacket = alter_receivers_packet(Packet, FromRoomJid, To),
            handle_incoming_message(Host, Room, To, NewPacket)
    end.

direction(From, To) ->
    case jid:are_equal(From, To) of
        true -> outgoing;
        false -> incoming
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


handle_outgoing_message(Host, User, Room, Packet) ->
    Markers = mod_inbox_utils:get_reset_markers(Host),
    case mod_inbox_utils:if_chat_marker_get_id(Packet, Markers) of
        undefined ->
            mod_inbox_utils:write_to_sender_inbox(Host, User, Room, Packet);
        Id ->
            mod_inbox_utils:reset_unread_count(User, Room, Id)
    end.

handle_incoming_message(Host, Room, To, Packet) ->
    Markers = mod_inbox_utils:get_reset_markers(Host),
    case mod_inbox_utils:has_chat_marker(Packet, Markers) of
        true ->
            %% don't store chat markers in inbox
            ok;
        false ->
            mod_inbox_utils:write_to_receiver_inbox(Host, Room, To, Packet)
    end.
