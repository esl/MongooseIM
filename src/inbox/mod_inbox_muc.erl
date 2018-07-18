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
-include("mongoose.hrl").

-export([update_inbox_for_muc/1, start/1, stop/1]).

start(Host) ->
    ejabberd_hooks:add(update_inbox_for_muc, Host, ?MODULE, update_inbox_for_muc, 90),
    % TODO check ooptions: if system messages stored ->
    % add hook handler for system messages on hook ie. invitation_sent
    ok.

stop(Host) ->
    ejabberd_hooks:delete(update_inbox_for_muc, Host, ?MODULE, update_inbox_for_muc, 90),
    ok.


update_inbox_for_muc(
    #{room_jid := Room,
      from_jid := From,
      from_room_jid := FromRoomJid,
      packet := Packet,
      affiliations_map := AffsMap} = Acc) ->
    F = fun(AffLJID, Affiliation, _) ->
            case is_allowed_affiliation(Affiliation) of
                true ->
                    %% To - receiver's bare user jid (not muc jid)
                    To = jid:make(AffLJID),
                    %% Guess direction based on user JIDs
                    Direction = direction(From, To),
                    %% Host - domain of the receiver
                    Host = To#jid.lserver,
                    Packet2 = jlib:replace_from_to(FromRoomJid, To, Packet),
                    update_inbox_for_user(Direction, Host, From, To, Room, Packet2);
                false ->
                    ok
            end
        end,
    maps:fold(F, ok, AffsMap),
    Acc.

is_allowed_affiliation(outcast) -> false;
is_allowed_affiliation(_)       -> true.

%% Host - domain of the receiver and sender
%% From - sender's bare user jid (not muc jid)
%% To - receiver's bare user jid (not muc jid)
%% Room - room's bare jid
update_inbox_for_user(Direction, Host, From, To, Room, Packet) ->
    %% Check that Host is a local served domain (not s2s)
    case {is_local_host(Host), Direction} of
        {true, outgoing} ->
            handle_outgoing_message(Host, From, Room, Packet);
        {true, incoming} ->
            handle_incoming_message(Host, Room, To, Packet);
        _ ->
            ok
    end.

%% From and To are user JIDs (not room JIDs)
direction(From, To) ->
    case jid:are_bare_equal(From, To) of
        true -> outgoing;
        false -> incoming
    end.

%% Sender and receiver is the same user
%%
%% Host - domain of the receiver and sender
%% From - sender's bare user jid (not muc jid)
%% Room - room's bare jid
handle_outgoing_message(Host, From, Room, Packet) ->
    Markers = mod_inbox_utils:get_reset_markers(Host),
    case mod_inbox_utils:if_chat_marker_get_id(Packet, Markers) of
        undefined ->
            mod_inbox_utils:write_to_sender_inbox(Host, From, Room, Packet);
        Id ->
            mod_inbox_utils:reset_unread_count(From, Room, Id)
    end.

%% Host - domain of the receiver
%% Room - room's bare jid
%% To - receiver's bare user jid (not muc jid)
handle_incoming_message(Host, Room, To, Packet) ->
    Markers = mod_inbox_utils:get_reset_markers(Host),
    case mod_inbox_utils:has_chat_marker(Packet, Markers) of
        true ->
            %% don't store chat markers in inbox
            ok;
        false ->
            mod_inbox_utils:write_to_receiver_inbox(Host, Room, To, Packet)
    end.

%% Returns false, if host is s2s host
is_local_host(LServer) ->
    lists:member(LServer, ?MYHOSTS).
