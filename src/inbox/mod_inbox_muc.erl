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

%% Receiver's host in lowercase
-type receiver_host() :: jid:server().
-type receiver_bare_user_jid() :: jid:jid().
-type room_bare_jid() :: jid:jid().
-type packet() :: exml:element().

start(Host) ->
    ejabberd_hooks:add(update_inbox_for_muc, Host, ?MODULE, update_inbox_for_muc, 90),
    % TODO check ooptions: if system messages stored ->
    % add hook handler for system messages on hook ie. invitation_sent
    ok.

stop(Host) ->
    ejabberd_hooks:delete(update_inbox_for_muc, Host, ?MODULE, update_inbox_for_muc, 90),
    ok.


-spec update_inbox_for_muc(Acc) -> Acc when
      Acc :: map().
update_inbox_for_muc(
    #{room_jid := Room,
      from_jid := From,
      from_room_jid := FromRoomJid,
      packet := Packet,
      affiliations_map := AffsMap} = Acc) ->
    F = fun(AffLJID, Affiliation, _) ->
            case is_allowed_affiliation(Affiliation) of
                true ->
                    To = jid:to_bare(jid:make(AffLJID)),
                    %% Guess direction based on user JIDs
                    Direction = direction(From, To),
                    Host = To#jid.lserver,
                    Packet2 = jlib:replace_from_to(FromRoomJid, To, Packet),
                    update_inbox_for_user(Direction, Host, Room, To, Packet2);
                false ->
                    ok
            end
        end,
    maps:fold(F, ok, AffsMap),
    Acc.

-spec is_allowed_affiliation(mod_muc:affiliation()) -> boolean().
is_allowed_affiliation(outcast) -> false;
is_allowed_affiliation(_)       -> true.

-spec update_inbox_for_user(Direction, Host, Room, To, Packet) -> term() when
      Direction :: incoming | outgoing,
      Host :: receiver_host(),
      Room :: room_bare_jid(),
      To :: receiver_bare_user_jid(),
      Packet :: packet().
update_inbox_for_user(Direction, Host, Room, To, Packet) ->
    %% Check that Host is a local served domain (not s2s)
    case {is_local_host(Host), Direction} of
        {true, outgoing} ->
            handle_outgoing_message(Host, Room, To, Packet);
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
-spec handle_outgoing_message(Host, Room, To, Packet) -> term() when
      Host :: receiver_host(),
      Room :: room_bare_jid(),
      To :: receiver_bare_user_jid(),
      Packet :: packet().
handle_outgoing_message(Host, Room, To, Packet) ->
    Markers = mod_inbox_utils:get_reset_markers(Host),
    case mod_inbox_utils:if_chat_marker_get_id(Packet, Markers) of
        undefined ->
            mod_inbox_utils:write_to_sender_inbox(Host, To, Room, Packet);
        Id ->
            mod_inbox_utils:reset_unread_count(To, Room, Id)
    end.

-spec handle_incoming_message(Host, Room, To, Packet) -> term() when
      Host :: receiver_host(),
      Room :: room_bare_jid(),
      To :: receiver_bare_user_jid(),
      Packet :: packet().
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
