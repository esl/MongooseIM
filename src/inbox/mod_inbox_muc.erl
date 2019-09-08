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

%% User jid example is "alice@localhost"
-type user_jid() :: jid:jid().
%% Receiver's host in lowercase
-type receiver_host() :: jid:lserver().
-type receiver_bare_user_jid() :: user_jid().
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
      Acc :: mod_muc_room:update_inbox_for_muc_payload().
update_inbox_for_muc(
    #{room_jid := Room,
      from_jid := From,
      from_room_jid := FromRoomJid,
      packet := Packet,
      affiliations_map := AffsMap} = Acc) ->
    F = fun(AffLJID, Affiliation) ->
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
    mongoose_lib:maps_foreach(F, AffsMap),
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
    case {is_local_xmpp_host(Host), Direction} of
        {true, outgoing} ->
            handle_outgoing_message(Host, Room, To, Packet);
        {true, incoming} ->
            handle_incoming_message(Host, Room, To, Packet);
        _ ->
            %% We ignore inbox for users on the remote (s2s) hosts
            %% We ignore inbox for components (also known as services or bots)
            ok
    end.

-spec direction(From :: user_jid(), To :: user_jid()) -> incoming | outgoing.
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
    maybe_reset_unread_count(Host, To, Room, Packet),
    maybe_write_to_inbox(Host, To, Room, Packet, fun write_to_sender_inbox/4).

-spec handle_incoming_message(Host, Room, To, Packet) -> term() when
      Host :: receiver_host(),
      Room :: room_bare_jid(),
      To :: receiver_bare_user_jid(),
      Packet :: packet().
handle_incoming_message(Host, Room, To, Packet) ->
    maybe_write_to_inbox(Host, Room, To, Packet, fun write_to_receiver_inbox/4).

maybe_reset_unread_count(Host, User, Room, Packet) ->
    mod_inbox_utils:maybe_reset_unread_count(Host, User, Room, Packet).

maybe_write_to_inbox(Host, User, Remote, Packet, WriteF) ->
    mod_inbox_utils:maybe_write_to_inbox(Host, User, Remote, Packet, WriteF).

write_to_sender_inbox(Server, User, Remote, Packet) ->
    mod_inbox_utils:write_to_sender_inbox(Server, User, Remote, Packet).

write_to_receiver_inbox(Server, User, Remote, Packet) ->
    mod_inbox_utils:write_to_receiver_inbox(Server, User, Remote, Packet).

%% @doc Check, that the host is served by MongooseIM.
%% A local host can be used to fire hooks or write into database on this node.
-spec is_local_xmpp_host(jid:lserver()) -> boolean().
is_local_xmpp_host(LServer) ->
    lists:member(LServer, ?MYHOSTS).
