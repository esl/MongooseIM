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

-export([update_inbox_for_muc/3, start/1, stop/1]).

%% User jid example is "alice@localhost"
-type user_jid() :: jid:jid().
-type receiver_bare_user_jid() :: user_jid().
-type room_bare_jid() :: jid:jid().
-type packet() :: exml:element().

start(HostType) ->
    gen_hook:add_handlers(hooks(HostType)),
    % TODO check options: if system messages stored ->
    % add hook handler for system messages on hook ie. invitation_sent
    ok.

stop(HostType) ->
    gen_hook:delete_handlers(hooks(HostType)),
    ok.

hooks(HostType) ->
    [{update_inbox_for_muc, HostType, fun ?MODULE:update_inbox_for_muc/3, #{}, 90}].

-spec update_inbox_for_muc(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mod_muc_room:update_inbox_for_muc_payload(),
      Params :: map(),
      Extra :: gen_hook:extra().
update_inbox_for_muc(
    #{host_type := HostType,
      room_jid := Room,
      from_jid := From,
      from_room_jid := FromRoomJid,
      packet := Packet,
      affiliations_map := AffsMap} = Acc, _, _) ->
    F = fun(AffLJID, Affiliation) ->
            case is_allowed_affiliation(Affiliation) of
                true ->
                    To = jid:to_bare(jid:make(AffLJID)),
                    %% Guess direction based on user JIDs
                    Direction = direction(From, To),
                    Packet2 = jlib:replace_from_to(FromRoomJid, To, Packet),
                    update_inbox_for_user(HostType, Direction, Room, To, Packet2);
                false ->
                    ok
            end
        end,
    mongoose_lib:maps_foreach(F, AffsMap),
    {ok, Acc}.

-spec is_allowed_affiliation(mod_muc:affiliation()) -> boolean().
is_allowed_affiliation(outcast) -> false;
is_allowed_affiliation(_)       -> true.

-spec update_inbox_for_user(HostType, Direction, Room, To, Packet) -> term() when
      HostType :: mongooseim:host_type(),
      Direction :: incoming | outgoing,
      Room :: room_bare_jid(),
      To :: receiver_bare_user_jid(),
      Packet :: packet().
update_inbox_for_user(HostType, Direction, Room, To, Packet) ->
    ReceiverDomain = To#jid.lserver,
    MucDomain = mod_muc:server_host_to_muc_host(HostType, ReceiverDomain),
    case Room#jid.lserver of
        MucDomain ->
            handle_message(HostType, Room, To, Packet, Direction);
        _ ->
            %% We ignore inbox for users on the remote (s2s) hosts
            %% We ignore inbox for components (also known as services or bots)
            ok
    end.

handle_message(HostType, Room, To, Packet, outgoing) ->
    handle_outgoing_message(HostType, Room, To, Packet);
handle_message(HostType, Room, To, Packet, incoming) ->
    handle_incoming_message(HostType, Room, To, Packet).

-spec direction(From :: user_jid(), To :: user_jid()) -> incoming | outgoing.
direction(From, To) ->
    case jid:are_bare_equal(From, To) of
        true -> outgoing;
        false -> incoming
    end.

%% Sender and receiver is the same user
-spec handle_outgoing_message(HostType, Room, To, Packet) -> term() when
      HostType :: mongooseim:host_type(),
      Room :: room_bare_jid(),
      To :: receiver_bare_user_jid(),
      Packet :: packet().
handle_outgoing_message(HostType, Room, To, Packet) ->
    Acc = mongoose_acc:new(#{location => ?LOCATION, lserver => To#jid.lserver, host_type => HostType}),
    maybe_reset_unread_count(HostType, To, Room, Packet, Acc),
    maybe_write_to_inbox(HostType, To, Room, Packet, Acc, fun write_to_sender_inbox/5).

-spec handle_incoming_message(HostType, Room, To, Packet) -> term() when
      HostType :: mongooseim:host_type(),
      Room :: room_bare_jid(),
      To :: receiver_bare_user_jid(),
      Packet :: packet().
handle_incoming_message(HostType, Room, To, Packet) ->
    Acc = mongoose_acc:new(#{location => ?LOCATION, lserver => To#jid.lserver, host_type => HostType}),
    maybe_write_to_inbox(HostType, Room, To, Packet, Acc, fun write_to_receiver_inbox/5).

maybe_reset_unread_count(HostType, User, Room, Packet, Acc) ->
    mod_inbox_utils:maybe_reset_unread_count(HostType, User, Room, Packet, Acc).

maybe_write_to_inbox(HostType, User, Remote, Packet, Acc, WriteF) ->
    mod_inbox_utils:maybe_write_to_inbox(HostType, User, Remote, Packet, Acc, WriteF).

write_to_sender_inbox(Server, User, Remote, Packet, Acc) ->
    mod_inbox_utils:write_to_sender_inbox(Server, User, Remote, Packet, Acc).

write_to_receiver_inbox(Server, User, Remote, Packet, Acc) ->
    mod_inbox_utils:write_to_receiver_inbox(Server, User, Remote, Packet, Acc).
