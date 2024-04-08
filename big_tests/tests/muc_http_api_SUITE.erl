%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Author: Joseph Yiasemides <joseph.yiasemides@erlang-solutions.com>
%% Description: Test HTTP Administration API for Mult-user Chat (MUC)
%%==============================================================================

-module(muc_http_api_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-import(domain_helper, [domain/0, secondary_domain/0]).
-import(rest_helper, [post/3, delete/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, positive},
     {group, negative}].

groups() ->
    [{positive, [parallel], success_response() ++ complex()},
     {negative, [parallel], failure_response()}].

success_response() ->
    [
     create_room,
     invite_online_user_to_room,
     kick_user_from_room,
     %% invite_offline_user_to_room, %% TO DO.
     send_message_to_room
    ].

complex() ->
    [
     multiparty_multiprotocol
    ].

failure_response() ->
    [room_creation_errors,
     invite_errors,
     kick_user_errors,
     message_errors].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    muc_helper:load_muc(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    muc_helper:unload_muc(),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob, kate])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, kate])).

init_per_testcase(CaseName, Config0) ->
    Config1 = [{room_name, make_distinct_name(<<"wonderland">>)}|Config0],
    escalus:init_per_testcase(CaseName, Config1).

end_per_testcase(CaseName, Config) ->
    muc_helper:destroy_room(muc_helper:muc_host(), ?config(room_name, Config)),
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

create_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Path = path([]),
        Name = ?config(room_name, Config),
        Body = #{name => Name,
                 owner => escalus_client:short_jid(Alice),
                 nick => <<"ali">>},
        Res = rest_helper:make_request(#{role => admin,
                                         method => <<"POST">>,
                                         path => Path,
                                         body => Body,
                                         return_headers => true}),
        {{<<"201">>, _}, Headers, Name} = Res,
        Exp = <<"/api", (path([Name]))/binary>>,
        Uri = uri_string:parse(proplists:get_value(<<"location">>, Headers)),
        ?assertEqual(Exp, maps:get(path, Uri)),
        %% Service acknowledges room creation (10.1.1 Ex. 154), then
        %% (presumably 7.2.16) sends room subject, finally the IQ
        %% result of the IQ request (10.1.2) for an instant room. The
        %% stanza for 7.2.16 has a BODY element which it shouldn't.
        escalus:wait_for_stanzas(Alice, 3),
        escalus:send(Alice, stanza_get_rooms()),
        Stanza = escalus:wait_for_stanza(Alice),
        true = has_room(muc_helper:room_address(Name), Stanza),
        escalus:assert(is_stanza_from, [muc_helper:muc_host()], Stanza)
    end).

invite_online_user_to_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Name = ?config(room_name, Config),
        Path = path([Name, "participants"]),
        Reason = <<"I think you'll like this room!">>,
        Body = #{sender => escalus_client:short_jid(Alice),
                 recipient => escalus_client:short_jid(Bob),
                 reason => Reason},
        {{<<"404">>, _}, <<"Room not found">>} = rest_helper:post(admin, Path, Body),
        set_up_room(Config, escalus_client:short_jid(Alice)),
        {{<<"204">>, _}, <<"">>} = rest_helper:post(admin, Path, Body),
        Stanza = escalus:wait_for_stanza(Bob),
        is_direct_invitation(Stanza),
        direct_invite_has_reason(Stanza, Reason)
    end).

send_message_to_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(_Alice, Bob) ->
        Name = ?config(room_name, Config),
        %% Alice creates a MUC room.
        muc_helper:start_room([], escalus_users:get_user_by_name(alice),
                              Name, <<"ali">>, []),
        %% Bob enters the room.
        escalus:send(Bob,
                     muc_helper:stanza_muc_enter_room(Name,
                                                      <<"bobcat">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Parameters for this test.
        Path = path([Name, "messages"]),
        Message = <<"Greetings!">>,
        Body = #{from => escalus_client:short_jid(Bob),
                 body => Message},
        {{<<"204">>, _}, <<"">>} = rest_helper:post(admin, Path, Body),
        Got = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, Got),
        Message = exml_query:path(Got, [{element, <<"body">>}, cdata])
    end).

kick_user_from_room(Config) ->
    escalus:fresh_story(Config,
      [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        %% Parameters for this test.
        Name = ?config(room_name, Config),
        Path = path([Name, "bobcat"]),
        %% Alice creates and enters the room.
        escalus:send(Alice,
                     muc_helper:stanza_muc_enter_room(Name,
                                                      <<"alibaba">>)),
        escalus:send(Alice,
                     muc_helper:stanza_default_muc_room(Name,
                                                        <<"alibaba">>)),
        %% Alice gets an IQ result, her affiliation information, and
        %% the room's subject line.
        escalus:wait_for_stanzas(Alice, 3),
        %% Bob enters the room.
        escalus:send(Bob,
                     muc_helper:stanza_muc_enter_room(Name,
                                                      <<"bobcat">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Alice sees Bob's presence.
        escalus:wait_for_stanza(Alice),
        %% Kate enters the room.
        escalus:send(Kate,
                     muc_helper:stanza_muc_enter_room(Name,
                                                      <<"kitkat">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Alice and Bob see Kate's presence.
        escalus:wait_for_stanza(Alice),
        escalus:wait_for_stanza(Bob),
        %% The HTTP call in question.
        {{<<"204">>, _}, <<"">>} = rest_helper:delete(admin, Path),
        BobRoomAddress = muc_helper:room_address(Name, <<"bobcat">>),
        %% Bob finds out he's been kicked.
        KickedStanza = escalus:wait_for_stanza(Bob),
        is_unavailable_presence_from(KickedStanza, BobRoomAddress),
        %% Kate finds out Bob is kicked.
        is_unavailable_presence_from(escalus:wait_for_stanza(Kate),
                                     BobRoomAddress),
        %% Alice finds out Bob is kicked.
        is_unavailable_presence_from(escalus:wait_for_stanza(Alice),
                                     BobRoomAddress),
        %% **NOTE**: Alice is a moderator so Bob is kicked through
        %% her. She recieves and IQ result.
        escalus:wait_for_stanza(Alice)
    end).

multiparty_multiprotocol(Config) ->
    MUCPath = path([]),
    Room = ?config(room_name, Config),
    RoomInvitePath = path([Room, "participants"]),
    Reason = <<"I think you'll like this room!">>,
    MessagePath = path([Room, "messages"]),
    Message = <<"Greetings!">>,
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            %% XMPP: Bob does not see a MUC room called 'wonderland'.
            false = user_sees_room(Bob, Room),
            %% HTTP: create a room on Alice's behalf.
            {{<<"201">>, _}, Room} =
                rest_helper:post(admin, MUCPath,
                                 #{name => Room,
                                   owner => escalus_client:short_jid(Alice),
                                   nick => <<"ali">>}),
            %% See comments under the create room test case.
            escalus:wait_for_stanzas(Alice, 3),
            %% XMPP: Kate sees the MUC room.
            true = user_sees_room(Kate, Room),
            %% HTTP: Alice invites Bob to the MUC room.
            {{<<"204">>, _}, <<"">>} =
                rest_helper:post(admin, RoomInvitePath,
                                 invite_body(Alice, Bob, Reason)),
            %% XMPP: Bob recieves the invite to the MUC room.
            Room = wait_for_invite(Bob, Reason),
            %% HTTP: Alice invites Kate to the MUC room.
            {{<<"204">>, _}, <<"">>} =
                rest_helper:post(admin, RoomInvitePath,
                                 invite_body(Alice, Kate, Reason)),
            %% XMPP: kate recieves the invite to the MUC room.
            Room = wait_for_invite(Kate, Reason),
            %% XMPP: Bob joins the MUC room with the JID he recieved.
            escalus:send(Bob,
                         muc_helper:stanza_muc_enter_room(Room,
                                                          <<"bobcat">>)),
            %% Bob gets precense informing him of his room occupancy,
            %% he recieves a presence informing him about Alice's
            %% affiliation and occupancy, and (presumably what is
            %% intended to be) the room subject. See 7.1 in the XEP.
            escalus:wait_for_stanzas(Bob, 3),
            %% Alice sees Bob's presence.
            escalus:wait_for_stanza(Alice),
            %% XMPP: kate joins the MUC room with the JID she recieved.
            escalus:send(Kate,
                         muc_helper:stanza_muc_enter_room(Room,
                                                          <<"kitkat">>)),
            %% Kate gets analogous stanza's to Bob + Bob's presence.
            escalus:wait_for_stanzas(Kate, 4),
            %% Alice & Bob get's Kate's presence.
            [ escalus:wait_for_stanza(User) || User <- [Alice, Bob] ],
            %% HTTP: Alice sends a message to the room.
            {{<<"204">>, _}, <<"">>} =
                rest_helper:post(admin, MessagePath,
                                 #{from => escalus_client:short_jid(Alice),
                                   body => Message}),
            %% XMPP: All three recieve the message sent to the MUC room.
            [ Message = wait_for_group_message(User) || User <- [Alice, Bob, Kate] ],
            %% XMPP: Bob and Kate send a message to the MUC room.
            [ user_sends_message_to_room(U, M, Room)
              || {U, M} <- [{Bob, <<"I'm Bob.">>}, {Kate, <<"I'm Kate.">>}] ],
            %% XMPP: Alice recieves the messages from Bob and Kate.
            BobRoomJID = muc_helper:room_address(Room, <<"bobcat">>),
            KateRoomJID = muc_helper:room_address(Room, <<"kitkat">>),

            ?assertEqual([{BobRoomJID, <<"I'm Bob.">>}, {KateRoomJID, <<"I'm Kate.">>}],
                         user_sees_message_from(Alice, Room, 2))
        end).

room_creation_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    Name = ?config(room_name, Config),
    Body = #{name => Name, owner => AliceJid, nick => <<"nick">>},
    {{<<"400">>, _}, <<"Missing room name">>} =
        post(admin, <<"/mucs/", (domain())/binary>>, maps:remove(name, Body)),
    {{<<"400">>, _}, <<"Missing nickname">>} =
        post(admin, <<"/mucs/", (domain())/binary>>, maps:remove(nick, Body)),
    {{<<"400">>, _}, <<"Missing owner JID">>} =
        post(admin, <<"/mucs/", (domain())/binary>>, maps:remove(owner, Body)),
    {{<<"400">>, _}, <<"Invalid room name">>} =
        post(admin, <<"/mucs/", (domain())/binary>>, Body#{name := <<"@invalid">>}),
    {{<<"400">>, _}, <<"Invalid owner JID">>} =
        post(admin, <<"/mucs/", (domain())/binary>>, Body#{owner := <<"@invalid">>}),
    {{<<"404">>, _}, <<"Given user not found">>} =
        post(admin, <<"/mucs/", (domain())/binary>>, Body#{owner := <<"baduser@baddomain">>}).

invite_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    BobJid = escalus_users:get_jid(Config1, bob),
    Name = set_up_room(Config1, AliceJid),
    Path = path([Name, "participants"]),
    Body = #{sender => AliceJid, recipient => BobJid, reason => <<"Join this room!">>},
    {{<<"400">>, _}, <<"Missing sender JID">>} =
        post(admin, Path, maps:remove(sender, Body)),
    {{<<"400">>, _}, <<"Missing recipient JID">>} =
        post(admin, Path, maps:remove(recipient, Body)),
    {{<<"400">>, _}, <<"Missing invite reason">>} =
        post(admin, Path, maps:remove(reason, Body)),
    {{<<"400">>, _}, <<"Invalid recipient JID">>} =
        post(admin, Path, Body#{recipient := <<"@badjid">>}),
    {{<<"400">>, _}, <<"Invalid sender JID">>} =
        post(admin, Path, Body#{sender := <<"@badjid">>}),
    {{<<"404">>, _}, <<"MUC domain not found">>} =
        post(admin, <<"/mucs/baddomain/", Name/binary, "/participants">>, Body),
    {{<<"404">>, _}, <<"Room not found">>} =
        post(admin, path(["thisroomdoesnotexist", "participants"]), Body).

kick_user_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    Name = ?config(room_name, Config1),
    {{<<"404">>, _}, <<"Room not found">>} = delete(admin, path([Name, "nick"])),
    set_up_room(Config1, AliceJid),
    mongoose_helper:wait_until(fun() -> check_if_moderator_not_found(Name) end, ok),
    %% Alice sends presence to the room, making her the moderator
    {ok, Alice} = escalus_client:start(Config1, alice, <<"res1">>),
    escalus:send(Alice, muc_helper:stanza_muc_enter_room(Name, <<"ali">>)),
    %% Alice gets her affiliation information and the room's subject line.
    escalus:wait_for_stanzas(Alice, 2),
    %% Kicking a non-existent nick succeeds in the current implementation
    {{<<"204">>, _}, <<>>} = delete(admin, path([Name, "nick"])),
    escalus_client:stop(Config, Alice).

%% @doc Check if the sequence below has already happened:
%%   1. Room notification to the owner is bounced back, because the owner is offline
%%   2. The owner is removed from the online users
%% As a result, a request to kick a user returns Error 404
check_if_moderator_not_found(RoomName) ->
    case delete(admin, path([RoomName, "nick"])) of
        {{<<"404">>, _}, <<"Moderator user not found">>} -> ok;
        {{<<"204">>, _}, _} -> not_yet
    end.

message_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    Name = set_up_room(Config1, AliceJid),
    Path = path([Name, "messages"]),
    Body = #{from => AliceJid, body => <<"Greetings!">>},
    % Message to a non-existent room succeeds in the current implementation
    {{<<"204">>, _}, <<>>} = post(admin, path(["thisroomdoesnotexist", "messages"]), Body),
    {{<<"400">>, _}, <<"Missing message body">>} = post(admin, Path, maps:remove(body, Body)),
    {{<<"400">>, _}, <<"Missing sender JID">>} = post(admin, Path, maps:remove(from, Body)),
    {{<<"400">>, _}, <<"Invalid sender JID">>} = post(admin, Path, Body#{from := <<"@invalid">>}).

%%--------------------------------------------------------------------
%% Ancillary (adapted from the MUC suite)
%%--------------------------------------------------------------------

set_up_room(Config, OwnerJID) ->
    % create a room first
    Name = ?config(room_name, Config),
    Path = path([]),
    Body = #{name => Name,
             owner => OwnerJID,
             nick => <<"ali">>},
    Res = rest_helper:post(admin, Path, Body),
    {{<<"201">>, _}, Name} = Res,
    Name.

make_distinct_name(Prefix) ->
    {_, S, US} = os:timestamp(),
    L = lists:flatten([integer_to_list(S rem 100), ".", integer_to_list(US)]),
    Suffix = list_to_binary(L),
    %% The bove is adapted from `escalus_fresh'.
    <<Prefix/binary, $-, Suffix/binary>>.

stanza_get_rooms() ->
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
        muc_helper:muc_host()).

has_room(JID, #xmlel{children = [ #xmlel{children = Rooms} ]}) ->
    RoomPred = fun(Item) ->
        exml_query:attr(Item, <<"jid">>) == JID
    end,
    lists:any(RoomPred, Rooms).

is_direct_invitation(Stanza) ->
    escalus:assert(is_message, Stanza),
    ?NS_JABBER_X_CONF = exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"xmlns">>}]).

direct_invite_has_reason(Stanza, Reason) ->
    Reason = exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"reason">>}]).

invite_body(Sender, Recipient, Reason) ->
    #{sender => escalus_client:short_jid(Sender),
      recipient => escalus_client:short_jid(Recipient),
      reason => Reason}.

wait_for_invite(Recipient, Reason) ->
    Stanza = escalus:wait_for_stanza(Recipient),
    is_direct_invitation(Stanza),
    direct_invite_has_reason(Stanza, Reason),
    get_room_name(get_room_jid(Stanza)).

get_room_jid(#xmlel{children = [ Invite ]}) ->
    exml_query:attr(Invite, <<"jid">>).

wait_for_group_message(Recipient) ->
    Got = escalus:wait_for_stanza(Recipient),
    escalus:assert(is_message, Got),
    exml_query:path(Got, [{element, <<"body">>}, cdata]).

user_sees_room(User, Room) ->
    escalus:send(User, stanza_get_rooms()),
    Stanza = escalus:wait_for_stanza(User),
    escalus:assert(is_stanza_from, [muc_helper:muc_host()], Stanza),
    has_room(muc_helper:room_address(Room), Stanza).

get_room_name(JID) ->
    escalus_utils:get_username(JID).

user_sends_message_to_room(User, Message, Room) ->
    Chat = escalus_stanza:chat_to(muc_helper:room_address(Room), Message),
    Stanza = escalus_stanza:setattr(Chat, <<"type">>, <<"groupchat">>),
    escalus:send(User, muc_helper:stanza_to_room(Stanza, Room)).

user_sees_message_from(User, Room, Times) ->
    user_sees_message_from(User, Room, Times, []).

user_sees_message_from(_, _, 0, Messages) ->
    lists:sort(Messages);
user_sees_message_from(User, Room, Times, Messages) ->
    Stanza = escalus:wait_for_stanza(User),
    UserRoomJID = exml_query:path(Stanza, [{attr, <<"from">>}]),
    Body = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    user_sees_message_from(User, Room, Times - 1, [{UserRoomJID, Body} | Messages]).

is_unavailable_presence_from(Stanza, RoomJID) ->
    escalus:assert(is_presence_with_type, [<<"unavailable">>], Stanza),
    escalus_assert:is_stanza_from(RoomJID, Stanza).

path(Items) ->
    AllItems = ["mucs", domain() | Items],
    iolist_to_binary([[$/, Item] || Item <- AllItems]).
