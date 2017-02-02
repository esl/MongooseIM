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
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, positive}].

groups() ->
    [{positive, [parallel], success_response() ++ complex()}].

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


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    muc_helper:load_muc(muc_helper:muc_host()),
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
        Host = <<"localhost">>,
        Path = <<"/mucs/", Host/binary>>,
        Name = ?config(room_name, Config),
        Body = #{name => Name,
                 owner => escalus_client:short_jid(Alice),
                 nick => <<"ali">>},
        {{<<"201">>, _}, <<"">>} = rest_helper:post(Path, Body),
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
        Path = <<"/mucs/localhost/", Name/binary, "/participants">>,
        Reason = <<"I think you'll like this room!">>,
        Body = #{sender => escalus_client:short_jid(Alice),
                 recipient => escalus_client:short_jid(Bob),
                 reason => Reason},
        {{<<"204">>, _}, <<"">>} = rest_helper:post(Path, Body),
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
        Host = <<"localhost">>,
        Path = <<"/mucs", $/, Host/binary, $/, Name/binary, $/, "messages">>,
        Message = <<"Greetings!">>,
        Body = #{from => escalus_client:short_jid(Bob),
                 body => Message},
        %% The HTTP call in question. Notice: status 200 because no
        %% resource is created.
        {{<<"204">>, _}, <<"">>} = rest_helper:post(Path, Body),
        Got = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, Got),
        Message = exml_query:path(Got, [{element, <<"body">>}, cdata])
    end).

kick_user_from_room(Config) ->
    escalus:fresh_story(Config,
      [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        %% Parameters for this test.
        Name = ?config(room_name, Config),
        Host = <<"localhost">>,
        Path = <<"/mucs",$/,Host/binary,$/,Name/binary,$/,"bobcat">>,
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
        {{<<"204">>, _}, <<"">>} = rest_helper:delete(Path),
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
    Host = <<"localhost">>,
    MUCPath = <<"/mucs/", Host/binary>>,
    Room = ?config(room_name, Config),
    RoomPath = <<MUCPath/binary, $/, Room/binary>>,
    RoomInvitePath = <<MUCPath/binary, $/, Room/binary, "/participants">>,
    Reason = <<"I think you'll like this room!">>,
    MessagePath = <<"/mucs", $/, Host/binary, $/, Room/binary, $/, "messages">>,
    Message = <<"Greetings!">>,
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            %% XMPP: Bob does not see a MUC room called 'wonderland'.
            false = user_sees_room(Bob, Room),
            %% HTTP: create a room on Alice's behalf.
            {{<<"201">>, _}, <<"">>} =
                rest_helper:post(MUCPath,
                                 #{name => Room,
                                   owner => escalus_client:short_jid(Alice),
                                   nick => <<"ali">>}),
            %% See comments under the create room test case.
            escalus:wait_for_stanzas(Alice, 3),
            %% XMPP: Kate sees the MUC room.
            true = user_sees_room(Kate, Room),
            %% HTTP: Alice invites Bob to the MUC room.
            {{<<"204">>, _}, <<"">>} =
                rest_helper:post(RoomInvitePath,
                                 invite_body(Alice, Bob, Reason)),
            %% XMPP: Bob recieves the invite to the MUC room.
            Room = wait_for_invite(Bob, Reason),
            %% HTTP: Alice invites Kate to the MUC room.
            {{<<"204">>, _}, <<"">>} =
                rest_helper:post(RoomInvitePath,
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
                rest_helper:post(MessagePath,
                                 #{from => escalus_client:short_jid(Alice),
                                   body => Message}),
            %% XMPP: All three recieve the message sent to the MUC room.
            [ Message = wait_for_group_message(User) || User <- [Alice, Bob, Kate] ],
            %% XMPP: Bob and Kate send a message to the MUC room.
            [ user_sends_message_to_room(U, M, Room)
              || {U, M} <- [{Bob, <<"I'm Bob.">>}, {Kate, <<"I'm Kate.">>}] ],
            %% XMPP: Alice recieves the messages from Bob and Kate.
            [<<"I'm Bob.">>, <<"I'm Kate.">>] =
                user_sees_message_from(Alice, [<<"bobcat">>, <<"kitkat">>], Room)
        end).


%%--------------------------------------------------------------------
%% Ancillary (adapted from the MUC suite)
%%--------------------------------------------------------------------

make_distinct_name(Prefix) ->
    {_, S, US} = erlang:now(),
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

user_sees_message_from(User, Nicks, Room) ->
    user_sees_message_from(User, Nicks, Room, []).

user_sees_message_from(_, [], _, Messages) ->
    lists:reverse(Messages);
user_sees_message_from(User, [Nick|Rest], Room,  Messages) ->
    Stanza = escalus:wait_for_stanza(User),
    UserRoomJID = muc_helper:room_address(Room, Nick),
    UserRoomJID = exml_query:path(Stanza, [{attr, <<"from">>}]),
    Body = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    user_sees_message_from(User, Rest, Room, [Body|Messages]).

is_unavailable_presence_from(Stanza, RoomJID) ->
    escalus_assert:is_presence_type(<<"unavailable">>, Stanza),
    escalus_assert:is_stanza_from(RoomJID, Stanza).
