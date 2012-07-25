%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
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
%%==============================================================================

-module(muc_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(MUC_HOST, <<"muc.localhost">>).
-define(MUC_CLIENT_HOST, <<"localhost/res1">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          {group, disco},
          {group, moderator},
          {group, admin},
          {group, admin_membersonly},
          {group, occupant},
          {group, owner},
          {group, room_management}
         ].

groups() -> [
             {disco, [sequence], [
                                  disco_service,
                                  disco_features,
                                  disco_rooms,
                                  disco_info,
                                  disco_items
                                 ]},
             {moderator, [sequence], [
                                      moderator_subject,
                                      moderator_subject_unauthorized,
                                      moderator_kick,
                                      moderator_kick_unauthorized,
                                      moderator_voice
                                     ]},
             {admin, [sequence], [
                                  admin_ban,
                                  admin_ban_list,
                                  admin_ban_higher_user,
                                  admin_membership,
                                  admin_member_list,
                                  admin_moderator,
                                  admin_moderator_revoke_owner,
                                  admin_moderator_list
                                 ]},
             {admin_membersonly, [sequence], [
                                              admin_mo_revoke
                                              %% fails, see testcase
                                              %% admin_mo_invite,
                                              %% fails, see testcase
                                              %% admin_mo_invite_mere
                                             ]},
             {occupant, [sequence], [
                                    groupchat_user_enter,
                                    groupchat_user_enter_no_nickname,
                                    muc_user_enter,
                                    deny_access_to_password_protected_room,
                                    enter_password_protected_room,
                                    deny_accesss_to_memebers_only_room,
                                    deny_entry_to_a_banned_user,
                                    deny_entry_nick_conflict
                                    ]},
             {owner, [sequence], [
                                  %% failing, see testcase for explanation
                                  %room_creation_not_allowed
                                 ]},
             {room_management, [sequence], [
                                            create_and_destroy_room
                                           ]}
            ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(moderator, Config) ->
    RoomName = <<"alicesroom">>,
    RoomNick = <<"alicesnick">>,
    Config1 = escalus:create_users(Config),
    [Alice | _] = ?config(escalus_users, Config1),
    start_room(Config1, Alice, RoomName, RoomNick,
        [{persistent, true}, {allow_change_subj, false}, {moderated, true},
         {members_by_default, false}]);

init_per_group(admin, Config) ->
    RoomName = <<"alicesroom">>,
    RoomNick = <<"alicesnick">>,
    Config1 = escalus:create_users(Config),
    [Alice | _] = ?config(escalus_users, Config1),
    start_room(Config1, Alice, RoomName, RoomNick, [{persistent, true}]);

init_per_group(admin_membersonly, Config) ->
    RoomName = <<"alicesroom">>,
    RoomNick = <<"alicesnick">>,
    Config1 = escalus:create_users(Config),
    [Alice | _] = ?config(escalus_users, Config1),
    start_room(Config1, Alice, RoomName, RoomNick, [{persistent, true},
        {members_only, true}]);

init_per_group(disco, Config) ->
    Config1 = escalus:create_users(Config),
    [Alice | _] = ?config(escalus_users, Config1),
    start_room(Config1, Alice, <<"alicesroom">>, <<"aliceonchat">>,
        [{persistent, true}]);

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(moderator, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);

end_per_group(admin, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);

end_per_group(admin_membersonly, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);

end_per_group(disco, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName = groupchat_user_enter, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = groupchat_user_enter_no_nickname, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = muc_user_enter, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = enter_non_anonymous_room, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{anonymous, false}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = deny_access_to_password_protected_room, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    %{password_protected, Password}?
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{password_protected, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = enter_password_protected_room, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{password_protected, true}, {password, ?PASSWORD}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = deny_accesss_to_memebers_only_room, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{members_only, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =deny_entry_to_a_banned_user, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =deny_entry_nick_conflict, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =deny_entry_user_limit_reached, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{max_users, 1}]),
    escalus:init_per_testcase(CaseName, Config1);

%init_per_testcase(CaseName =deny_entry_locked_room, Config) ->
%    escalus:init_per_testcase(CaseName, Config);

%init_per_testcase(CaseName =enter_room_with_logging, Config) ->
%    [Alice | _] = ?config(escalus_users, Config),
%    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{logging, true}]),
%    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).



end_per_testcase(CaseName = groupchat_user_enter, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName = groupchat_user_enter_no_nickname, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName = muc_user_enter, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName = deny_access_to_password_protected_room, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName = enter_password_protected_room, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName = deny_accesss_to_memebers_only_room, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName =deny_entry_to_a_banned_user, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName =deny_entry_nick_conflict, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName =deny_entry_user_limit_reached, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config);

%end_per_testcase(CaseName =deny_entry_locked_room, Config) ->
%    destroy_room(Config),
%    escalus:end_per_testcase(CaseName, Config);

%end_per_testcase(CaseName =enter_room_with_logging, Config) ->
%    destroy_room(Config),
%    escalus:end_per_testcase(CaseName, Config);


end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%%  Moderator use case tests
%%
%%  Tests the usecases described here :
%%  http://xmpp.org/extensions/xep-0045.html/#moderator
%%--------------------------------------------------------------------

%%  Examples 84-85
moderator_subject(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),

        %% Alice sets room subject
        escalus:send(Alice,
            stanza_room_subject(?config(room,Config), <<"Lets have a chat!">>)),

        %% Alice receives subject change message
        Message = escalus:wait_for_stanza(Alice),
        true = is_subject_message(Message, <<"Lets have a chat!">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"alice">>)], Message)
    end).

%%  Example 87
moderator_subject_unauthorized(Config) ->
    escalus:story(Config, [1,1], fun(_Alice, Bob) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob tries to set the room subject
        escalus:send(Bob,
            stanza_room_subject(?config(room,Config), <<"Lets have a chat!">>)),

        %% Bob should receive an error
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>],
            escalus:wait_for_stanza(Bob))
    end).

%%  Examples 89-92
%%  Apparently you user has to be in the room to kick someone, however XEP doesn't need that
moderator_kick(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Alice kicks Bob
        escalus:send(Alice, stanza_set_roles(
            ?config(room,Config), [{<<"bob">>,<<"none">>}])),

        %% Alice receives both iq result and Bob's unavailable presence
        escalus:assert_many([is_iq_result,
          fun(Stanza) -> is_unavailable_presence(Stanza, <<"307">>) andalso
              escalus_pred:is_stanza_from(
                  room_address(?config(room,Config)), Stanza) end],
          escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his presence
        is_unavailable_presence(escalus:wait_for_stanza(Bob), <<"307">>)
    end).

%%  Example 93
moderator_kick_unauthorized(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Bob tries to kick Alice
        escalus:send(Bob, stanza_set_roles(
            ?config(room,Config), [{<<"alice">>,<<"none">>}])),

        %% Bob should get an error
        escalus:assert(is_error, [<<"cancel">>,<<"not-allowed">>],
          escalus:wait_for_stanza(Bob))
    end).

%%  Examples 94-101
moderator_voice(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Alice grants voice to Bob
        escalus:send(Alice, stanza_set_roles(?config(room,Config),
            [{<<"bob">>,<<"participant">>}])),

        %% Alice receives success information and new Bob's presence
        Pred = fun(Stanza) ->
            is_presence_with_role(Stanza, <<"participant">>)
        end,
        escalus:assert_many([is_iq_result, Pred],
            escalus:wait_for_stanzas(Alice, 2)),

        %% Bob should receive his new presence
        escalus:assert(Pred, escalus:wait_for_stanza(Bob)),

        %% Revoke Bob's voice
        escalus:send(Alice, stanza_set_roles(?config(room,Config),
            [{<<"bob">>,<<"visitor">>}])),

        %% Alice receives success information and new Bob's presence
        Pred2 = fun(Stanza) ->
            is_presence_with_role(Stanza, <<"visitor">>)
        end,
        escalus:assert_many([is_iq_result, Pred2],
            escalus:wait_for_stanzas(Alice, 2)),

        %% Bob should receive his new presence
        escalus:assert(Pred2, escalus:wait_for_stanza(Bob))
    end).

%%--------------------------------------------------------------------
%%  Admin use case tests
%%
%%  Tests the usecases described here :
%%  http://xmpp.org/extensions/xep-0045.html/#admin
%%--------------------------------------------------------------------

%%    Examples 110-114
admin_ban(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 3),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),

        %% Alice bans Bob
        escalus:send(Alice, stanza_ban_user(Bob, ?config(room, Config))),
        
        %% Alice receives confirmation
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Stanza),

        %% Bob receives outcast presence
        Outcast = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], Outcast),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Outcast),
        true = is_presence_with_status_code(Outcast, <<"301">>),
        true = is_presence_with_affiliation(Outcast, <<"outcast">>),
        true = is_presence_with_role(Outcast, <<"none">>),

        %% Kate receives Bob's outcast presence
        BobOutcast = escalus:wait_for_stanza(Kate),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], BobOutcast),
        true = is_presence_with_affiliation(BobOutcast, <<"outcast">>),
        true = is_presence_with_role(BobOutcast, <<"none">>),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config), <<"bob">>)],
            BobOutcast)
        %% ejabberd doesn't send jid attribute in presence as in ex. 114
    end).

%%    Example 115
admin_ban_higher_user(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        %% Bob tries to ban Alice
        escalus:send(Bob, stanza_ban_user(Alice, ?config(room, Config))),

        %% Bob receives an error
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>],
            escalus:wait_for_stanza(Bob))
    end).

%%    Examples 116-119
admin_ban_list(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        %% Alice requests ban list
        escalus:send(Alice, stanza_ban_list_request(?config(room, Config))),
        List = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List),

        %% Bob should be banned
        true = is_iq_with_affiliation(List, <<"outcast">>),
        is_iq_with_jid(List, Bob),

        %% Remove Bob's ban
        stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, []), ?config(room, Config)),
        Items = [{<<"none">>, escalus_utils:get_short_jid(Bob)}],
        escalus:send(Alice, stanza_admin_list(?config(room, Config), Items)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Request again
        escalus:send(Alice, stanza_ban_list_request(?config(room, Config))),
        List2 = escalus:wait_for_stanza(Alice),

        %% Noone should be banned
        [] = List2#xmlelement.body
    end).

%%  Examples 120-127
admin_membership(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 3),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),

        %% Alice grants membership to Bob
        Items = [{<<"member">>, escalus_utils:get_short_jid(Bob)}],
        escalus:send(Alice, stanza_admin_list(?config(room, Config), Items)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = is_presence_with_affiliation(Bobs, <<"member">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = is_presence_with_affiliation(Kates, <<"member">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Alice revokes Bob's membership
        Items2 = [{<<"none">>, escalus_utils:get_short_jid(Bob)}],
        escalus:send(Alice, stanza_admin_list(?config(room, Config), Items2)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives his loss of membership presence
        Bobs2 = escalus:wait_for_stanza(Bob),
        true = is_presence_with_affiliation(Bobs2, <<"none">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Bobs2),

        %% Kate receives Bob's loss of membership presence
        Kates2 = escalus:wait_for_stanza(Kate),
        true = is_presence_with_affiliation(Kates2, <<"none">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Kates2)
    end).

%%  Examples 129-136
admin_member_list(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 3),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),

        %% Alice requests member list
        escalus:send(Alice, stanza_affiliation_list_request(
            ?config(room, Config), <<"member">>)),
        List = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List),

        %% List should be empty
        [] = List#xmlelement.body,

        %% Make Bob a member
        Items = [{<<"member">>, escalus_utils:get_short_jid(Bob)}],
        escalus:send(Alice, stanza_admin_list(?config(room, Config), Items)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = is_presence_with_affiliation(Bobs, <<"member">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = is_presence_with_affiliation(Kates, <<"member">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Request again
        escalus:send(Alice, stanza_affiliation_list_request(
              ?config(room, Config), <<"member">>)),
        List2 = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List2),

        %% Bob should be on the list
        true = is_iq_with_affiliation(List2, <<"member">>),
        is_iq_with_jid(List2, Bob),

        %% Revoke Bob's membership and make Kate a member
        Items2 = [{<<"none">>, escalus_utils:get_short_jid(Bob)},
            {<<"member">>, escalus_utils:get_short_jid(Kate)}],
        escalus:send(Alice, stanza_admin_list(?config(room,Config), Items2)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives his and Kate's presence
        Preds = [
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"kate">>),Stanza) andalso
                is_presence_with_affiliation(Stanza, <<"member">>)
            end,
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_affiliation(Stanza, <<"none">>)
            end
        ],
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Bob, 2)),

        %% Kate receives her and Bob's presence
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Kate, 2))
  end).

%%  Examples 137-145
admin_moderator(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Grant bob moderator status
        escalus:send(Alice, stanza_set_roles(
            ?config(room, Config), [{<<"bob">>,<<"moderator">>}])),
        escalus:assert_many([is_iq_result, is_presence], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = is_presence_with_role(Bobs, <<"moderator">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = is_presence_with_role(Kates, <<"moderator">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Revoke bob moderator status
        Pred = fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"participant">>)
        end,

        escalus:send(Alice, stanza_set_roles(
            ?config(room, Config), [{<<"bob">>, <<"participant">>}])),
        escalus:assert_many([is_iq_result, Pred], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his loss of moderator presence
        Bobs2 = escalus:wait_for_stanza(Bob),
        true = is_presence_with_role(Bobs2, <<"participant">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Bobs2),

        %% Kate receives Bob's loss of moderator presence
        Kates2 = escalus:wait_for_stanza(Kate),
        true = is_presence_with_role(Kates2, <<"participant">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Kates2)

    end).

%%  Examples 145, 150
admin_moderator_revoke_owner(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),

        %% Alice tries to revoke moderator status from herself
        escalus:send(Alice, stanza_set_roles(
             ?config(room, Config), [{<<"alice">>, <<"participant">>}])),

        %% Should be an error
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>],
            escalus:wait_for_stanza(Alice))
    end).

%%  Examples 146-150
admin_moderator_list(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Request moderator list
        escalus:send(Alice, stanza_role_list_request(
            ?config(room, Config), <<"moderator">>)),
        List = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List),

        %% Grant Bob and Kate moderators role
        Preds = [
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"kate">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"moderator">>)
            end,
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"moderator">>)
            end
        ],
        escalus:send(Alice, stanza_set_roles(?config(room, Config),
            [{<<"bob">>,<<"moderator">>},{<<"kate">>,<<"moderator">>}])),
        escalus:assert_many([is_iq_result | Preds], escalus:wait_for_stanzas(Alice,3)),

        %% Bob receives his and Kate's moderator presence
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Bob,2)),

        %% Kate receives her and Bob's moderator presence
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Kate,2)),

        %% Revoke Bob's and Kate's roles
        Preds2 = [
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"kate">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"participant">>)
            end,
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"participant">>)
            end
        ],
        escalus:send(Alice, stanza_set_roles(?config(room, Config),
              [{<<"bob">>,<<"participant">>},{<<"kate">>,<<"participant">>}])),
        escalus:assert_many([is_iq_result|Preds2], escalus:wait_for_stanzas(Alice,3)),

        %% Bob receives his and Kate's participant presence
        escalus:assert_many(Preds2, escalus:wait_for_stanzas(Bob,2)),

        %% Kate receives her and Bob's participant presence
        escalus:assert_many(Preds2, escalus:wait_for_stanzas(Kate,2))
    end).

%%  Example 128
admin_mo_revoke(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Make Bob and Kate members
        Items = [{<<"member">>, escalus_utils:get_short_jid(Bob)},
            {<<"member">>, escalus_utils:get_short_jid(Kate)}],
        escalus:send(Alice, stanza_admin_list(?config(room,Config), Items)),
        escalus:wait_for_stanza(Alice),

        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 3),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),

        %% Alice revokes Bob's membership
        Items2 = [{<<"none">>, escalus_utils:get_short_jid(Bob)}],
        escalus:send(Alice, stanza_admin_list(?config(room, Config), Items2)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Skip Bob's lost of membership presence (tested in the other case)
        escalus:wait_for_stanza(Bob),

        %% Kate receives Bob's loss of unavailable presence
        Kates = escalus:wait_for_stanza(Kate),
        true = is_membership_presence(Kates, <<"none">>, <<"none">>),
        true = is_unavailable_presence(Kates, <<"321">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config),<<"bob">>)], Kates)
    end).

%%  Example 134
%%  This test fails
%%  ejabberd doesn't send an invitation after adding user to a member list
admin_mo_invite(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Make Bob a member
        Items = [{<<"member">>, escalus_utils:get_short_jid(Bob)}],
        escalus:send(Alice, stanza_admin_list(?config(room,Config), Items)),
        escalus:wait_for_stanza(Alice),

        %% Bob should receive an invitation
        Inv = escalus:wait_for_stanza(Bob),
        is_invitation(Inv),
        escalus:assert(is_stanza_from, [room_address(?config(room,Config))], Inv)
    end).

%%  Example 135
%%  This test fails
%%  ejabberd returns cancel/not-allowed error while it should return auth/forbidden according to XEP
admin_mo_invite_mere(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Make Bob a member
        Items = [{<<"member">>, escalus_utils:get_short_jid(Bob)}],
        escalus:send(Alice, stanza_admin_list(?config(room,Config), Items)),
        escalus:wait_for_stanza(Alice),

        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob tries to invite Kate
        escalus:send(Bob, stanza_mediated_invitation(?config(room,Config), Kate)),

        %% He should receive an error
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>],
          escalus:wait_for_stanza(Bob))
    end).

%%--------------------------------------------------------------------
%%  Occupant use case tests
%%
%%  Tests the usecases described here :
%%  http://xmpp.org/extensions/xep-0045.html/#user
%%
%%  Issue: the service does not broadcast the new users presence. This behaviour
%%  should be configurable and possibly enabled by default, is neither.
%%  This makes some of the use cases untestable
%%--------------------------------------------------------------------

%Example 18
groupchat_user_enter(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice, Bob) ->
        Enter_room_stanza = stanza_groupchat_enter_room(<<"alicesroom">>, <<"bob">>),
        escalus:send(Bob, Enter_room_stanza),
        Presence = escalus:wait_for_stanza(Bob),
        escalus_assert:is_presence_stanza(Presence),
        From = << "alicesroom" ,"@", ?MUC_HOST/binary, "/", "bob" >>,
        From = exml_query:attr(Presence, <<"from">>)

        end).

%Example 19
%No error message sent from the server
groupchat_user_enter_no_nickname(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        EnterRoomStanza = stanza_groupchat_enter_room_no_nick(<<"alicesroom">>),
        error_logger:info_msg("Enter room stanza: ~n~p", [EnterRoomStanza]),
        escalus:send(Bob, EnterRoomStanza),

        timer:sleep(1000),

        %% no error message here!
        %% processone ejabberd crashes with function clause, nick (binary) is required
        %Presence2 = escalus:wait_for_stanza(Bob),
        %escalus_assert:is_presence_stanza(Presence2),
        %From = <<"alicesroom" ,"@", ?MUC_HOST/binary, "/", "aliceonchat" >>,
        %From = exml_query:attr(Presence2, <<"from">>),

        escalus_assert:has_no_stanzas(Alice),   %!!
        escalus_assert:has_no_stanzas(Bob)

        end).

% Examples 20, 21, 22
% No broadcast message about now user's presence. The feature should be configurable, but does
% not seem to be.
muc_user_enter(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice, Bob) ->
        %error_logger:info_msg("Configuration form: ~n~n~n~p~n",[stanza_configuration_form(get_from_config(room, Config), [])]),
        %Bob enters the room
        EnterRoomStanza = stanza_muc_enter_room(<<"alicesroom">>, <<"aliceonchat">>),
        error_logger:info_msg("Enter room stanza: ~n~p", [EnterRoomStanza]),
        escalus:send(Bob, EnterRoomStanza),
        Presence = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("Bob's new user presence notification: ~n~p~n",[Presence]),
        escalus_assert:is_presence_stanza(Presence),
        From = << "alicesroom" ,"@", ?MUC_HOST/binary, "/", "aliceonchat" >>,
        From = exml_query:attr(Presence, <<"from">>),

        Topic = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("Bobs topic notification: ~n~p~n",[Topic])
        %possible new user broadcast presence messages
    end).

% Example 23 missing
% Example 24 impossible to test due to the issues with presence broadcast.

% Example 25, 26
enter_non_anonymous_room(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        %Bob enters the room
        Enter_room_stanza = stanza_muc_enter_room(<<"alicesroom">>, <<"aliceonchat">>),
        error_logger:info_msg("Enter room stanza: ~n~p", [Enter_room_stanza]),
        escalus:send(Bob, Enter_room_stanza),
        %A message that informs users about this room being non-anonymous.
        %Should send aprecence with a 100 staus code. Sends a simple message instead
        Message = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("Info message about the room being non-anonymous: ~n~p~n", [Message]),
        Presence = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("Bob's new user presence notification: ~n~p~n",[Presence]),
        escalus_assert:is_presence_stanza(Presence),
        From = << "alicesroom" ,"@", ?MUC_HOST/binary, "/", "aliceonchat" >>,
        From = exml_query:attr(Presence, <<"from">>),

        JID = <<"bob", "@", ?MUC_CLIENT_HOST/binary>>,
        error_logger:info_msg("item: ~n~p~n", [exml_query:subelement(exml_query:subelement(Presence, <<"x">>), <<"item">>)]),
        JID = exml_query:attr(
                        exml_query:subelement(
                            exml_query:subelement(Presence, <<"x">>), <<"item">>) ,<<"jid">>),
        %error_logger:info_msg("suelement : ~n~p~n", [FullJID=exml_query:subelement(<<"item">>)]),
        Topic = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("Bobs topic notification: ~n~p~n",[Topic])
        %possible new user broadcast presence messages
    end).

% Semi-anonymous rooms untestable due to the issues with new user presence broadcast settings.
% (No examples, section 7.2.5)

%Example 27
deny_access_to_password_protected_room(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        Enter_room_stanza = stanza_muc_enter_room(<<"alicesroom">>, <<"aliceonchat">>),
        error_logger:info_msg("Enter room stanza: ~n~p", [Enter_room_stanza]),
        escalus:send(Bob, Enter_room_stanza),
        Message = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("No password error message: ~n~p~n", [Message]),
        escalus_assert:is_error(Message, <<"auth">>, <<"not-authorized">>)
    end).

%Example 28
enter_password_protected_room(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        %Bob enters the room
        Enter_room_stanza = stanza_muc_enter_password_protected_room(<<"alicesroom">>, <<"aliceonchat">>, ?PASSWORD),
        error_logger:info_msg("Enter room stanza: ~n~p", [Enter_room_stanza]),
        escalus:send(Bob, Enter_room_stanza),
        Presence = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("Bob's new user presence notification: ~n~p~n",[Presence]),
        escalus_assert:is_presence_stanza(Presence),
        From = << "alicesroom" ,"@", ?MUC_HOST/binary, "/", "aliceonchat" >>,
        From = exml_query:attr(Presence, <<"from">>),
        Topic = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("Bobs topic notification: ~n~p~n",[Topic])
        %possible new user broadcast presence messages
    end).

%Example 29
deny_accesss_to_memebers_only_room(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        Enter_room_stanza = stanza_muc_enter_room(<<"alicesroom">>, <<"aliceonchat">>),
        error_logger:info_msg("Enter room stanza: ~n~p", [Enter_room_stanza]),
        escalus:send(Bob, Enter_room_stanza),
        Message = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("Not a member error message: ~n~p~n", [Message]),
        escalus_assert:is_error(Message, <<"auth">>, <<"registration-required">>)
    end).

%Example 30
deny_entry_to_a_banned_user(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,  Bob) ->
        %% Alice bans Bob
        escalus:send(Alice, stanza_ban_user(Bob, ?config(room, Config))),
        %% Alice receives confirmation
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Stanza),

        Enter_room_stanza = stanza_muc_enter_room(<<"alicesroom">>, <<"aliceonchat">>),
        error_logger:info_msg("Enter room stanza: ~n~p", [Enter_room_stanza]),
        escalus:send(Bob, Enter_room_stanza),
        Message = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("Banned message ~n~p~n", [Message]),
        escalus_assert:is_error(Message, <<"auth">>, <<"forbidden">>)
    end).

%Examlpe 31
deny_entry_nick_conflict(Config) -> 
    escalus:story(Config, [1, 1, 1], fun(_Alice,  Bob, Eve) ->
        Enter_room_stanza = stanza_muc_enter_room(<<"alicesroom">>, <<"bob">>),
        error_logger:info_msg("Enter room stanza: ~n~p", [Enter_room_stanza]),
        escalus:send(Bob, Enter_room_stanza),
        escalus:send(Eve, Enter_room_stanza),
        escalus:wait_for_stanzas(Bob, 2),
        Message  =escalus:wait_for_stanza(Eve),
        error_logger:info_msg("Not a member error message: ~n~p~n", [Message]),
        escalus_assert:is_error(Message, <<"cancel">>, <<"conflict">>)
    end).

%Example 32
deny_entry_user_limit_reached(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        escalus:send(Bob,stanza_muc_enter_room(<<"alicesroom">>, <<"aliceonchat">>)),
        Message = escalus:wait_for_stanza(Bob),
        error_logger:info_msg("Not a member error message: ~n~p~n", [Message]),
        escalus_assert:is_error(Message, <<"wait">>, <<"service-unavailable">>)
    end).

%%Example 33 
%%requires creating a locked room in the init per testcase function somehow
%deny_entry_locked_room(Config) ->
%    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
%        escalus:send(Bob,stanza_muc_enter_room(<<"alicesroom">>, <<"aliceonchat">>)),
%        Message = escalus:wait_for_stanza(Bob),
%        error_logger:info_msg("Not a member error message: ~n~p~n", [Message]),
%        escalus_assert:is_error(Message, <<"cancal">>, <<"item-not-found">>)
%    end).

% Nonexistent rooms:
% If a user seeks to enter a non-existent room, servers behaviour is undefined.
% See the xep: http://xmpp.org/extensions/xep-0045.html/#enter-nonexistent
%

%Example 34
%requires the service to send new occupant's presence to him. This does not happen
%this test is unfinished
%enter_room_with_logging(Config) ->
%    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
%        %Bob enters the room
%        escalus:send(Bob,stanza_muc_enter_room(<<"alicesroom">>, <<"aliceonchat">>)),
%        Presence = escalus:wait_for_stanza(Bob),
%        error_logger:info_msg("Bob's new user presence notification: ~n~p~n",[Presence]),
%        escalus_assert:is_presence_stanza(Presence),
%        From = << "alicesroom" ,"@", ?MUC_HOST/binary, "/", "aliceonchat" >>,
%        From = exml_query:attr(Presence, <<"from">>),
%        escalus:wait_for_stanza(Bob)
%        %possible new user broadcast presence messages
%    end).
%
%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

disco_service(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:service_discovery(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(has_service, [?MUC_HOST], Stanza),
        escalus:assert(is_stanza_from, [escalus_config:get_config(ejabberd_domain, Config)], Stanza)
    end).

disco_features(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, stanza_get_features()),
        Stanza = escalus:wait_for_stanza(Alice),
        has_features(Stanza),
        escalus:assert(is_stanza_from, [?MUC_HOST], Stanza)
    end).

disco_rooms(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, stanza_get_rooms()),
        %% we should have 1 room, created in init
        Stanza = escalus:wait_for_stanza(Alice),
        count_rooms(Stanza, 1),
        has_room(room_address(<<"alicesroom">>), Stanza),
        escalus:assert(is_stanza_from, [?MUC_HOST], Stanza)
    end).

disco_info(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, stanza_to_room(escalus_stanza:iq_get(?NS_DISCO_INFO,[]), <<"alicesroom">>)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Stanza),
        has_feature(Stanza, <<"muc_persistent">>)
    end).

disco_items(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        escalus:send(Alice, stanza_join_room(<<"alicesroom">>, <<"nicenick">>)),
        _Stanza = escalus:wait_for_stanza(Alice),

        escalus:send(Bob, stanza_to_room(escalus_stanza:iq_get(?NS_DISCO_ITEMS,[]), <<"alicesroom">>)),
        Stanza2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_iq_result, Stanza2)
    end).

create_and_destroy_room(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        Room1 = stanza_create_room(<<"room1">>, <<"nick1">>),
        escalus:send(Alice, Room1),
        %Alice gets topic message after creating the room
        [S, _S2] = escalus:wait_for_stanzas(Alice, 2),
        was_room_created(S),

        DestroyRoom1 = stanza_destroy_room(<<"room1">>),
        escalus:send(Alice, DestroyRoom1),
        [Presence, Iq] = escalus:wait_for_stanzas(Alice, 2),
        was_room_destroyed(Iq),
        was_destroy_presented(Presence)
    end).

%% Example 152. Service Informs User of Inability to Create a Room
%% As of writing this testcase (2012-07-24) it fails. Room is not created
%% as expected, but the returned error message is not the one specified by XEP.
%% ejabberd returns 'forbidden' while it ought to return 'not-allowed'.
room_creation_not_allowed(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus_ejabberd:with_global_option({access,muc_create,global},
                                            [{deny,all}], fun() ->

            escalus:send(Alice, stanza_create_room(<<"room1">>, <<"nick1">>)),
            escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>],
                           escalus:wait_for_stanza(Alice))

        end)
    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
get_from_config(Option, [{Option, Value}|_T]) ->
    Value;
get_from_config(Option, [_H|T]) ->
    get_from_config(Option, T);
get_from_config(_Option, []) ->
    throw(no_such_option).

generate_rpc_jid({_,User}) ->
    {username, Username} = lists:keyfind(username, 1, User),
    {server, Server} = lists:keyfind(server, 1, User),
    JID = <<Username/binary, "@", Server/binary, "/rpc">>,
    {jid, JID, Username, Server, <<"rpc">>}.

%Groupchat 1.0 protocol
stanza_groupchat_enter_room(Room, Nick) ->
    stanza_to_room(escalus_stanza:presence(<<"available">>), Room, Nick).


stanza_groupchat_enter_room_no_nick(Room) ->
    stanza_to_room(escalus_stanza:presence(<<"available">>), Room).


%Basic MUC protocol
stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence(  <<"available">>,
                                [#xmlelement{ name = <<"x">>, attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}]),
        Room, Nick).

stanza_muc_enter_password_protected_room(Room, Nick, Password) ->
    stanza_to_room(
        escalus_stanza:presence(  <<"available">>,
                                [#xmlelement{ name = <<"x">>, attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}],
                                             body=[#xmlelement{name = <<"password">>, body = [#xmlcdata{content=[Password]}]} ]}]),
        Room, Nick).


start_room(Config, User, Room, Nick, Opts) ->
    From = generate_rpc_jid(User),
    escalus_ejabberd:rpc(mod_muc, create_room,
        [<<"localhost">>, Room, From, Nick, Opts]),
    [{nick, Nick}, {room, Room} | Config].

destroy_room(Config) ->
    case escalus_ejabberd:rpc(ets, lookup, [muc_online_room,
        {?config(room, Config), <<"muc.localhost">>}]) of
        [{_,_,Pid}|_] -> gen_fsm:send_all_state_event(Pid, destroy);
        _ -> ok
    end.

room_address(Room) ->
    <<Room/binary, "@", ?MUC_HOST/binary>>.

room_address(Room, Nick) ->
    <<Room/binary, "@", ?MUC_HOST/binary, "/", Nick/binary>>.

%%--------------------------------------------------------------------
%% Helpers (stanzas)
%%--------------------------------------------------------------------

stanza_room_subject(Room, Subject) ->
    stanza_to_room(#xmlelement{name = <<"message">>,
        attrs = [{<<"type">>,<<"groupchat">>}],
        body = [#xmlelement{
            name = <<"subject">>,
            body = [exml:escape_cdata(Subject)]
        }]
    }, Room).

stanza_mediated_invitation(Room, Invited) ->
    Payload = [ #xmlelement{name = <<"invite">>,
        attrs = [{<<"to">>, escalus_utils:get_short_jid(Invited)}]} ],
    stanza_to_room(#xmlelement{name = <<"message">>,
        body = [ #xmlelement{
            name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
            body = Payload }
        ]}, Room).

stanza_set_roles(Room, List) ->
    Payload = [ #xmlelement{name = <<"item">>,
        attrs = [{<<"nick">>, Nick}, {<<"role">>, Role}]} || {Nick,Role} <- List ],
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, Payload), Room).

stanza_role_list_request(Room, Role) ->
    Payload = [ #xmlelement{name = <<"item">>,
        attrs = [{<<"role">>, Role}]} ],
    stanza_to_room(escalus_stanza:iq_get(?NS_MUC_ADMIN, Payload), Room).

stanza_affiliation_list_request(Room, Affiliation) ->
    Payload = [ #xmlelement{name = <<"item">>,
        attrs = [{<<"affiliation">>, Affiliation}]} ],
    stanza_to_room(escalus_stanza:iq_get(?NS_MUC_ADMIN, Payload), Room).

stanza_admin_list(Room, Items) ->
    Payload = [ #xmlelement{name = <<"item">>,
                            attrs = [{<<"affiliation">>, Affiliation},
                                     {<<"jid">>, JID}]}
              || {Affiliation, JID} <- Items ],
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, Payload), Room).

stanza_ban_list_request(Room) ->
    Payload = #xmlelement{name = <<"item">>,
        attrs = [{<<"affiliation">>, <<"outcast">>}]},
    stanza_to_room(escalus_stanza:iq_get(?NS_MUC_ADMIN, Payload), Room).

stanza_ban_user(User, Room) ->
  stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, #xmlelement{
      name = <<"item">>,
      attrs = [{<<"affiliation">>,<<"outcast">>},
               {<<"jid">>, escalus_utils:get_short_jid(User)}]
      }), Room).

stanza_join_room(Room, Nick) ->
    stanza_to_room(#xmlelement{name = <<"presence">>, body =
        #xmlelement{
            name = <<"x">>,
            attrs = [{<<"xmlns">>,<<"http://jabber.org/protocol/muc">>}]
        }
    },Room, Nick).

stanza_configuration_form(Room, Params) ->
    DefaultParams = [{<<"FORM_TYPE">>,<<"http://jabber.org/protocol/muc#roomconfig">>,<<"hidden">>}],
    FinalParams = lists:foldl(
        fun({Key,_Val,_Type},Acc) ->
            lists:keydelete(Key,1,Acc)
        end,
        DefaultParams, Params) ++ Params,
    XPayload = [ form_field(FieldData) || FieldData <- FinalParams ],
    Payload = #xmlelement{
        name = <<"x">>,
        attrs = [{<<"xmlns">>,<<"jabber:x:data">>}, {<<"type">>,<<"submit">>}],
        body = XPayload
    },
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_OWNER, Payload), Room).

form_field({Var, Value, Type}) ->
    #xmlelement{ name  = <<"field">>,
                 attrs = [{<<"type">>, Type},{<<"var">>, Var}],
                 body  = [#xmlelement{ name = <<"value">>,
                                       body = [#xmlcdata{content = Value}] }] }.

stanza_destroy_room(Room) ->
    Payload = [ #xmlelement{name = <<"destroy">>} ],
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_OWNER, Payload), Room).

stanza_create_room(Room, Nick) ->
    stanza_to_room(#xmlelement{name = <<"presence">>}, Room, Nick).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

stanza_get_rooms() ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%   id='zb8q41f4'
    %%   to='chat.shakespeare.lit'
    %%   type='get'>
    %% <query xmlns='http://jabber.org/protocol/disco#items'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
        ?MUC_HOST).

stanza_get_features() ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%     id='lx09df27'
    %%     to='chat.shakespeare.lit'
    %%     type='get'>
    %%  <query xmlns='http://jabber.org/protocol/disco#info'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_INFO, []), <<"to">>,
        ?MUC_HOST).

stanza_get_services(Config) ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%     id='h7ns81g'
    %%     to='shakespeare.lit'
    %%     type='get'>
    %%   <query xmlns='http://jabber.org/protocol/disco#items'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
        escalus_config:get_config(ejabberd_domain, Config)).

%%--------------------------------------------------------------------
%% Helpers (assertions)
%%--------------------------------------------------------------------

is_groupchat_message(Stanza) ->
    escalus_pred:is_message(Stanza) andalso
    escalus_pred:has_type(<<"groupchat">>, Stanza).

is_subject_message(Stanza) ->
    is_groupchat_message(Stanza) andalso
    exml_query:subelement(Stanza, <<"subject">>) /= undefined.

is_subject_message(Stanza, Subject) ->
    is_groupchat_message(Stanza) andalso
    exml_query:path(Stanza, [{element,<<"subject">>},cdata]) == Subject.

is_unavailable_presence(Stanza, Status) ->
    escalus_pred:is_presence_with_type(<<"unavailable">>,Stanza) andalso
    is_presence_with_status_code(Stanza, Status).

is_membership_presence(Stanza, Affiliation, Role) ->
    is_presence_with_affiliation(Stanza, Affiliation) andalso
    is_presence_with_role(Stanza, Role).

is_invitation(Stanza) ->
    escalus:assert(is_message, Stanza),
    #xmlelement{} = exml_query:path(Stanza, [{element, <<"x">>}, {element, <<"invite">>}]).

is_presence_with_role(Stanza, Role) ->
    is_with_role(exml_query:subelement(Stanza, <<"x">>), Role).

is_iq_with_role(Stanza, Role) ->
    is_with_role(exml_query:subelement(Stanza, <<"query">>), Role).

is_with_role(Stanza, Role) ->
    Item = exml_query:subelement(Stanza, <<"item">>),
    Role == exml_query:attr(Item, <<"role">>).

is_presence_with_affiliation(Stanza, Affiliation) ->
    is_affiliation(exml_query:subelement(Stanza, <<"x">>), Affiliation).

is_iq_with_affiliation(Stanza, Affiliation) ->
    is_affiliation(exml_query:subelement(Stanza, <<"query">>), Affiliation).

is_affiliation(Stanza, Affiliation) ->
    Item = exml_query:subelement(Stanza, <<"item">>),
    Affiliation == exml_query:attr(Item, <<"affiliation">>).

is_presence_with_jid(Stanza, User) ->
    is_jid(exml_query:subelement(Stanza, <<"x">>), User).

is_iq_with_jid(Stanza, User) ->
    is_jid(exml_query:subelement(Stanza, <<"query">>), User).

is_jid(Stanza, User) ->
    Item = exml_query:subelement(Stanza, <<"item">>),
    JID = escalus_utils:get_short_jid(User),
    JID = exml_query:attr(Item, <<"jid">>).

is_presence_with_status_code(Presence, Code) ->
    escalus:assert(is_presence, Presence),
    Code == exml_query:path(Presence, [{element, <<"x">>}, {element, <<"status">>},
        {attr, <<"code">>}]).

has_feature(Stanza, Feature) ->
    Features = exml_query:path(Stanza, [{element, <<"query">>}, {elements, <<"feature">>}]),
    true = lists:any(fun(Item) ->
                        exml_query:attr(Item, <<"var">>) == Feature
                     end,
                     Features).

was_destroy_presented(#xmlelement{body = [Items]} = Presence) ->
    #xmlelement{} = exml_query:subelement(Items, <<"destroy">>),
    <<"unavailable">> = exml_query:attr(Presence, <<"type">>).

was_room_destroyed(Query) ->
    <<"result">> = exml_query:attr(Query, <<"type">>).

was_room_created(#xmlelement{body = [ Query ]}) ->
    #xmlelement{} = Status = exml_query:subelement(Query, <<"status">>),
    <<"201">> = exml_query:attr(Status, <<"code">>),

    #xmlelement{} = Item = exml_query:subelement(Query, <<"item">>),
    <<"owner">> = exml_query:attr(Item, <<"affiliation">>),
    <<"moderator">> = exml_query:attr(Item, <<"role">>).

has_room(JID, #xmlelement{body = [ #xmlelement{body = Rooms} ]}) ->
    %% <iq from='chat.shakespeare.lit'
    %%   id='zb8q41f4'
    %%   to='hag66@shakespeare.lit/pda'
    %%   type='result'>
    %% <query xmlns='http://jabber.org/protocol/disco#items'>
    %%    <item jid='heath@chat.shakespeare.lit'
    %%         name='A Lonely Heath'/>
    %%    <item jid='coven@chat.shakespeare.lit'
    %%         name='A Dark Cave'/>
    %%    <item jid='forres@chat.shakespeare.lit'
    %%         name='The Palace'/>
    %%     <item jid='inverness@chat.shakespeare.lit'
    %%         name='Macbeth&apos;s Castle'/>
    %%   </query>
    %% </iq>

    RoomPred = fun(Item) ->
        exml_query:attr(Item, <<"jid">>) == JID
    end,
    true = lists:any(RoomPred, Rooms).

count_rooms(#xmlelement{body = [ #xmlelement{body = Rooms} ]}, N) ->
    N = length(Rooms).

has_features(#xmlelement{body = [ Query ]}) ->
    %%<iq from='chat.shakespeare.lit'
    %%  id='lx09df27'
    %%  to='hag66@shakespeare.lit/pda'
    %%  type='result'>
    %%  <query xmlns='http://jabber.org/protocol/disco#info'>
    %%    <identity
    %%      category='conference'
    %%      name='Shakespearean Chat Service'
    %%      type='text'/>
    %%      <feature var='http://jabber.org/protocol/muc'/>
    %%  </query>
    %%</iq>

    Identity = exml_query:subelement(Query, <<"identity">>),
    <<"conference">> = exml_query:attr(Identity, <<"category">>),
    #xmlelement{name = _Name, attrs = _Attrs, body = _Body} = exml_query:subelement(Query, <<"feature">>).

has_muc(#xmlelement{body = [ #xmlelement{body = Services} ]}) ->
    %% should be along the lines of (taken straight from the XEP):
    %% <iq from='shakespeare.lit'
    %%     id='h7ns81g'
    %%     to='hag66@shakespeare.lit/pda'
    %%     type='result'>
    %%   <query xmlns='http://jabber.org/protocol/disco#items'>
    %%     <item jid='chat.shakespeare.lit'
    %%           name='Chatroom Service'/>
    %%   </query>
    %% </iq>

    %% is like this:
    %% {xmlelement,<<"iq">>,
    %%     [{<<"from">>,<<"localhost">>},
    %%         {<<"to">>,<<"alice@localhost/res1">>},
    %%         {<<"id">>,<<"a5eb1dc70826598893b15f1936b18a34">>},
    %%         {<<"type">>,<<"result">>}],
    %%     [{xmlelement,<<"query">>,
    %%             [{<<"xmlns">>,
    %%                     <<"http://jabber.org/protocol/disco#items">>}],
    %%             [{xmlelement,<<"item">>,
    %%                     [{<<"jid">>,<<"vjud.localhost">>}],
    %%                     []},
    %%                 {xmlelement,<<"item">>,
    %%                     [{<<"jid">>,<<"pubsub.localhost">>}],
    %%                     []},
    %%                 {xmlelement,<<"item">>,
    %%                     [{<<"jid">>,<<"muc.localhost">>}],
    %%                     []},
    %%                 {xmlelement,<<"item">>,
    %%                     [{<<"jid">>,<<"irc.localhost">>}],
    %%                     []}]}]}
    %% how to obtaing output like the above? simply put this in the test case:
    %% S = escalus:wait_for_stanza(Alice),
    %% error_logger:info_msg("~p~n", [S]),
    IsMUC = fun(Item) ->
        exml_query:attr(Item, <<"jid">>) == ?MUC_HOST
    end,
    lists:any(IsMUC, Services).
