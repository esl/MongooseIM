%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
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

-module(muc_light_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-import(escalus_ejabberd, [rpc/3]).

-define(ROOM, <<"testroom">>).
-define(MUCHOST, <<"muc.localhost">>).

-define(NS_MUC_LIGHT, <<"mongooseim:muc:light">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, muc_light_ops}
    ].

groups() ->
    [{muc_light_ops, [sequence], [
                             create_room,
                             set_get_config,
                             get_affiliations,
                             invite_remove_others,
                             leave_room,
                             leave_new_owner,
                             change_subject
                            ]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(create_room, Config) ->
    escalus:init_per_testcase(create_room, Config);
init_per_testcase(leave_new_owner, Config) ->
    create_room(?ROOM, ?MUCHOST, alice, Config),
    add_occupant(?ROOM, ?MUCHOST, bob, Config),
    add_occupant(?ROOM, ?MUCHOST, kate, Config),
    escalus:init_per_testcase(leave_new_owner, Config);
init_per_testcase(CaseName, Config) ->
    create_room(?ROOM, ?MUCHOST, alice, Config),
    add_occupant(?ROOM, ?MUCHOST, bob, Config),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    destroy_room(?ROOM, ?MUCHOST),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% MUC light tests
%%--------------------------------------------------------------------

create_room(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
            CreateRequest = room_config_set([]),
            escalus:send(Alice, CreateRequest),
            RoomCreated = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, [CreateRequest], RoomCreated),

            no_stanzas([Alice])
        end).

set_get_config(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
            %% Both owner and member can get configuration
            ConfigGet = room_config_get(),
            escalus:send(Alice, ConfigGet),
            escalus:send(Bob, ConfigGet),
            escalus:assert(is_iq_result, [ConfigGet],
                           escalus:wait_for_stanza(Alice)),
            escalus:assert(is_iq_result, [ConfigGet],
                           escalus:wait_for_stanza(Bob)),

            %% Only owner can set configuration
            ConfigRequest = room_config_set([{<<"roomname">>, <<"newname">>}]),
            escalus:send(Alice, ConfigRequest),
            escalus:assert(is_iq_result, [ConfigRequest],
                           escalus:wait_for_stanza(Alice)),
            escalus:send(Bob, ConfigRequest),
            escalus:assert(is_iq_error, escalus:wait_for_stanza(Bob)),
            
            no_stanzas([Alice, Bob])
        end).

get_affiliations(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
            %% Both owner and member can get affiliation list
            ConfigGet = affiliations_get([<<"member">>, <<"owner">>]),
            escalus:send(Alice, ConfigGet),
            escalus:send(Bob, ConfigGet),
            escalus:assert(is_iq_result, [ConfigGet],
                           escalus:wait_for_stanza(Alice)),
            escalus:assert(is_iq_result, [ConfigGet],
                           escalus:wait_for_stanza(Bob)),

            %% User can get only the current owner
            OwnerGet = affiliations_get([<<"owner">>]),
            escalus:send(Bob, OwnerGet),
            escalus:assert(is_iq_result, [OwnerGet],
                           escalus:wait_for_stanza(Bob)),

            %% User can get only members
            MembersGet = affiliations_get([<<"member">>]),
            escalus:send(Bob, MembersGet),
            escalus:assert(is_iq_result, [MembersGet],
                           escalus:wait_for_stanza(Bob)),

            no_stanzas([Alice, Bob])
        end).

invite_remove_others(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            KateJID = escalus_users:get_jid(Config, kate),
            AddKate = [{KateJID, <<"member">>}],
            AddKateStanza = affiliations_set(AddKate),
            RemoveKate = [{KateJID, <<"none">>}],
            RemoveKateStanza = affiliations_set(RemoveKate),
            
            % Owner can invite
            verify_msg_bcast([Alice, Bob]),
            escalus:send(Alice, AddKateStanza),
            verify_aff_bcast([Alice, Bob, Kate], AddKate),
            escalus:assert(is_iq_result, [AddKateStanza],
                           escalus:wait_for_stanza(Alice)),
            verify_msg_bcast([Alice, Bob, Kate]),
           
            % Non-owner can't remove
            escalus:send(Bob, RemoveKateStanza),
            escalus:assert(is_iq_error, escalus:wait_for_stanza(Bob)),
            verify_msg_bcast([Alice, Bob, Kate]),

            % Owner can remove
            escalus:send(Alice, RemoveKateStanza),
            verify_aff_bcast([Alice, Bob, Kate], RemoveKate),
            escalus:assert(is_iq_result, [RemoveKateStanza],
                           escalus:wait_for_stanza(Alice)),
            verify_msg_bcast([Alice, Bob]),

            % Non-owner cannot invite
            escalus:send(Bob, AddKateStanza),
            escalus:assert(is_iq_error, escalus:wait_for_stanza(Bob)),
            verify_msg_bcast([Alice, Bob]),

            
            no_stanzas([Alice, Bob, Kate])
        end).

leave_room(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
            % Bob is already a member, so he will leave...
            BobJID = escalus_users:get_jid(Config, bob),
            BobLeave = [{BobJID, <<"none">>}],
            BobLeaveStanza = affiliations_set(BobLeave),
            escalus:send(Bob, BobLeaveStanza),
            verify_aff_bcast([Alice, Bob], BobLeave),
            escalus:assert(is_iq_result, [BobLeaveStanza],
                           escalus:wait_for_stanza(Bob)),
            verify_msg_bcast([Alice]),

            no_stanzas([Alice, Bob])
        end).

leave_new_owner(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                  fun(Alice, Bob, Kate) ->
            AliceJID = escalus_users:get_jid(Config, alice),
            BobJID = escalus_users:get_jid(Config, bob),
            KateJID = escalus_users:get_jid(Config, kate),

            % Alice leaves and selects Kate as new owner
            AliceLeave = [{AliceJID, <<"none">>}, {KateJID, <<"owner">>}],
            AliceLeaveStanza = affiliations_set(AliceLeave),
            escalus:send(Alice, AliceLeaveStanza),
            verify_aff_bcast([Bob, Kate], AliceLeave),
            verify_aff_bcast([Alice], [{AliceJID, <<"none">>}]),
            escalus:assert(is_iq_result, [AliceLeaveStanza],
                           escalus:wait_for_stanza(Alice)),

            % Now Kate leaves and room should automatically
            % pick Bob for new owner
            KateLeave = [{KateJID, <<"none">>}],
            KateLeaveStanza = affiliations_set(KateLeave),
            escalus:send(Kate, KateLeaveStanza),
            verify_aff_bcast([Bob], [{BobJID, <<"owner">>} | KateLeave]),
            verify_aff_bcast([Kate], KateLeave),
            escalus:assert(is_iq_result, [KateLeaveStanza],
                           escalus:wait_for_stanza(Kate)),

            % Now last owner leaves, room should be destroyed
            BobLeave = [{BobJID, <<"none">>}],
            BobLeaveStanza = affiliations_set(BobLeave),
            escalus:send(Bob, BobLeaveStanza),
            verify_aff_bcast([Bob], BobLeave),
            escalus:assert(is_iq_result, [BobLeaveStanza],
                           escalus:wait_for_stanza(Bob)),
            false = room_exists(?ROOM, ?MUCHOST)
        end).

change_subject(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
            % Non-owner will change subject and it will become a part
            % of room configuration
            escalus:send(Bob, set_subject(<<"Cookies!">>)),
            escalus:assert(is_groupchat_message, escalus:wait_for_stanza(Alice)),
            escalus:assert(is_groupchat_message, escalus:wait_for_stanza(Bob)),

            escalus:send(Alice, room_config_get()),
            RoomConfig = escalus:wait_for_stanza(Alice),
            <<"Cookies!">> = get_room_config(<<"roomname">>, RoomConfig)
        end).

%%--------------------------------------------------------------------
%% Room configuration manipulation
%%--------------------------------------------------------------------

room_config_get() ->
    escalus_stanza:to(escalus_stanza:iq_get(?NS_MUC_OWNER, []),
                     room_bin_jid()).

room_config_set(Options) ->
    Fields = escalus_stanza:search_fields(
               [ {<<"FORM_TYPE">>, ?NS_MUC_CONFIG} | Options ]),
    Form = escalus_stanza:x_data_form(<<"submit">>, Fields),
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_OWNER, Form),
                      room_bin_jid()).

get_room_config(Name, ConfigIQ) ->
    Path = [{element, <<"query">>},{element, <<"x">>},{element, <<"field">>}],
    find_config_field(Name, exml_query:paths(ConfigIQ, Path)).

set_subject(Subject) ->
    SubjectElement = #xmlel{ name = <<"subject">>,
                             children = [#xmlcdata{ content = Subject }] },
    SubjectStanza = #xmlel{ name = <<"message">>,
                            attrs = [{<<"type">>, <<"groupchat">>}],
                            children = [SubjectElement] },
    escalus_stanza:to(SubjectStanza, room_bin_jid()).

find_config_field(_Name, []) ->
    undefined;
find_config_field(Name, [Field | RFields]) ->
    case exml_query:path(Field, [{attr, <<"var">>}]) of
        Name -> exml_query:path(Field, [{element, <<"value">>}, cdata]);
        _ -> find_config_field(Name, RFields)
    end.

%%--------------------------------------------------------------------
%% Affiliations manipulation
%%--------------------------------------------------------------------

affiliations_get(Affiliations) ->
    Items = [ #xmlel{ name = <<"item">>, 
                      attrs = [{<<"affiliation">>, Affiliation}] }
              || Affiliation <- Affiliations ],
    escalus_stanza:to(escalus_stanza:iq_get(?NS_MUC_ADMIN, Items),
                     room_bin_jid()).

affiliations_set(Affiliations) ->
    Items = [ #xmlel{ name = <<"item">>, 
                      attrs = [{<<"affiliation">>, Affiliation},
                               {<<"jid">>, JID}] }
              || {JID, Affiliation} <- Affiliations ],
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_ADMIN, Items),
                     room_bin_jid()).

%%--------------------------------------------------------------------
%% Broadcast verifiers
%%--------------------------------------------------------------------

verify_msg_bcast(Users) ->
    Msg = escalus_stanza:groupchat_to(room_bin_jid(), <<"Heyah">>),
    lists:foreach(
      fun(Sender) ->
              escalus:send(Sender, Msg),
              lists:foreach(
                fun(Recipient) ->
                        escalus:assert(is_groupchat_message, [<<"Heyah">>],
                                       escalus:wait_for_stanza(Recipient))
                end, Users)
      end, Users).

verify_aff_bcast(Users, Affiliations) ->
    lists:foreach(
      fun(Recipient) ->
              Stanza = escalus:wait_for_stanza(Recipient),
              XEl = exml_query:path(Stanza, [{element, <<"x">>}]),
              ?NS_MUC_LIGHT = exml_query:path(XEl, [{attr, <<"xmlns">>}]),
              Items = exml_query:paths(XEl, [{element, <<"item">>}]),
              true = (length(Items) == length(Affiliations)),
              [] = lists:foldl(
                     fun(Item, AffAcc) ->
                             JID = exml_query:path(
                                     Item, [{attr, <<"jid">>}]),
                             Aff = exml_query:path(
                                     Item, [{attr, <<"affiliation">>}]),
                             true = lists:member({JID, Aff}, AffAcc),
                             lists:delete({JID, Aff}, AffAcc)
                     end, Affiliations, Items)
      end, Users).

%%--------------------------------------------------------------------
%% Other helpers
%%--------------------------------------------------------------------

create_room(RoomU, MUCHost, Owner, Config) ->
    DefaultConfig = rpc(mod_muc_light, default_configuration, []),
    RoomJID = make_jid(RoomU, MUCHost),
    OwnerJID = lower(user_jid(Owner, Config)),
    ok = rpc(backend(), create_room, [RoomJID, OwnerJID, DefaultConfig]).

destroy_room(RoomU, MUCHost) ->
    RoomJID = make_jid(RoomU, MUCHost),
    ok = rpc(backend(), force_destroy_room, [RoomJID]).

room_exists(RoomU, MUCHost) ->
    RoomJID = make_jid(RoomU, MUCHost),
    rpc(backend(), room_exists, [RoomJID]).

user_jid(User, Config) ->
    UserU = escalus_users:get_username(Config, User),
    UserS = escalus_users:get_server(Config, User),
    make_jid(UserU, UserS).

room_bin_jid() ->
    <<(?ROOM)/binary, $@, (?MUCHOST)/binary>>.

add_occupant(RoomU, MUCHost, User, Config) ->
    UserLJID = lower(user_jid(User, Config)),
    RoomJID = make_jid(RoomU, MUCHost),
    NewAff = [{UserLJID, member}],
    {ok, _, _} = rpc(backend(), modify_affiliated_users, [RoomJID, NewAff]).

make_jid(User, Server) ->
    escalus_ejabberd:rpc(jlib, make_jid, [User, Server, <<>>]).

lower(JID) ->
    escalus_ejabberd:rpc(jlib, jid_to_lower, [JID]).

backend() ->
    rpc(mod_muc_light, backend, []).

no_stanzas(Users) ->
    lists:foreach(
      fun(User) ->
              false = escalus_client:has_stanzas(User)
      end, Users).
