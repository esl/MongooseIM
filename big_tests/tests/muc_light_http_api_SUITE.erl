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
%% Description: Test HTTP Administration API for MUC Light
%%==============================================================================

-module(muc_light_http_api_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-import(muc_light_helper, [stanza_create_room/3]).
-import(distributed_helper, [subhost_pattern/1]).
-import(domain_helper, [host_type/0, domain/0]).
-import(config_parser_helper, [mod_config/2]).
-import(rest_helper, [putt/3, post/3, delete/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, positive},
     {group, negative}].

groups() ->
    [{positive, [parallel], success_response()},
     {negative, [parallel], negative_response()}].

success_response() ->
    [create_unique_room,
     create_identifiable_room,
     invite_to_room,
     send_message_to_room,
     delete_room
    ].

negative_response() ->
    [create_room_errors,
     create_identifiable_room_errors,
     invite_to_room_errors,
     send_message_errors,
     delete_room_errors].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    dynamic_modules:ensure_modules(host_type(), required_modules()),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob, kate])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, kate])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

required_modules() ->
    [{mod_muc_light,
      mod_config(mod_muc_light, #{rooms_in_rosters => true,
                                  backend => mongoose_helper:mnesia_or_rdbms_backend()})
     }].

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

create_unique_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        MUCLightDomain = muc_light_domain(),
        Path = path([MUCLightDomain]),
        Name = <<"wonderland">>,
        Body = #{ name => Name,
                  owner => escalus_client:short_jid(Alice),
                  subject => <<"Lewis Carol">>
                },
        {{<<"201">>, _}, _} = rest_helper:post(admin, Path, Body),
        [Item] = get_disco_rooms(Alice),
        true = is_room_name(Name, Item),
        true = is_room_domain(MUCLightDomain, Item)
    end).

create_identifiable_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        MUCLightDomain = muc_light_domain(),
        Path = path([MUCLightDomain]),
        RandBits = binary:encode_hex(crypto:strong_rand_bytes(5), lowercase),
        Name = <<"wonderland">>,
        RoomID = <<"just_some_id_", RandBits/binary>>,
        RoomIDescaped = escalus_utils:jid_to_lower(RoomID),
        Body = #{ id => RoomID,
                  name => Name,
                  owner => escalus_client:short_jid(Alice),
                  subject => <<"Lewis Carol">>
                },
        {{<<"201">>, _}, RoomJID} = rest_helper:putt(admin, Path, Body),
        [Item] = get_disco_rooms(Alice),
        [RoomIDescaped, MUCLightDomain] = binary:split(RoomJID, <<"@">>),
        true = is_room_name(Name, Item),
        true = is_room_domain(MUCLightDomain, Item),
        true = is_room_id(RoomIDescaped, Item)
    end).

invite_to_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
      fun(Alice, Bob, Kate) ->
        RoomID = atom_to_binary(?FUNCTION_NAME),
        Path = path([muc_light_domain(), RoomID, "participants"]),
        %% XMPP: Alice creates a room.
        Stt = stanza_create_room(RoomID,
            [{<<"roomname">>, <<"wonderland">>}], [{Kate, member}]),
        escalus:send(Alice, Stt),
        %% XMPP: Alice recieves a affiliation message to herself and
        %% an IQ result when creating the MUC Light room.
        escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
        %% (*) HTTP: Invite Bob (change room affiliation) on Alice's behalf.
        Body = #{ sender => escalus_client:short_jid(Alice),
                  recipient => escalus_client:short_jid(Bob)
                },
        {{<<"204">>, _}, <<"">>} = rest_helper:post(admin, Path, Body),
        %% XMPP: Bob recieves his affiliation information.
        member_is_affiliated(escalus:wait_for_stanza(Bob), Bob),
        %% XMPP: Alice recieves Bob's affiliation infromation.
        member_is_affiliated(escalus:wait_for_stanza(Alice), Bob),
        %% XMPP: Alice does NOT recieve an IQ result stanza following
        %% her HTTP request to invite Bob in story point (*).
        escalus_assert:has_no_stanzas(Alice)
      end).

send_message_to_room(Config) ->
    RoomID = atom_to_binary(?FUNCTION_NAME),
    Path = path([muc_light_domain(), RoomID, "messages"]),
    Text = <<"Hello everyone!">>,
    escalus:fresh_story(Config,
      [{alice, 1}, {bob, 1}, {kate, 1}],
      fun(Alice, Bob, Kate) ->
        %% XMPP: Alice creates a room.
        escalus:send(Alice, stanza_create_room(RoomID,
            [{<<"roomname">>, <<"wonderland">>}], [{Bob, member}, {Kate, member}])),
        %% XMPP: Alice gets her own affiliation info
        escalus:wait_for_stanza(Alice),
        %% XMPP: And Alice gets IQ result
        CreationResult = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, CreationResult),
        %% XMPP: Get Bob and Kate recieve their affiliation information.
        [ escalus:wait_for_stanza(U) || U <- [Bob, Kate] ],
        %% HTTP: Alice sends a message to the MUC room.
        Body = #{ from => escalus_client:short_jid(Alice),
                  body => Text
                },
        {{<<"204">>, _}, <<"">>} = rest_helper:post(admin, Path, Body),
        %% XMPP: Both Bob and Kate see the message.
        [ see_message_from_user(U, Alice, Text) || U <- [Bob, Kate] ]
    end).

delete_room(Config) ->
    RoomID = atom_to_binary(?FUNCTION_NAME),
    RoomName = <<"wonderland">>,
    escalus:fresh_story(Config,
                        [{alice, 1}, {bob, 1}, {kate, 1}],
                        fun(Alice, Bob, Kate)->
                                {{<<"204">>, <<"No Content">>}, <<"">>} =
                                    check_delete_room(Config, RoomName, RoomID, RoomID,
                                                      Alice, [Bob, Kate])
                        end).

create_room_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    Path = path([muc_light_domain()]),
    Body = #{name => <<"Name">>, owner => AliceJid, subject => <<"Lewis Carol">>},
    {{<<"400">>, _}, <<"Missing room name">>} =
        post(admin, Path, maps:remove(name, Body)),
    {{<<"400">>, _}, <<"Missing owner JID">>} =
        post(admin, Path, maps:remove(owner, Body)),
    {{<<"400">>, _}, <<"Missing room subject">>} =
        post(admin, Path, maps:remove(subject, Body)),
    {{<<"400">>, _}, <<"Invalid owner JID">>} =
        post(admin, Path, Body#{owner := <<"@invalid">>}),
    {{<<"400">>, _}, <<"Given user does not exist">>} =
        post(admin, Path, Body#{owner := <<"baduser@", (domain())/binary>>}),
    {{<<"404">>, _}, <<"MUC Light server not found">>} =
        post(admin, path([domain_helper:domain()]), Body).

create_identifiable_room_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    Path = path([muc_light_domain()]),
    Body = #{id => <<"ID">>, name => <<"NameA">>, owner => AliceJid, subject => <<"Lewis Carol">>},
    {{<<"201">>, _}, _RoomJID} = putt(admin, Path, Body#{id => <<"ID1">>}),
    % Fails to create a room with the same ID
    {{<<"400">>, _}, <<"Missing room ID">>} =
        putt(admin, Path, maps:remove(id, Body)),
    {{<<"400">>, _}, <<"Missing room name">>} =
        putt(admin, Path, maps:remove(name, Body)),
    {{<<"400">>, _}, <<"Missing owner JID">>} =
        putt(admin, Path, maps:remove(owner, Body)),
    {{<<"400">>, _}, <<"Missing room subject">>} =
        putt(admin, Path, maps:remove(subject, Body)),
    {{<<"400">>, _}, <<"Invalid owner JID">>} =
        putt(admin, Path, Body#{owner := <<"@invalid">>}),
    {{<<"400">>, _}, <<"Given user does not exist">>} =
        post(admin, Path, Body#{owner := <<"baduser@", (domain())/binary>>}),
    {{<<"403">>, _}, <<"Room already exists">>} =
        putt(admin, Path, Body#{id := <<"ID1">>, name := <<"NameB">>}),
    {{<<"404">>, _}, <<"MUC Light server not found">>} =
        putt(admin, path([domain_helper:domain()]), Body).

invite_to_room_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    BobJid = escalus_users:get_jid(Config1, bob),
    Name = jid:nodeprep(<<(escalus_users:get_username(Config1, alice))/binary, "-room">>),
    muc_light_helper:create_room(Name, muc_light_domain(), alice, [], Config1, <<"v1">>),
    Path = path([muc_light_domain(), Name, "participants"]),
    Body = #{sender => AliceJid, recipient => BobJid},
    {{<<"400">>, _}, <<"Missing recipient JID">>} =
        rest_helper:post(admin, Path, maps:remove(recipient, Body)),
    {{<<"400">>, _}, <<"Missing sender JID">>} =
        rest_helper:post(admin, Path, maps:remove(sender, Body)),
    {{<<"400">>, _}, <<"Invalid recipient JID">>} =
        rest_helper:post(admin, Path, Body#{recipient := <<"@invalid">>}),
    {{<<"400">>, _}, <<"Invalid sender JID">>} =
        rest_helper:post(admin, Path, Body#{sender := <<"@invalid">>}),
    {{<<"400">>, _}, <<"Given user does not exist">>} =
        rest_helper:post(admin, Path, Body#{sender := <<"baduser@", (domain())/binary>>}),
    {{<<"403">>, _}, <<"Given user does not occupy this room">>} =
        rest_helper:post(admin, Path, Body#{sender := BobJid, recipient := AliceJid}),
    {{<<"404">>, _}, <<"Room not found">>} =
        rest_helper:post(admin, path([muc_light_domain(), "badroom", "participants"]), Body),
    {{<<"404">>, _}, <<"MUC Light server not found">>} =
        rest_helper:post(admin, path([domain(), Name, "participants"]), Body).

send_message_errors(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    BobJid = escalus_users:get_jid(Config1, bob),
    Name = jid:nodeprep(<<(escalus_users:get_username(Config1, alice))/binary, "-room">>),
    muc_light_helper:create_room(Name, muc_light_domain(), alice, [], Config1, <<"v1">>),
    Path = path([muc_light_domain(), Name, "messages"]),
    Body = #{from => AliceJid, body => <<"hello">>},
    {{<<"204">>, _}, <<>>} =
        rest_helper:post(admin, Path, Body),
    {{<<"400">>, _}, <<"Missing message body">>} =
        rest_helper:post(admin, Path, maps:remove(body, Body)),
    {{<<"400">>, _}, <<"Missing sender JID">>} =
        rest_helper:post(admin, Path, maps:remove(from, Body)),
    {{<<"400">>, _}, <<"Invalid sender JID">>} =
        rest_helper:post(admin, Path, Body#{from := <<"@invalid">>}),
    {{<<"400">>, _}, <<"Given user does not exist">>} =
        rest_helper:post(admin, Path, Body#{from := <<"baduser@", (domain())/binary>>}),
    {{<<"403">>, _}, <<"Given user does not occupy this room">>} =
        rest_helper:post(admin, Path, Body#{from := BobJid}),
    {{<<"404">>, _}, <<"Room not found">>} =
        rest_helper:post(admin, path([muc_light_domain(), "badroom", "messages"]), Body),
    {{<<"404">>, _}, <<"MUC Light server not found">>} =
        rest_helper:post(admin, path([domain(), Name, "messages"]), Body).

delete_room_errors(_Config) ->
    {{<<"400">>, _}, <<"Invalid room ID or domain name">>} =
        delete(admin, path([muc_light_domain(), "@badroom", "management"])),
    {{<<"404">>, _}, _} =
        delete(admin, path([muc_light_domain()])),
    {{<<"404">>, _}, _} =
        delete(admin, path([muc_light_domain(), "badroom"])),
    {{<<"404">>, _}, <<"Room not found">>} =
        delete(admin, path([muc_light_domain(), "badroom", "management"])),
    {{<<"404">>, _}, <<"MUC Light server not found">>} =
        delete(admin, path([domain(), "badroom", "management"])),
    {{<<"404">>, _}, <<"MUC Light server not found">>} =
        delete(admin, path(["baddomain", "badroom", "management"])).

%%--------------------------------------------------------------------
%% Ancillary (borrowed and adapted from the MUC and MUC Light suites)
%%--------------------------------------------------------------------

get_disco_rooms(User) ->
    DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), muc_light_domain()),
    escalus:send(User, DiscoStanza),
    Stanza =  escalus:wait_for_stanza(User),
    XNamespaces = exml_query:paths(Stanza, [{element, <<"query">>}, {attr, <<"xmlns">>}]),
    true = lists:member(?NS_DISCO_ITEMS, XNamespaces),
    escalus:assert(is_stanza_from, [muc_light_domain()], Stanza),
    exml_query:paths(Stanza, [{element, <<"query">>}, {element, <<"item">>}]).

is_room_name(Name, Item) ->
    Name == exml_query:attr(Item, <<"name">>).

is_room_domain(Domain, Item) ->
    JID = exml_query:attr(Item, <<"jid">>),
    [_, Got] = binary:split(JID, <<$@>>, [global]),
    Domain == Got.

is_room_id(Id, Item) ->
    JID = exml_query:attr(Item, <<"jid">>),
    [Got, _] = binary:split(JID, <<$@>>, [global]),
    Id == Got.

see_message_from_user(User, Sender, Contents) ->
    Stanza = escalus:wait_for_stanza(User),
    #xmlel{ name = <<"message">> } = Stanza,
    SenderJID = escalus_utils:jid_to_lower(escalus_utils:get_short_jid(Sender)),
    From = exml_query:path(Stanza, [{attr, <<"from">>}]),
    {_, _} = binary:match(From, SenderJID),
    Contents = exml_query:path(Stanza, [{element, <<"body">>}, cdata]).

member_is_affiliated(Stanza, User) ->
    MemberJID = escalus_utils:jid_to_lower(escalus_utils:get_short_jid(User)),
    Data = exml_query:path(Stanza, [{element, <<"x">>}, {element, <<"user">>}, cdata]),
    MemberJID == Data.

check_delete_room(_Config, RoomName, RoomIDToCreate, RoomIDToDelete, RoomOwner, RoomMembers) ->
    Members = [{Member, member} || Member <- RoomMembers],
    escalus:send(RoomOwner, stanza_create_room(RoomIDToCreate,
                                           [{<<"roomname">>, RoomName}],
                                           Members)),
    %% XMPP RoomOwner gets affiliation and IQ result
    Affiliations = [{RoomOwner, owner} | Members],
    muc_light_helper:verify_aff_bcast([{RoomOwner, owner}], Affiliations),
    %% and now RoomOwner gets IQ result
    CreationResult = escalus:wait_for_stanza(RoomOwner),
    escalus:assert(is_iq_result, CreationResult),
    muc_light_helper:verify_aff_bcast(Members, Affiliations),
    Path = path([muc_light_domain(), RoomIDToDelete, "management"]),
    rest_helper:delete(admin, Path).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

path(Items) ->
    AllItems = ["muc-lights" | Items],
    iolist_to_binary([[$/, Item] || Item <- AllItems]).

muc_light_domain() ->
    muc_light_helper:muc_host().
