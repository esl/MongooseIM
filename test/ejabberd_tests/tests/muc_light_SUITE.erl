-module(muc_light_SUITE).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-export([ % service
         mismatched_default_config_is_rejected/1,
         removing_users_from_server_triggers_room_destruction/1
        ]).
-export([ % entity
         disco_service/1,
         disco_features/1,
         disco_rooms/1,
         disco_rooms_empty_page_1/1,
         disco_rooms_empty_page_infinity/1,
         disco_rooms_created_page_1/1,
         disco_rooms_created_page_infinity/1,
         disco_rooms_rsm/1,
         rooms_in_rosters/1,
         no_roomname_in_schema_doesnt_break_disco_and_roster/1,
         unauthorized_stanza/1
    ]).
-export([ % occupant
         send_message/1,
         change_subject/1,
         change_roomname/1,
         all_can_configure/1,
         set_config_deny/1,
         get_room_config/1,
         custom_schema_works_with_standard_default_config/1,
         custom_default_config_works/1,
         get_room_occupants/1,
         get_room_info/1,
         leave_room/1,
         change_other_aff_deny/1
        ]).
-export([ % owner
         create_room/1,
         create_room_unique/1,
         create_room_with_equal_occupants/1,
         create_existing_room_deny/1,
         destroy_room/1,
         destroy_room_get_disco_items_empty/1,
         destroy_room_get_disco_items_one_left/1,
         set_config/1,
         set_config_with_custom_schema/1,
         deny_config_change_that_conflicts_with_schema/1,
         assorted_config_doesnt_lead_to_duplication/1,
         remove_and_add_users/1,
         explicit_owner_change/1,
         implicit_owner_change/1,
         edge_case_owner_change/1
        ]).
-export([ % limits
         rooms_per_user/1,
         max_occupants/1
        ]).
-export([ % blocking
         manage_blocklist/1,
         block_room/1,
         block_user/1,
         blocking_disabled/1
        ]).
-export([ % for mam_SUITE
         ns_muc_light_affiliations/0,
         stanza_create_room/3,
         room2/0,
         clear_db/0,
         verify_aff_bcast/2,
         room_bin_jid/1,
         gc_message_verify_fun/3,
         stanza_aff_set/2,
         bin_aff_users/1,
         verify_aff_users/2
        ]).

-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-import(escalus_ejabberd, [rpc/3]).
-import(muc_helper, [foreach_occupant/3, foreach_recipient/2]).

-define(ROOM, <<"testroom">>).
-define(ROOM2, <<"testroom2">>).

-define(MUCHOST, <<"muclight.localhost">>).

-define(NS_MUC_LIGHT, <<"urn:xmpp:muclight:0">>).
-define(NS_MUC_LIGHT_CONFIGURATION, <<"urn:xmpp:muclight:0#configuration">>).
-define(NS_MUC_LIGHT_AFFILIATIONS, <<"urn:xmpp:muclight:0#affiliations">>).
-define(NS_MUC_LIGHT_INFO, <<"urn:xmpp:muclight:0#info">>).
-define(NS_MUC_LIGHT_BLOCKING, <<"urn:xmpp:muclight:0#blocking">>).
-define(NS_MUC_LIGHT_CREATE, <<"urn:xmpp:muclight:0#create">>).
-define(NS_MUC_LIGHT_DESTROY, <<"urn:xmpp:muclight:0#destroy">>).

-define(CHECK_FUN, fun mod_muc_light_room:participant_limit_check/2).
-define(BACKEND, mod_muc_light_db_backend).

-type ct_aff_user() :: {EscalusClient :: escalus:client(), Aff :: atom()}.
-type ct_aff_users() :: [ct_aff_user()].
-type ct_block_item() :: {What :: atom(), Action :: atom(), Who :: binary()}.
-type verify_fun() :: muc_helper:verify_fun().

-define(DEFAULT_AFF_USERS, [{Alice, owner}, {Bob, member}, {Kate, member}]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, service},
     {group, entity},
     {group, occupant},
     {group, owner},
     {group, blocking},
     {group, limits}
    ].

groups() ->
    [
     {service, [sequence], [
                            mismatched_default_config_is_rejected,
                            removing_users_from_server_triggers_room_destruction
                           ]},
     {entity, [sequence], [
                            disco_service,
                            disco_features,
                            disco_rooms,
                            disco_rooms_rsm,
                            disco_rooms_created_page_1,
                            disco_rooms_created_page_infinity,
                            disco_rooms_empty_page_infinity,
                            disco_rooms_empty_page_1,
                            rooms_in_rosters,
                            no_roomname_in_schema_doesnt_break_disco_and_roster,
                            unauthorized_stanza
                         ]},
     {occupant, [sequence], [
                             send_message,
                             change_subject,
                             change_roomname,
                             all_can_configure,
                             set_config_deny,
                             get_room_config,
                             custom_schema_works_with_standard_default_config,
                             custom_default_config_works,
                             get_room_occupants,
                             get_room_info,
                             leave_room,
                             change_other_aff_deny
                            ]},
     {owner, [sequence], [
                          create_room,
                          create_room_unique,
                          create_room_with_equal_occupants,
                          create_existing_room_deny,
                          destroy_room,
                          destroy_room_get_disco_items_empty,
                          destroy_room_get_disco_items_one_left,
                          set_config,
                          set_config_with_custom_schema,
                          deny_config_change_that_conflicts_with_schema,
                          assorted_config_doesnt_lead_to_duplication,
                          remove_and_add_users,
                          explicit_owner_change,
                          implicit_owner_change,
                          edge_case_owner_change
                         ]},
     {limits, [sequence], [
                           rooms_per_user,
                           max_occupants
                          ]},
     {blocking, [sequence], [
                             manage_blocklist,
                             block_room,
                             block_user,
                             blocking_disabled
                            ]}
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    dynamic_modules:start(<<"localhost">>, mod_muc_light,
                          [{host, binary_to_list(?MUCHOST)},
                           {rooms_in_rosters, true}]),
    Config1 = escalus:init_per_suite(Config),
    escalus:create_users(Config1, escalus:get_users([alice, bob, kate, mike, carol])).

end_per_suite(Config) ->
    clear_db(),
    Config1 = escalus:delete_users(Config, escalus:get_users([alice, bob, kate, mike])),
    dynamic_modules:stop(<<"localhost">>, mod_muc_light),
    escalus:end_per_suite(Config1).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-define(ROOM_LESS_CASES, [disco_rooms_empty_page_infinity,
                          disco_rooms_empty_page_1,
                          mismatched_default_config_is_rejected]).

-define(CUSTOM_CONFIG_CASES, [set_config_with_custom_schema,
                              deny_config_change_that_conflicts_with_schema,
                              no_roomname_in_schema_doesnt_break_disco_and_roster,
                              custom_default_config_works]).

init_per_testcase(removing_users_from_server_triggers_room_destruction = CN, Config) ->
    set_default_mod_config(),
    create_room(?ROOM, ?MUCHOST, carol, [], Config, ver(1)),
    escalus:init_per_testcase(CN, Config);
init_per_testcase(disco_rooms_rsm, Config) ->
    set_default_mod_config(),
    set_mod_config(rooms_per_page, 1),
    create_room(?ROOM, ?MUCHOST, alice, [bob, kate], Config, ver(1)),
    create_room(?ROOM2, ?MUCHOST, alice, [bob, kate], Config, ver(1)),
    escalus:init_per_testcase(disco_rooms_rsm, Config);
init_per_testcase(custom_schema_works_with_standard_default_config = CaseName, Config) ->
    set_default_mod_config(),
    set_custom_config(["roomname", "subject", "background", "music"], standard_default_config()),
    create_room(?ROOM, ?MUCHOST, alice, [bob, kate], Config, ver(1)),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CaseName, Config) ->
    set_default_mod_config(),
    case lists:member(CaseName, ?CUSTOM_CONFIG_CASES) of
        true ->
            CustomDefaultConfig = [{atom_to_list(K), binary_to_list(V)}
                                   || {K, V} <- custom_default_config()],
            set_custom_config(["background", "music"], CustomDefaultConfig);
        _ ->
            ok
    end,
    case lists:member(CaseName, ?ROOM_LESS_CASES) of
        false -> create_room(?ROOM, ?MUCHOST, alice, [bob, kate], Config, ver(1));
        _ -> ok
    end,
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    case lists:member(
           CaseName, [custom_schema_works_with_standard_default_config | ?CUSTOM_CONFIG_CASES]) of
        true -> set_custom_config(standard_config_schema(), standard_default_config());
        _ -> ok
    end,
    clear_db(),
    escalus:end_per_testcase(CaseName, Config).

%% ---------------------- Helpers ----------------------

create_room(RoomU, MUCHost, Owner, Members, Config, Version) ->
    DefaultConfig = default_config(),
    RoomUS = {RoomU, MUCHost},
    AffUsers = [{to_lus(Owner, Config), owner}
                | [ {to_lus(Member, Config), member} || Member <- Members ]],
    {ok, _RoomUS} = rpc(?BACKEND, create_room, [RoomUS, DefaultConfig, AffUsers, Version]).

clear_db() ->
    rpc(?BACKEND, force_clear, []).

%%--------------------------------------------------------------------
%% MUC light tests
%%--------------------------------------------------------------------

%% ---------------------- Service ----------------------

mismatched_default_config_is_rejected(_Config) ->
    {'EXIT', _} = (catch set_custom_config(["background", "music"], standard_default_config())),
    {'EXIT', _} = (catch set_custom_config(["background", "music"], ["misfit"])),
    ok.

removing_users_from_server_triggers_room_destruction(Config) ->
    escalus:delete_users(Config, escalus:get_users([carol])),
    {error, not_exists} = rpc(mod_muc_light_db_backend, get_info, [{?ROOM, ?MUCHOST}]).

%% ---------------------- Disco ----------------------

disco_service(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            Server = escalus_client:server(Alice),
            escalus:send(Alice, escalus_stanza:service_discovery(Server)),
            Stanza = escalus:wait_for_stanza(Alice),
            escalus:assert(has_service, [?MUCHOST], Stanza),
            escalus:assert(is_stanza_from, [escalus_config:get_config(ejabberd_domain, Config)], Stanza)
        end).

disco_features(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_INFO, []), ?MUCHOST),
            escalus:send(Alice, DiscoStanza),
            Stanza = escalus:wait_for_stanza(Alice),
            <<"conference">> = exml_query:path(Stanza, [{element, <<"query">>},
                                                        {element, <<"identity">>},
                                                        {attr, <<"category">>}]),
            ?NS_MUC_LIGHT = exml_query:path(Stanza, [{element, <<"query">>},
                                                     {element, <<"feature">>},
                                                     {attr, <<"var">>}]),
            escalus:assert(is_stanza_from, [?MUCHOST], Stanza)
        end).

%% The room list is empty. Rooms_per_page set to `infinity`
disco_rooms_empty_page_infinity(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        set_mod_config(rooms_per_page, infinity),
        [] = get_disco_rooms(Alice)
        end).

%% The room list is empty. Rooms_per_page set to 1
disco_rooms_empty_page_1(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        set_mod_config(rooms_per_page, 1),
        [] = get_disco_rooms(Alice)
        end).

%% There is one room created. Rooms_per_page set to 1
disco_rooms_created_page_1(Config) ->
    set_mod_config(rooms_per_page, 1),
    escalus:story(Config, [{alice, 1}], fun verify_user_has_one_room/1).

%% There is one room created. Rooms_per_page set to `infinity`
disco_rooms_created_page_infinity(Config) ->
    set_mod_config(rooms_per_page, infinity),
    escalus:story(Config, [{alice, 1}], fun verify_user_has_one_room/1).

disco_rooms(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            {ok, {?ROOM2, ?MUCHOST}} = create_room(?ROOM2, ?MUCHOST, kate, [], Config, ver(0)),
            %% we should get 1 room, Alice is not in the second one
            [Item] = get_disco_rooms(Alice),
            ProperJID = room_bin_jid(?ROOM),
            ProperJID = exml_query:attr(Item, <<"jid">>),
            ProperVer = ver(1),
            ProperVer = exml_query:attr(Item, <<"version">>)
        end).

disco_rooms_rsm(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), ?MUCHOST),
            escalus:send(Alice, DiscoStanza),
            %% we should get 1 room with RSM info
            Stanza = escalus:wait_for_stanza(Alice),
            [Item] = exml_query:paths(Stanza, [{element, <<"query">>}, {element, <<"item">>}]),
            ProperJID = room_bin_jid(?ROOM),
            ProperJID = exml_query:attr(Item, <<"jid">>),

            RSM = #xmlel{ name = <<"set">>,
                          attrs = [{<<"xmlns">>, ?NS_RSM}],
                          children = [ #xmlel{ name = <<"max">>,
                                               children = [#xmlcdata{ content = <<"10">> }] },
                                       #xmlel{ name = <<"before">> } ]  },
            DiscoStanza2 = escalus_stanza:to(
                             escalus_stanza:iq_get(?NS_DISCO_ITEMS, [RSM]), ?MUCHOST),
            escalus:send(Alice, DiscoStanza2),
            %% we should get second room
            Stanza2 = escalus:wait_for_stanza(Alice),
            [Item2] = exml_query:paths(Stanza2, [{element, <<"query">>}, {element, <<"item">>}]),
            ProperJID2 = room_bin_jid(?ROOM2),
            ProperJID2 = exml_query:attr(Item2, <<"jid">>),

            BadAfter = #xmlel{ name = <<"after">>,
                               children = [#xmlcdata{ content = <<"oops@muclight.localhost">> }] },
            RSM2 = #xmlel{ name = <<"set">>,
                          attrs = [{<<"xmlns">>, ?NS_RSM}],
                          children = [ #xmlel{ name = <<"max">>,
                                               children = [#xmlcdata{ content = <<"10">> }] },
                                       BadAfter ]  },
            DiscoStanza3 = escalus_stanza:to(
                             escalus_stanza:iq_get(?NS_DISCO_ITEMS, [RSM2]), ?MUCHOST),
            escalus:send(Alice, DiscoStanza3),
            escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>],
                           escalus:wait_for_stanza(Alice))
        end).

rooms_in_rosters(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            escalus:send(Alice, escalus_stanza:roster_get()),
            RosterResult = escalus:wait_for_stanza(Alice),
            escalus_assert:is_roster_result(RosterResult),

            [Item] = exml_query:paths(
                       RosterResult, [{element, <<"query">>}, {element, <<"item">>}]),
            ProperJID = room_bin_jid(?ROOM),
            ProperJID = exml_query:attr(Item, <<"jid">>),
            ProperName = proplists:get_value(roomname, default_config()),
            ProperName = exml_query:attr(Item, <<"name">>),
            ProperVer = ver(1),
            ProperVer = exml_query:path(Item, [{element, <<"version">>}, cdata])
        end).

no_roomname_in_schema_doesnt_break_disco_and_roster(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            [DiscoItem] = get_disco_rooms(Alice),
            ?ROOM = exml_query:attr(DiscoItem, <<"name">>),

            escalus:send(Alice, escalus_stanza:roster_get()),
            RosterResult = escalus:wait_for_stanza(Alice),
            [RosterItem] = exml_query:paths(
                       RosterResult, [{element, <<"query">>}, {element, <<"item">>}]),
            ?ROOM = exml_query:attr(RosterItem, <<"name">>)
        end).

unauthorized_stanza(Config) ->
    escalus:story(Config, [{alice, 1}, {kate, 1}], fun(Alice, Kate) ->
            {ok, {?ROOM2, ?MUCHOST}} = create_room(?ROOM2, ?MUCHOST, kate, [], Config, ver(0)),
            MsgStanza = escalus_stanza:groupchat_to(room_bin_jid(?ROOM2), <<"malicious">>),
            escalus:send(Alice, MsgStanza),
            escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>],
                           escalus:wait_for_stanza(Alice)),
            verify_no_stanzas([Alice, Kate])
        end).

%% ---------------------- Occupant ----------------------

send_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            Msg = <<"Heyah!">>,
            Id = <<"MyID">>,
            Stanza = escalus_stanza:set_id(
                       escalus_stanza:groupchat_to(room_bin_jid(?ROOM), Msg), Id),
            foreach_occupant([Alice, Bob, Kate], Stanza, gc_message_verify_fun(?ROOM, Msg, Id))
        end).

change_subject(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"subject">>, <<"new subject">>}],
            Stanza = stanza_config_set(?ROOM, ConfigChange),
            foreach_occupant([Alice, Bob, Kate], Stanza, config_msg_verify_fun(ConfigChange))
        end).

change_roomname(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        %% change room name
        ConfigChange = [{<<"roomname">>, <<"new_test_room">>}],
        Stanza = stanza_config_set(?ROOM, ConfigChange),
        escalus:send(Alice, Stanza),
        escalus:wait_for_stanzas(Alice, 2),
        StanzaCheck = stanza_config_get(?ROOM, ver(1)),
        escalus:send(Alice, StanzaCheck),
        Res = escalus:wait_for_stanza(Alice),
        [_] = exml_query:paths(Res, [{element, <<"query">>}, {element, <<"roomname">>}])
        end).

all_can_configure(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            set_mod_config(all_can_configure, true),
            ConfigChange = [{<<"roomname">>, <<"new subject">>}],
            Stanza = stanza_config_set(?ROOM, ConfigChange),
            foreach_occupant([Alice, Bob, Kate], Stanza, config_msg_verify_fun(ConfigChange))
        end).

set_config_deny(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"roomname">>, <<"new subject">>}],
            Stanza = stanza_config_set(?ROOM, ConfigChange),
            escalus:send(Kate, Stanza),
            escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>],
                           escalus:wait_for_stanza(Kate)),
            verify_no_stanzas([Alice, Bob, Kate])
        end).

get_room_config(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            Stanza = stanza_config_get(?ROOM, <<"oldver">>),
            ConfigKV = [{version, ver(1)} | default_config()],
            ConfigKVBin = [{list_to_binary(atom_to_list(Key)), Val} || {Key, Val} <- ConfigKV],
            foreach_occupant([Alice, Bob, Kate], Stanza, config_iq_verify_fun(ConfigKVBin)),

            %% Empty result when user has most recent version
            escalus:send(Bob, stanza_config_get(?ROOM, ver(1))),
            IQRes = escalus:wait_for_stanza(Bob),
            escalus:assert(is_iq_result, IQRes),
            undefined = exml_query:subelement(IQRes, <<"query">>)
        end).

custom_schema_works_with_standard_default_config(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            Stanza = stanza_config_get(?ROOM, <<"oldver">>),
            ConfigKV = [{version, ver(1)} | default_config()],
            ConfigKVBin = [{atom_to_binary(Key, utf8), Val} || {Key, Val} <- ConfigKV],
            foreach_occupant([Alice, Bob, Kate], Stanza, config_iq_verify_fun(ConfigKVBin))
        end).

custom_default_config_works(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            Stanza = stanza_config_get(?ROOM, <<"oldver">>),
            ConfigKV = [{version, ver(1)} | custom_default_config()],
            ConfigKVBin = [{atom_to_binary(Key, utf8), Val} || {Key, Val} <- ConfigKV],
            foreach_occupant([Alice, Bob, Kate], Stanza, config_iq_verify_fun(ConfigKVBin))
        end).


get_room_occupants(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsers = [{Alice, owner}, {Bob, member}, {Kate, member}],
            foreach_occupant([Alice, Bob, Kate], stanza_aff_get(?ROOM, <<"oldver">>),
                             aff_iq_verify_fun(AffUsers, ver(1))),

            escalus:send(Bob, stanza_aff_get(?ROOM, ver(1))),
            IQRes = escalus:wait_for_stanza(Bob),
            escalus:assert(is_iq_result, IQRes),
            undefined = exml_query:subelement(IQRes, <<"query">>)
        end).

get_room_info(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            Stanza = stanza_info_get(?ROOM, <<"oldver">>),
            ConfigKV = default_config(),
            ConfigKVBin = [{list_to_binary(atom_to_list(Key)), Val} || {Key, Val} <- ConfigKV],
            foreach_occupant([Alice, Bob, Kate], Stanza,
                             info_iq_verify_fun(?DEFAULT_AFF_USERS, ver(1), ConfigKVBin)),

            escalus:send(Bob, stanza_aff_get(?ROOM, ver(1))),
            IQRes = escalus:wait_for_stanza(Bob),
            escalus:assert(is_iq_result, IQRes),
            undefined = exml_query:subelement(IQRes, <<"query">>)
        end).

leave_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            % Users will leave one by one, owner last
            lists:foldr(
              fun(User, {Occupants, Outsiders}) ->
                      NewOccupants = lists:keydelete(User, 1, Occupants),
                      user_leave(User, NewOccupants),
                      verify_no_stanzas(Outsiders),
                      {NewOccupants, [User | Outsiders]}
              end, {?DEFAULT_AFF_USERS, []}, [Alice, Bob, Kate]),

            % Now we verify that room is removed from DB
            {error, not_exists} = rpc(mod_muc_light_db_backend, get_info, [{?ROOM, ?MUCHOST}])
        end).

change_other_aff_deny(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}],
                  fun(Alice, Bob, Kate, Mike) ->
            AffUsersChanges1 = [{Bob, none}],
            escalus:send(Kate, stanza_aff_set(?ROOM, AffUsersChanges1)),
            escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>],
                           escalus:wait_for_stanza(Kate)),

            AffUsersChanges2 = [{Alice, member}, {Kate, owner}],
            escalus:send(Kate, stanza_aff_set(?ROOM, AffUsersChanges2)),
            escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>],
                           escalus:wait_for_stanza(Kate)),

            set_mod_config(all_can_invite, false),
            AffUsersChanges3 = [{Mike, member}],
            escalus:send(Kate, stanza_aff_set(?ROOM, AffUsersChanges3)),
            escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>],
                           escalus:wait_for_stanza(Kate)),

            verify_no_stanzas([Alice, Bob, Kate, Mike])
        end).

%% ---------------------- owner ----------------------

create_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            InitOccupants = [{Alice, member},
                             {Kate, member}],
            FinalOccupants = [{Bob, owner} | InitOccupants],
            InitConfig = [{<<"roomname">>, <<"Bob's room">>}],
            RoomNode = <<"bobroom">>,
            escalus:send(Bob, stanza_create_room(RoomNode, InitConfig, InitOccupants)),
            verify_aff_bcast(FinalOccupants, FinalOccupants),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob))
        end).

create_room_unique(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
            InitConfig = [{<<"roomname">>, <<"Bob's room">>}],
            escalus:send(Bob, stanza_create_room(undefined, InitConfig, [])),
            verify_aff_bcast([{Bob, owner}], [{Bob, owner}]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob))
        end).

create_room_with_equal_occupants(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
            set_mod_config(equal_occupants, true),
            InitConfig = [{<<"roomname">>, <<"Bob's room">>}],
            escalus:send(Bob, stanza_create_room(undefined, InitConfig, [])),
            verify_aff_bcast([{Bob, member}], [{Bob, member}]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob))
        end).

create_existing_room_deny(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
            escalus:send(Bob, stanza_create_room(?ROOM, [], [])),
            escalus:assert(is_error, [<<"cancel">>, <<"conflict">>], escalus:wait_for_stanza(Bob))
        end).

destroy_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            escalus:send(Alice, stanza_destroy_room(?ROOM)),
            AffUsersChanges = [{Bob, none}, {Alice, none}, {Kate, none}],
            verify_aff_bcast([], AffUsersChanges, [?NS_MUC_LIGHT_DESTROY]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

destroy_room_get_disco_items_empty(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        escalus:send(Alice, stanza_destroy_room(?ROOM)),
        AffUsersChanges = [{Bob, none}, {Alice, none}, {Kate, none}],
        verify_aff_bcast([], AffUsersChanges, [?NS_MUC_LIGHT_DESTROY]),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
        % Send disco#items request
        DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), ?MUCHOST),
        foreach_occupant([Alice, Bob, Kate], DiscoStanza, disco_items_verify_fun([]))
     end).

destroy_room_get_disco_items_one_left(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        {ok, {?ROOM2, ?MUCHOST}} = create_room(?ROOM2, ?MUCHOST, kate, [bob, alice], Config, ver(0)),
        ProperJID = room_bin_jid(?ROOM2),
        %% alie destroy her room
        escalus:send(Alice, stanza_destroy_room(?ROOM)),
        AffUsersChanges = [{Bob, none}, {Alice, none}, {Kate, none}],
        verify_aff_bcast([], AffUsersChanges, [?NS_MUC_LIGHT_DESTROY]),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
        % Send disco#items request. Shoul be one room created by kate
        DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), ?MUCHOST),
        foreach_occupant([Alice, Bob, Kate], DiscoStanza, disco_items_verify_fun([ProperJID]))
     end).

set_config(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"roomname">>, <<"The Coven">>}],
            escalus:send(Alice, stanza_config_set(?ROOM, ConfigChange)),
            foreach_recipient([Alice, Bob, Kate], config_msg_verify_fun(ConfigChange)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

set_config_with_custom_schema(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"background">>, <<"builtin:unicorns">>},
                            {<<"music">>, <<"builtin:rainbow">>}],
            escalus:send(Alice, stanza_config_set(?ROOM, ConfigChange)),
            foreach_recipient([Alice, Bob, Kate], config_msg_verify_fun(ConfigChange)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

deny_config_change_that_conflicts_with_schema(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            ConfigChange = [{<<"roomname">>, <<"The Coven">>}],
            escalus:send(Alice, stanza_config_set(?ROOM, ConfigChange)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Alice))
        end).

assorted_config_doesnt_lead_to_duplication(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"subject">>, <<"Elixirs">>},
                            {<<"roomname">>, <<"The Coven">>},
                            {<<"subject">>, <<"Elixirs">>}],
            ConfigSetStanza = stanza_config_set(?ROOM, ConfigChange),
            escalus:send(Alice, ConfigSetStanza),
            foreach_recipient([Alice, Bob, Kate], config_msg_verify_fun(ConfigChange)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

            Stanza = stanza_config_get(?ROOM, <<"oldver">>),
            VerifyFun = fun(Incoming) ->
                                [Query] = exml_query:subelements(Incoming, <<"query">>),
                                Length = length(Query#xmlel.children),
                                Length = length(lists:ukeysort(#xmlel.name, Query#xmlel.children))
                        end,
            foreach_occupant([Alice, Bob, Kate], Stanza, VerifyFun)
        end).

remove_and_add_users(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsersChanges1 = [{Bob, none}, {Kate, none}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges1)),
            verify_aff_bcast([{Alice, owner}], AffUsersChanges1),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            AffUsersChanges2 = [{Bob, member}, {Kate, member}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges2)),
            verify_aff_bcast([{Alice, owner}, {Bob, member}, {Kate, member}], AffUsersChanges2),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

explicit_owner_change(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsersChanges1 = [{Bob, none}, {Alice, none}, {Kate, owner}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges1)),
            verify_aff_bcast([{Kate, owner}], AffUsersChanges1),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

implicit_owner_change(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsersChanges1 = [{Bob, none}, {Alice, member}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges1)),
            verify_aff_bcast([{Kate, owner}, {Alice, member}], [{Kate, owner} | AffUsersChanges1]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

edge_case_owner_change(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsersChanges1 = [{Alice, member}, {Bob, none}, {Kate, none}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges1)),
            verify_aff_bcast([{Alice, owner}], [{Kate, none}, {Bob, none}]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

%% ---------------------- limits ----------------------

rooms_per_user(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}],
                  fun(Alice, Bob, Kate, Mike) ->
            set_mod_config(rooms_per_user, 1),
            escalus:send(Bob, stanza_create_room(undefined, [], [])),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            escalus:send(Mike, stanza_create_room(<<"mikeroom">>, [], [])),
            verify_aff_bcast([{Mike, owner}], [{Mike, owner}]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Mike)),
            KateAdd = [{Kate, member}],
            escalus:send(Mike, stanza_aff_set(<<"mikeroom">>, KateAdd)),
            %% Receives result and nothing happens, because Kate's limit is reached
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Mike)),

            verify_no_stanzas([Alice, Bob, Kate, Mike])
        end).

max_occupants(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}],
                  fun(Alice, Bob, Kate, Mike) ->
            set_mod_config(max_occupants, 1),
            escalus:send(Bob, stanza_create_room(undefined, [], [{Alice, member}, {Kate, member}])),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

            MikeAdd = [{Mike, member}],
            escalus:send(Alice, stanza_aff_set(?ROOM, MikeAdd)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Alice)),

            verify_no_stanzas([Alice, Bob, Kate, Mike])
        end).

%% ---------------------- blocking ----------------------

manage_blocklist(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            escalus:send(Alice, stanza_blocking_get()),
            GetResult1 = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, GetResult1),
            QueryEl1 = exml_query:subelement(GetResult1, <<"query">>),
            verify_blocklist(QueryEl1, []),

            BlocklistChange1 = [{user, deny, <<"user@localhost">>},
                                {room, deny, room_bin_jid(?ROOM)}],
            escalus:send(Alice, stanza_blocking_set(BlocklistChange1)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            escalus:send(Alice, stanza_blocking_get()),
            GetResult2 = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, GetResult2),
            QueryEl2 = exml_query:subelement(GetResult2, <<"query">>),
            verify_blocklist(QueryEl2, BlocklistChange1),

            BlocklistChange2 = [{user, allow, <<"user@localhost">>},
                                {room, allow, room_bin_jid(?ROOM)}],
            escalus:send(Alice, stanza_blocking_set(BlocklistChange2)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            escalus:send(Alice, stanza_blocking_get()),
            GetResult3 = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, GetResult3),
            % Match below checks for empty list
            QueryEl1 = exml_query:subelement(GetResult3, <<"query">>)
        end).

block_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            BlocklistChange = [{room, deny, room_bin_jid(?ROOM)}],
            escalus:send(Bob, stanza_blocking_set(BlocklistChange)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
            user_leave(Bob, [{Alice, owner}, {Kate, member}]),

            % Alice tries to readd Bob to the room but fails
            BobReadd = [{Bob, member}],
            FailStanza = stanza_aff_set(?ROOM, BobReadd),
            escalus:send(Alice, FailStanza),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            verify_no_stanzas([Alice, Bob, Kate]),

            % But Alice can add Bob to another room!
            InitOccupants = [{Bob, member}],
            escalus:send(Alice, stanza_create_room(<<"newroom">>, [], InitOccupants)),
            verify_aff_bcast([{Alice, owner}, {Bob, member}],
                             [{Alice, owner} | InitOccupants]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

block_user(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AliceJIDBin = lbin(escalus_client:short_jid(Alice)),
            BlocklistChange = [{user, deny, AliceJIDBin}],
            escalus:send(Bob, stanza_blocking_set(BlocklistChange)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
            user_leave(Bob, [{Alice, owner}, {Kate, member}]),

            % Alice tries to create new room with Bob but Bob is not added
            escalus:send(Alice, stanza_create_room(<<"new">>, [], [{Bob, member}])),
            verify_aff_bcast([{Alice, owner}], [{Alice, owner}]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            verify_no_stanzas([Alice, Bob, Kate]),

            % But Kate can add Bob to the main room!
            set_mod_config(all_can_invite, true),
            BobReadd = [{Bob, member}],
            SuccessStanza = stanza_aff_set(?ROOM, BobReadd),
            escalus:send(Kate, SuccessStanza),
            verify_aff_bcast([{Alice, owner}, {Bob, member}, {Kate, member}], BobReadd),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Kate)),
            verify_no_stanzas([Alice, Bob, Kate])

        end).

blocking_disabled(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            set_mod_config(blocking, false),
            escalus:send(Alice, stanza_blocking_get()),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Alice)),

            BlocklistChange1 = [{user, deny, <<"user@localhost">>},
                                {room, deny, room_bin_jid(?ROOM)}],
            escalus:send(Alice, stanza_blocking_set(BlocklistChange1)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Alice))
        end).

%%--------------------------------------------------------------------
%% Subroutines
%%--------------------------------------------------------------------

-spec user_leave(User :: escalus:client(), RemainingOccupants :: ct_aff_users()) -> ok.
user_leave(User, RemainingOccupants) ->
    AffUsersChanges = [{User, none}],
    Stanza = stanza_aff_set(?ROOM, AffUsersChanges),
    escalus:send(User, Stanza),
    % bcast
    verify_aff_bcast(RemainingOccupants, AffUsersChanges),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(User)).

-spec get_disco_rooms(User :: escalus:client()) -> list(#xmlel{}).
get_disco_rooms(User) ->
    DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), ?MUCHOST),
    escalus:send(User, DiscoStanza),
    Stanza =  escalus:wait_for_stanza(User),
    XNamespaces = exml_query:paths(Stanza, [{element, <<"query">>}, {attr, <<"xmlns">>}]),
    true = lists:member(?NS_DISCO_ITEMS, XNamespaces),
    escalus:assert(is_stanza_from, [?MUCHOST], Stanza),
    exml_query:paths(Stanza, [{element, <<"query">>}, {element, <<"item">>}]).

%%--------------------------------------------------------------------
%% IQ getters
%%--------------------------------------------------------------------

-spec stanza_blocking_get() -> #xmlel{}.
stanza_blocking_get() ->
    escalus_stanza:to(escalus_stanza:iq_get(?NS_MUC_LIGHT_BLOCKING, []), ?MUCHOST).

-spec stanza_config_get(Room :: binary(), Ver :: binary()) -> #xmlel{}.
stanza_config_get(Room, Ver) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_LIGHT_CONFIGURATION, [version_el(Ver)]), room_bin_jid(Room)).

-spec stanza_info_get(Room :: binary(), Ver :: binary()) -> #xmlel{}.
stanza_info_get(Room, Ver) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_LIGHT_INFO, [version_el(Ver)]), room_bin_jid(Room)).

-spec stanza_aff_get(Room :: binary(), Ver :: binary()) -> #xmlel{}.
stanza_aff_get(Room, Ver) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_LIGHT_AFFILIATIONS, [version_el(Ver)]), room_bin_jid(Room)).

%%--------------------------------------------------------------------
%% IQ setters
%%--------------------------------------------------------------------

-spec stanza_blocking_set(BlocklistChanges :: [ct_block_item()]) -> #xmlel{}.
stanza_blocking_set(BlocklistChanges) ->
    Items = [#xmlel{ name = list_to_binary(atom_to_list(What)),
                     attrs = [{<<"action">>, list_to_binary(atom_to_list(Action))}],
                     children = [#xmlcdata{ content = Who }] }
             || {What, Action, Who} <- BlocklistChanges],
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_BLOCKING, Items), ?MUCHOST).

-spec stanza_create_room(RoomNode :: binary() | undefined, InitConfig :: [{binary(), binary()}],
                         InitOccupants :: ct_aff_users()) -> #xmlel{}.
stanza_create_room(RoomNode, InitConfig, InitOccupants) ->
    ToBinJID = case RoomNode of
                     undefined -> ?MUCHOST;
                     _ -> <<RoomNode/binary, $@, (?MUCHOST)/binary>>
                 end,
    ConfigItem = #xmlel{ name = <<"configuration">>,
                         children = [ kv_el(K, V) || {K, V} <- InitConfig ] },
    OccupantsItems = [ #xmlel{ name = <<"user">>,
                               attrs = [{<<"affiliation">>, BinAff}],
                               children = [#xmlcdata{ content = BinJID }] }
                       || {BinJID, BinAff} <- bin_aff_users(InitOccupants) ],
    OccupantsItem = #xmlel{ name = <<"occupants">>, children = OccupantsItems },
    escalus_stanza:to(escalus_stanza:iq_set(
                        ?NS_MUC_LIGHT_CREATE, [ConfigItem, OccupantsItem]), ToBinJID).

-spec stanza_destroy_room(Room :: binary()) -> #xmlel{}.
stanza_destroy_room(Room) ->
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_DESTROY, []), room_bin_jid(Room)).

-spec stanza_config_set(Room :: binary(), ConfigChanges :: [{binary(), binary()}]) -> #xmlel{}.
stanza_config_set(Room, ConfigChanges) ->
    Items = [ kv_el(Key, Value) || {Key, Value} <- ConfigChanges],
    escalus_stanza:to(
      escalus_stanza:iq_set(?NS_MUC_LIGHT_CONFIGURATION, Items), room_bin_jid(Room)).

-spec stanza_aff_set(Room :: binary(), AffUsers :: ct_aff_users()) -> #xmlel{}.
stanza_aff_set(Room, AffUsers) ->
    Items = [#xmlel{ name = <<"user">>, attrs = [{<<"affiliation">>, AffBin}],
                     children = [#xmlcdata{ content = UserBin }] }
             || {UserBin, AffBin} <- bin_aff_users(AffUsers)],
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_AFFILIATIONS, Items), room_bin_jid(Room)).

%%--------------------------------------------------------------------
%% Verifiers
%%--------------------------------------------------------------------

-spec verify_blocklist(Query :: #xmlel{}, ProperBlocklist :: [ct_block_item()]) -> [].
verify_blocklist(Query, ProperBlocklist) ->
    ?NS_MUC_LIGHT_BLOCKING = exml_query:attr(Query, <<"xmlns">>),
    BlockedRooms = exml_query:subelements(Query, <<"room">>),
    BlockedUsers = exml_query:subelements(Query, <<"user">>),
    BlockedItems = [{list_to_atom(binary_to_list(What)), list_to_atom(binary_to_list(Action)), Who}
                    || #xmlel{name = What, attrs = [{<<"action">>, Action}],
                              children = [#xmlcdata{ content = Who }]}
                       <- BlockedRooms ++ BlockedUsers],
    ProperBlocklistLen = length(ProperBlocklist),
    ProperBlocklistLen = length(BlockedItems),
    [] = lists:foldl(fun lists:delete/2, BlockedItems, ProperBlocklist).

-spec disco_items_verify_fun(list(Jid :: binary())) -> verify_fun().
disco_items_verify_fun(JidList) ->
    fun(Incomming) ->
        ResultItemList = exml_query:paths(Incomming, [{element, <<"query">>}, {element, <<"item">>}]),
        ResultJids = [exml_query:attr(ResultItem, <<"jid">>) || ResultItem <- ResultItemList],
        {SortedResult, SortedExptected} = {lists:sort(JidList), lists:sort(ResultJids)},
        SortedResult = SortedExptected
    end.

verify_aff_bcast(CurrentOccupants, AffUsersChanges) ->
    verify_aff_bcast(CurrentOccupants, AffUsersChanges, []).

verify_aff_bcast(CurrentOccupants, AffUsersChanges, ExtraNSs) ->
    foreach_recipient(
      [ User || {User, _} <- CurrentOccupants ], aff_msg_verify_fun(AffUsersChanges)),
    lists:foreach(
      fun({Leaver, none}) ->
              Incoming = escalus:wait_for_stanza(Leaver),
              %% This notification must come from the room bare JID
              [_, ?MUCHOST] = binary:split(exml_query:attr(Incoming, <<"from">>), <<"@">>),
              {[X], []} = lists:foldl(
                            fun(XEl, {XAcc, NSAcc}) ->
                                    XMLNS = exml_query:attr(XEl, <<"xmlns">>),
                                    case lists:member(XMLNS, NSAcc) of
                                        true -> {XAcc, lists:delete(XMLNS, NSAcc)};
                                        false -> {[XEl | XAcc], NSAcc}
                                    end
                            end, {[], ExtraNSs}, exml_query:subelements(Incoming, <<"x">>)),
              ?NS_MUC_LIGHT_AFFILIATIONS = exml_query:attr(X, <<"xmlns">>),
              [Item] = exml_query:subelements(X, <<"user">>),
              <<"none">> = exml_query:attr(Item, <<"affiliation">>),
              LeaverJIDBin = lbin(escalus_client:short_jid(Leaver)),
              LeaverJIDBin = exml_query:cdata(Item);
         (_) ->
              ignore
      end, AffUsersChanges).

-spec verify_no_stanzas(Users :: [escalus:client()]) -> ok.
verify_no_stanzas(Users) ->
    lists:foreach(
      fun(User) ->
              {false, _} = {escalus_client:has_stanzas(User), User}
      end, Users).

-spec verify_config(ConfigRoot :: #xmlel{}, Config :: [{binary(), binary()}]) -> ok.
verify_config(ConfigRoot, Config) ->
    lists:foreach(
      fun({Key, Val}) ->
              Val = exml_query:path(ConfigRoot, [{element, Key}, cdata])
      end, Config).

-spec verify_aff_users(Items :: [#xmlel{}], BinAffUsers :: [{binary(), binary()}]) -> [].
verify_aff_users(Items, BinAffUsers) ->
    true = (length(Items) == length(BinAffUsers)),
    [] = lists:foldl(
           fun(Item, AffAcc) ->
                   JID = exml_query:cdata(Item),
                   Aff = exml_query:attr(Item, <<"affiliation">>),
                   verify_keytake(lists:keytake(JID, 1, AffAcc), JID, Aff, AffAcc)
           end, BinAffUsers, Items).

-spec verify_keytake(Result :: {value, Item :: tuple(), Acc :: list()}, JID :: binary(),
                     Aff :: binary(), AffAcc :: list()) -> list().
verify_keytake({value, {_, Aff}, NewAffAcc}, _JID, Aff, _AffAcc) -> NewAffAcc.

%%--------------------------------------------------------------------
%% Verification funs generators
%%--------------------------------------------------------------------

-spec gc_message_verify_fun(Room :: binary(), MsgText :: binary(), Id :: binary()) -> verify_fun().
gc_message_verify_fun(Room, MsgText, Id) ->
    fun(Incoming) ->
            escalus:assert(is_groupchat_message, [MsgText], Incoming),
            [RoomBareJID, FromNick] = binary:split(exml_query:attr(Incoming, <<"from">>), <<"/">>),
            [Room, ?MUCHOST] = binary:split(RoomBareJID, <<"@">>),
            [_] = binary:split(FromNick, <<"/">>), % nick is bare JID
            Id = exml_query:attr(Incoming, <<"id">>)
    end.

-spec config_msg_verify_fun(RoomConfig :: [{binary(), binary()}]) -> verify_fun().
config_msg_verify_fun(RoomConfig) ->
    fun(Incoming) ->
            escalus:assert(is_groupchat_message, Incoming),
            [X] = exml_query:subelements(Incoming, <<"x">>),
            ?NS_MUC_LIGHT_CONFIGURATION = exml_query:attr(X, <<"xmlns">>),
            PrevVersion = exml_query:path(X, [{element, <<"prev-version">>}, cdata]),
            Version = exml_query:path(X, [{element, <<"version">>}, cdata]),
            true = is_binary(Version),
            true = is_binary(PrevVersion),
            true = Version =/= PrevVersion,
            lists:foreach(
              fun({Key, Val}) ->
                      Val = exml_query:path(X, [{element, Key}, cdata])
              end, RoomConfig)
    end.

-spec config_iq_verify_fun(RoomConfig :: [{binary(), binary()}]) -> verify_fun().
config_iq_verify_fun(RoomConfig) ->
    fun(Incoming) ->
            [Query] = exml_query:subelements(Incoming, <<"query">>),
            ?NS_MUC_LIGHT_CONFIGURATION = exml_query:attr(Query, <<"xmlns">>),
            verify_config(Query, RoomConfig)
    end.

-spec aff_iq_verify_fun(AffUsers :: ct_aff_users(), Version :: binary()) -> verify_fun().
aff_iq_verify_fun(AffUsers, Version) ->
    BinAffUsers = bin_aff_users(AffUsers),
    fun(Incoming) ->
            [Query] = exml_query:subelements(Incoming, <<"query">>),
            ?NS_MUC_LIGHT_AFFILIATIONS = exml_query:attr(Query, <<"xmlns">>),
            Version = exml_query:path(Query, [{element, <<"version">>}, cdata]),
            Items = exml_query:subelements(Query, <<"user">>),
            verify_aff_users(Items, BinAffUsers)
    end.

-spec aff_msg_verify_fun(AffUsersChanges :: ct_aff_users()) -> verify_fun().
aff_msg_verify_fun(AffUsersChanges) ->
    BinAffUsersChanges = bin_aff_users(AffUsersChanges),
    fun(Incoming) ->
            [X] = exml_query:subelements(Incoming, <<"x">>),
            ?NS_MUC_LIGHT_AFFILIATIONS = exml_query:attr(X, <<"xmlns">>),
            PrevVersion = exml_query:path(X, [{element, <<"prev-version">>}, cdata]),
            Version = exml_query:path(X, [{element, <<"version">>}, cdata]),
            [Item | RItems] = Items = exml_query:subelements(X, <<"user">>),
            [ToBin | _] = binary:split(exml_query:attr(Incoming, <<"to">>), <<"/">>),
            true = is_binary(Version),
            true = Version =/= PrevVersion,
            case {ToBin == exml_query:cdata(Item), RItems} of
                {true, []} ->
                    {_, ProperAff} = lists:keyfind(ToBin, 1, BinAffUsersChanges),
                    ProperAff = exml_query:attr(Item, <<"affiliation">>);
                _ ->
                    true = is_binary(PrevVersion),
                    verify_aff_users(Items, BinAffUsersChanges)
            end
    end.

-spec info_iq_verify_fun(AffUsers :: ct_aff_users(), Version :: binary(),
                         ConfigKVBin :: [{binary(), binary()}]) -> verify_fun().
info_iq_verify_fun(AffUsers, Version, ConfigKVBin) ->
    BinAffUsers = bin_aff_users(AffUsers),
    fun(Incoming) ->
            [Query] = exml_query:subelements(Incoming, <<"query">>),
            ?NS_MUC_LIGHT_INFO = exml_query:attr(Query, <<"xmlns">>),
            Version = exml_query:path(Query, [{element, <<"version">>}, cdata]),
            UsersItems = exml_query:paths(Query, [{element, <<"occupants">>}, {element, <<"user">>}]),
            verify_aff_users(UsersItems, BinAffUsers),
            ConfigurationEl = exml_query:subelement(Query, <<"configuration">>),
            verify_config(ConfigurationEl, ConfigKVBin)
    end.

-spec verify_user_has_one_room(User :: escalus:client()) -> any().
verify_user_has_one_room(User) ->
        [Item] =  get_disco_rooms(User),
        ProperJID = room_bin_jid(?ROOM),
        ProperJID = exml_query:attr(Item, <<"jid">>).

%%--------------------------------------------------------------------
%% Other helpers
%%--------------------------------------------------------------------

-spec ver(Int :: integer()) -> binary().
ver(Int) ->
    <<"ver-", (list_to_binary(integer_to_list(Int)))/binary>>.

-spec version_el(Version :: binary()) -> #xmlel{}.
version_el(Version) ->
    #xmlel{ name = <<"version">>, children = [#xmlcdata{ content = Version }] }.

-spec kv_el(K :: binary(), V :: binary()) -> #xmlel{}.
kv_el(K, V) ->
    #xmlel{ name = K, children = [ #xmlcdata{ content = V } ] }.

-spec bin_aff_users(AffUsers :: ct_aff_users()) -> [{LBinJID :: binary(), AffBin :: binary()}].
bin_aff_users(AffUsers) ->
    [ {lbin(escalus_client:short_jid(User)), list_to_binary(atom_to_list(Aff))}
      || {User, Aff} <- AffUsers ].

-spec room_bin_jid(Room :: binary()) -> binary().
room_bin_jid(Room) -> <<Room/binary, $@, (?MUCHOST)/binary>>.

-spec to_lus(Config :: list(), UserAtom :: atom()) -> {binary(), binary()}.
to_lus(UserAtom, Config) ->
    {lbin(escalus_users:get_username(Config, UserAtom)),
     lbin(escalus_users:get_server(Config, UserAtom))}.

-spec lbin(Bin :: binary()) -> binary().
lbin(Bin) -> list_to_binary(string:to_lower(binary_to_list(Bin))).

-spec default_config() -> list().
default_config() -> rpc(mod_muc_light, default_config, [?MUCHOST]).

-spec standard_default_config() -> list().
standard_default_config() -> rpc(mod_muc_light, standard_default_config, []).

-spec custom_default_config() -> list().
custom_default_config() -> [{background, <<"builtin:hell">>}, {music, <<"builtin:screams">>}].

-spec standard_config_schema() -> list().
standard_config_schema() -> rpc(mod_muc_light, standard_config_schema, []).

-spec set_default_mod_config() -> ok.
set_default_mod_config() ->
    lists:foreach(
      fun({K, V}) -> set_mod_config(K, V) end,
      [
       {equal_occupants, false},
       {rooms_per_user, infinity},
       {blocking, true},
       {all_can_configure, false},
       {all_can_invite, false},
       {max_occupants, infinity},
       {rooms_per_page, infinity}
      ]).

-spec set_custom_config(RawSchema :: list(), RawDefaultConfig :: list()) -> true.
set_custom_config(RawSchema, RawDefaultConfig) ->
    ConfigSchema = rpc(mod_muc_light_utils, make_config_schema, [RawSchema]),
    _ = hd(ConfigSchema), %% checks if is a list
    set_mod_config(config_schema, ConfigSchema),

    DefaultConfig = rpc(mod_muc_light_utils, make_default_config, [RawDefaultConfig, ConfigSchema]),
    _ = hd(DefaultConfig), %% checks if is a list
    set_mod_config(default_config, DefaultConfig).

-spec set_mod_config(K :: atom(), V :: any()) -> ok.
set_mod_config(K, V) ->
    true = rpc(mod_muc_light, set_opt, [?MUCHOST, K, V]).

-spec ns_muc_light_affiliations() -> binary().
ns_muc_light_affiliations() ->
    ?NS_MUC_LIGHT_AFFILIATIONS.

-spec room2() -> binary().
room2() ->
    ?ROOM2.

