-module(muc_light_SUITE).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("mam_helper.hrl").

-export([ % service
         removing_users_from_server_triggers_room_destruction/1
        ]).
-export([ % entity
         disco_service/1,
         disco_features/1,
         disco_features_with_mam/1,
         disco_info/1,
         disco_info_with_mam/1,
         disco_rooms/1,
         disco_rooms_empty_page_1/1,
         disco_rooms_empty_page_infinity/1,
         disco_rooms_created_page_1/1,
         disco_rooms_created_page_infinity/1,
         disco_rooms_rsm/1,
         rooms_in_rosters/1,
         rooms_in_rosters_doesnt_break_disco_info/1,
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
         set_partial_config/1,
         set_config_with_custom_schema/1,
         deny_config_change_that_conflicts_with_schema/1,
         assorted_config_doesnt_lead_to_duplication/1,
         remove_and_add_users/1,
         multiple_owner_change/1,
         explicit_owner_change/1,
         implicit_owner_change/1,
         edge_case_owner_change/1,
         adding_wrongly_named_user_triggers_infinite_loop/1
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

-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-import(escalus_ejabberd, [rpc/3]).
-import(muc_helper, [foreach_occupant/3, foreach_recipient/2]).
-import(distributed_helper, [subhost_pattern/1]).
-import(domain_helper, [host_type/0, domain/0]).
-import(muc_light_helper, [
                           bin_aff_users/1,
                           gc_message_verify_fun/3,
                           lbin/1,
                           room_bin_jid/1,
                           verify_aff_bcast/2,
                           verify_aff_bcast/3,
                           verify_aff_users/2,
                           kv_el/2,
                           stanza_create_room/3,
                           create_room/6,
                           stanza_aff_set/2,
                           user_leave/3,
                           stanza_blocking_set/1,
                           default_config/1,
                           default_schema/0,
                           eq_bjid/2,
                           pos_int/1,
                           cache_name/0
                          ]).
-import(config_parser_helper, [mod_config/2]).

-include("muc_light.hrl").

-define(ROOM, <<"testroom">>).
-define(ROOM2, <<"testroom2">>).

-define(MUCHOST, (muc_light_helper:muc_host())).

-type ct_aff_user() :: {EscalusClient :: escalus:client(), Aff :: atom()}.
-type ct_aff_users() :: [ct_aff_user()].
-type ct_block_item() :: muc_light_helper:ct_block_item().
-type verify_fun() :: muc_helper:verify_fun().
-type xmlel() :: exml:element().

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
    G = [
         {service, [sequence], [
                                removing_users_from_server_triggers_room_destruction
                               ]},
         {entity, [sequence], [
                               disco_service,
                               disco_features,
                               disco_features_with_mam,
                               disco_info,
                               disco_info_with_mam,
                               disco_rooms,
                               disco_rooms_rsm,
                               disco_rooms_created_page_1,
                               disco_rooms_created_page_infinity,
                               disco_rooms_empty_page_infinity,
                               disco_rooms_empty_page_1,
                               unauthorized_stanza,
                               rooms_in_rosters,
                               rooms_in_rosters_doesnt_break_disco_info,
                               no_roomname_in_schema_doesnt_break_disco_and_roster
                              ]},
         {occupant, [sequence], [
                                 send_message,
                                 change_subject,
                                 change_roomname,
                                 all_can_configure,
                                 set_config_deny,
                                 get_room_config,
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
                              set_partial_config,
                              set_config_with_custom_schema,
                              deny_config_change_that_conflicts_with_schema,
                              assorted_config_doesnt_lead_to_duplication,
                              remove_and_add_users,
                              explicit_owner_change,
                              multiple_owner_change,
                              implicit_owner_change,
                              edge_case_owner_change,
                              adding_wrongly_named_user_triggers_infinite_loop
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
        ],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    Config2 = escalus:init_per_suite(Config1),
    instrument_helper:start([{user_cache_lookup, #{host_type => host_type(),
                                                   cache_name => cache_name()}}]),
    escalus:create_users(Config2, escalus:get_users([alice, bob, kate, mike])).

end_per_suite(Config) ->
    HostType = host_type(),
    muc_light_helper:clear_db(HostType),
    Config1 = escalus:delete_users(Config, escalus:get_users([alice, bob, kate, mike])),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config1),
    instrument_helper:stop().

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-define(ROOM_LESS_CASES, [disco_rooms_empty_page_infinity,
                          disco_rooms_empty_page_1]).

-define(CUSTOM_CONFIG_CASES, [set_config_with_custom_schema,
                              deny_config_change_that_conflicts_with_schema,
                              no_roomname_in_schema_doesnt_break_disco_and_roster,
                              custom_default_config_works]).

init_per_testcase(removing_users_from_server_triggers_room_destruction = CaseName, Config) ->
    dynamic_modules:ensure_modules(host_type(), required_modules(CaseName)),
    Config1 = escalus:create_users(Config, escalus:get_users([carol])),
    create_room(?ROOM, ?MUCHOST, carol, [], Config1, ver(1)),
    escalus:init_per_testcase(CaseName, Config1);
init_per_testcase(disco_rooms_rsm = CaseName, Config) ->
    dynamic_modules:ensure_modules(host_type(), required_modules(CaseName)),
    create_room(?ROOM, ?MUCHOST, alice, [bob, kate], Config, ver(1)),
    create_room(?ROOM2, ?MUCHOST, alice, [bob, kate], Config, ver(1)),
    escalus:init_per_testcase(disco_rooms_rsm, Config);
init_per_testcase(CaseName, Config) when CaseName =:= disco_features_with_mam;
                                         CaseName =:= disco_info_with_mam ->
    case mam_helper:backend() of
        disabled ->
            {skip, "No MAM backend available"};
        Backend ->
            dynamic_modules:ensure_modules(host_type(), required_modules(CaseName, Backend)),
            create_room(?ROOM, ?MUCHOST, alice, [bob, kate], Config, ver(1)),
            escalus:init_per_testcase(CaseName, Config)
    end;
init_per_testcase(CaseName, Config) ->
    dynamic_modules:ensure_modules(host_type(), required_modules(CaseName)),
    case lists:member(CaseName, ?ROOM_LESS_CASES) of
        false -> create_room(?ROOM, ?MUCHOST, alice, [bob, kate], Config, ver(1));
        _ -> ok
    end,
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    muc_light_helper:clear_db(host_type()),
    escalus:end_per_testcase(CaseName, Config).

%% Module configuration per test case

required_modules(CaseName, MAMBackend) ->
    [{mod_mam, mam_helper:config_opts(#{backend => MAMBackend,
                                        muc => #{host => subhost_pattern(?MUCHOST)}})} |
     common_required_modules(CaseName)].

required_modules(CaseName) ->
    [{mod_mam, stopped} | common_required_modules(CaseName)].

common_required_modules(CaseName) ->
    BasicOpts = maps:merge(common_muc_light_opts(), muc_light_opts(CaseName)),
    Opts = maps:merge(BasicOpts, schema_opts(CaseName)),
    [{mod_muc_light, mod_config(mod_muc_light, Opts)}].

muc_light_opts(CaseName) when CaseName =:= disco_rooms_rsm;
                              CaseName =:= disco_rooms_empty_page_1;
                              CaseName =:= rooms_created_page_1 ->
    #{rooms_per_page => 1};
muc_light_opts(CaseName) when CaseName =:= disco_rooms_empty_page_infinity;
                              CaseName =:= disco_rooms_created_page_infinity ->
    #{rooms_per_page => infinity};
muc_light_opts(all_can_configure) ->
    #{all_can_configure => true};
muc_light_opts(create_room_with_equal_occupants) ->
    #{equal_occupants => true};
muc_light_opts(rooms_per_user) ->
    #{rooms_per_user => 1};
muc_light_opts(max_occupants) ->
    #{max_occupants => 1};
muc_light_opts(block_user) ->
    #{all_can_invite => true};
muc_light_opts(blocking_disabled) ->
    #{blocking => false};
muc_light_opts(allow_multiple_admin) ->
    #{allow_multiple_admin => true};
muc_light_opts(_) ->
    #{}.

schema_opts(CaseName) ->
    case lists:member(CaseName, ?CUSTOM_CONFIG_CASES) of
        true -> #{config_schema => custom_schema()};
        false -> #{}
    end.

common_muc_light_opts() ->
    #{backend => mongoose_helper:mnesia_or_rdbms_backend(),
      cache_affs => config_parser_helper:default_config([modules, mod_muc_light, cache_affs]),
      rooms_in_rosters => true}.

%%--------------------------------------------------------------------
%% MUC light tests
%%--------------------------------------------------------------------

%% ---------------------- Service ----------------------

removing_users_from_server_triggers_room_destruction(Config) ->
    escalus:delete_users(Config, escalus:get_users([carol])),
    {error, not_exists} = rpc(mod_muc_light_db_backend, get_info, [host_type(), {?ROOM, ?MUCHOST}]).

%% ---------------------- Disco ----------------------

disco_service(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            Server = escalus_client:server(Alice),
            escalus:send(Alice, escalus_stanza:service_discovery(Server)),
            Stanza = escalus:wait_for_stanza(Alice),
            Query = exml_query:subelement(Stanza, <<"query">>),
            Item = exml_query:subelement_with_attr(Query, <<"jid">>, ?MUCHOST),
            ?assertEqual(?NS_MUC_LIGHT, exml_query:attr(Item, <<"node">>)),
            escalus:assert(is_stanza_from, [domain()], Stanza)
        end).

disco_features(Config) ->
    disco_features_story(Config, false).

disco_features_with_mam(Config) ->
    disco_features_story(Config, true).

disco_features_story(Config, HasMAM) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_INFO, []), ?MUCHOST),
            escalus:send(Alice, DiscoStanza),
            Stanza = escalus:wait_for_stanza(Alice),
            <<"conference">> = exml_query:path(Stanza, [{element, <<"query">>},
                                                        {element, <<"identity">>},
                                                        {attr, <<"category">>}]),
            check_features(Stanza, HasMAM),
            escalus:assert(is_stanza_from, [?MUCHOST], Stanza)
        end).

disco_info(Config) ->
    disco_info_story(Config, false).

disco_info_with_mam(Config) ->
    disco_info_story(Config, true).

disco_info_story(Config, HasMAM) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            RoomJID = room_bin_jid(?ROOM),
            DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_INFO, []), RoomJID),
            escalus:send(Alice, DiscoStanza),
            Stanza = escalus:wait_for_stanza(Alice),
            check_features(Stanza, HasMAM),
            escalus:assert(is_stanza_from, [RoomJID], Stanza)
        end).

check_features(Stanza, HasMAM) ->
    ExpectedFeatures = expected_features(HasMAM),
    ActualFeatures = exml_query:paths(Stanza, [{element, <<"query">>},
                                               {element, <<"feature">>},
                                               {attr, <<"var">>}]),
    ?assertEqual(ExpectedFeatures, ActualFeatures).

expected_features(true) ->
    lists:sort([?NS_MUC_LIGHT | mam_helper:namespaces()]);
expected_features(false) ->
    [?NS_MUC_LIGHT].

%% The room list is empty. Rooms_per_page set to `infinity`
disco_rooms_empty_page_infinity(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        [] = get_disco_rooms(Alice)
        end).

%% The room list is empty. Rooms_per_page set to 1
disco_rooms_empty_page_1(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        [] = get_disco_rooms(Alice)
        end).

%% There is one room created. Rooms_per_page set to 1
disco_rooms_created_page_1(Config) ->
    escalus:story(Config, [{alice, 1}], fun verify_user_has_one_room/1).

%% There is one room created. Rooms_per_page set to `infinity`
disco_rooms_created_page_infinity(Config) ->
    escalus:story(Config, [{alice, 1}], fun verify_user_has_one_room/1).

disco_rooms(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            MucHost = ?MUCHOST,
            {ok, {?ROOM2, MucHost}} = create_room(?ROOM2, ?MUCHOST, kate, [], Config, ver(0)),
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
            AliceU = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
            AliceS = escalus_utils:jid_to_lower(escalus_client:server(Alice)),
            escalus:send(Alice, escalus_stanza:roster_get()),
            mongoose_helper:wait_until(
                fun() ->
                    distributed_helper:rpc(
                        distributed_helper:mim(),
                        mod_roster,
                        get_user_rosters_length,
                        [host_type(), jid:make(AliceU, AliceS, <<>>)])
                end, 1, #{time_left => timer:seconds(10)}),
            RosterResult = escalus:wait_for_stanza(Alice),
            escalus_assert:is_roster_result(RosterResult),

            [Item] = exml_query:paths(
                       RosterResult, [{element, <<"query">>}, {element, <<"item">>}]),
            ProperJID = room_bin_jid(?ROOM),
            ProperJID = exml_query:attr(Item, <<"jid">>),
            ProperName = proplists:get_value(<<"roomname">>, default_config(default_schema())),
            ProperName = exml_query:attr(Item, <<"name">>),
            ProperVer = ver(1),
            ProperVer = exml_query:path(Item, [{element, <<"version">>}, cdata])
        end).

rooms_in_rosters_doesnt_break_disco_info(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            % Verify that room is in roster
            escalus:send(Alice, escalus_stanza:roster_get()),
            RosterResult = escalus:wait_for_stanza(Alice),
            [RosterItem] = exml_query:paths(
                       RosterResult, [{element, <<"query">>}, {element, <<"item">>}]),
            RoomJID = room_bin_jid(?ROOM),
            RoomJID = exml_query:attr(RosterItem, <<"jid">>),

            % Verify that disco#info doesn't crash when rooms are in roster
            DiscoStanza = escalus_stanza:disco_info(escalus_client:short_jid(Alice)),
            IQRes = escalus:send_iq_and_wait_for_result(Alice, DiscoStanza),
            escalus:assert(is_iq_result, IQRes)
        end).

no_roomname_in_schema_doesnt_break_disco_and_roster(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            [DiscoItem] = get_disco_rooms(Alice),
            ?assert_equal_extra(?ROOM, exml_query:attr(DiscoItem, <<"name">>),
                                #{elem => DiscoItem}),

            escalus:send(Alice, escalus_stanza:roster_get()),
            RosterResult = escalus:wait_for_stanza(Alice),
            [RosterItem] = exml_query:paths(
                       RosterResult, [{element, <<"query">>}, {element, <<"item">>}]),
            ?ROOM = exml_query:attr(RosterItem, <<"name">>)
        end).

unauthorized_stanza(Config) ->
    escalus:story(Config, [{alice, 1}, {kate, 1}], fun(Alice, Kate) ->
            MucHost = ?MUCHOST,
            {ok, {?ROOM2, MucHost}} = create_room(?ROOM2, ?MUCHOST, kate, [], Config, ver(0)),
            MsgStanza = escalus_stanza:groupchat_to(room_bin_jid(?ROOM2), <<"malicious">>),
            escalus:send(Alice, MsgStanza),
            escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>],
                           escalus:wait_for_stanza(Alice)),
            verify_no_stanzas([Alice, Kate])
        end).

%% ---------------------- Occupant ----------------------

send_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            TS = instrument_helper:timestamp(),
            Msg = <<"Heyah!">>,
            Id = <<"MyID">>,
            Stanza = escalus_stanza:set_id(
                       escalus_stanza:groupchat_to(room_bin_jid(?ROOM), Msg), Id),
            foreach_occupant([Alice, Bob, Kate], Stanza, gc_message_verify_fun(?ROOM, Msg, Id)),
            assert_cache_miss_event(TS, 1, room_bin_jid(?ROOM)),
            assert_cache_hit_event(TS, 2, room_bin_jid(?ROOM))
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
            ConfigKV = [{<<"version">>, ver(1)} | default_config(default_schema())],
            foreach_occupant([Alice, Bob, Kate], Stanza, config_iq_verify_fun(ConfigKV)),

            %% Empty result when user has most recent version
            escalus:send(Bob, stanza_config_get(?ROOM, ver(1))),
            IQRes = escalus:wait_for_stanza(Bob),
            escalus:assert(is_iq_result, IQRes),
            undefined = exml_query:subelement(IQRes, <<"query">>)
        end).

custom_default_config_works(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            Stanza = stanza_config_get(?ROOM, <<"oldver">>),
            ConfigKV = [{<<"version">>, ver(1)} | default_config(custom_schema())],
            foreach_occupant([Alice, Bob, Kate], Stanza, config_iq_verify_fun(ConfigKV))
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
            ConfigKV = default_config(default_schema()),
            foreach_occupant([Alice, Bob, Kate], Stanza,
                             info_iq_verify_fun(?DEFAULT_AFF_USERS, ver(1), ConfigKV)),

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
                      user_leave(?ROOM, User, NewOccupants),
                      verify_no_stanzas(Outsiders),
                      {NewOccupants, [User | Outsiders]}
              end, {?DEFAULT_AFF_USERS, []}, [Alice, Bob, Kate]),

            % Now we verify that room is removed from DB
            {error, not_exists} = rpc(mod_muc_light_db_backend, get_info, [host_type(), {?ROOM, ?MUCHOST}])
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
        MucHost = ?MUCHOST,
        {ok, {?ROOM2, MucHost}} = create_room(?ROOM2, ?MUCHOST, kate,
                                              [bob, alice], Config, ver(0)),
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
            TS = instrument_helper:timestamp(),
            ConfigChange = [{<<"roomname">>, <<"The Coven">>}],
            change_room_config(Alice, [Alice, Bob, Kate], ConfigChange),
            assert_cache_miss_event(TS, 1, room_bin_jid(?ROOM))
        end).

set_partial_config(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            FinalRoomName = <<"The Coven">>,
            %% Change all the config
            ConfigChange = [{<<"roomname">>, FinalRoomName}, {<<"subject">>, <<"Evil Ravens">>}],
            change_room_config(Alice, [Alice, Bob, Kate], ConfigChange),
            %% Now change only the subject
            ConfigChange2 = [{<<"subject">>, <<"Good Ravens">>}],
            change_room_config(Alice, [Alice, Bob, Kate], ConfigChange2),
            %% Verify that roomname is still the original change
            escalus:send(Alice, stanza_config_get(?ROOM, ver(1))),
            IQRes = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, IQRes),
            RoomName = exml_query:path(IQRes, [{element, <<"query">>}, {element, <<"roomname">>}, cdata]),
            ?assertEqual(FinalRoomName, RoomName)
        end).

set_config_with_custom_schema(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"background">>, <<"builtin:unicorns">>},
                            {<<"music">>, <<"builtin:rainbow">>}],
            change_room_config(Alice, [Alice, Bob, Kate], ConfigChange)
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
            change_room_config(Alice, [Alice, Bob, Kate], ConfigChange),

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
            TS = instrument_helper:timestamp(),
            AffUsersChanges1 = [{Bob, none}, {Kate, none}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges1)),
            verify_aff_bcast([{Alice, owner}], AffUsersChanges1),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            AffUsersChanges2 = [{Bob, member}, {Kate, member}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges2)),
            verify_aff_bcast([{Alice, owner}, {Bob, member}, {Kate, member}], AffUsersChanges2),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            assert_cache_miss_event(TS, 1, room_bin_jid(?ROOM)),
            assert_cache_hit_event(TS, 1, room_bin_jid(?ROOM))
        end).

multiple_owner_change(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            TS = instrument_helper:timestamp(),
            AffUsersChanges1 = [{Bob, none}, {Kate, none}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges1)),
            verify_aff_bcast([{Alice, owner}], AffUsersChanges1),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            AffUsersChanges2 = [{Bob, owner}, {Kate, owner}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges2)),
            verify_aff_bcast([{Alice, owner}, {Bob, owner}, {Kate, owner}], AffUsersChanges2),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            assert_cache_miss_event(TS, 1, room_bin_jid(?ROOM)),
            assert_cache_hit_event(TS, 1, room_bin_jid(?ROOM))
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

adding_wrongly_named_user_triggers_infinite_loop(Config)->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            BuggyRoomName = <<"buggyroom">>,
            Username = <<"buggyuser">>,
            escalus:send(Alice, generate_buggy_aff_staza(BuggyRoomName, Username)),
            timer:sleep(300),
            AUsername = lbin(escalus_users:get_username(Config, alice)),
            Host = lbin(escalus_users:get_server(Config, alice)),
            Resource = <<"res1">>,
            JID = mongoose_helper:make_jid(AUsername, Host, Resource),
            ct:log("JID ~p", [JID]),
            SessionRecPid = rpc(ejabberd_sm, get_session, [JID]),
            {session, {_,Pid}, {AUsername, Host, Resource}, _, _, _} = SessionRecPid,
            %% maybe throws exception
            assert_process_memory_not_growing(Pid, 0, 2),
            escalus:wait_for_stanzas(Alice, 2)
    end).

%% ---------------------- limits ----------------------

rooms_per_user(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}],
                  fun(Alice, Bob, Kate, Mike) ->
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
            Domain = domain(),
            BlocklistChange1 = [{user, deny, <<"user@", Domain/binary>>},
                                {room, deny, room_bin_jid(?ROOM)}],
            escalus:send(Alice, stanza_blocking_set(BlocklistChange1)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            escalus:send(Alice, stanza_blocking_get()),
            GetResult2 = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, GetResult2),
            QueryEl2 = exml_query:subelement(GetResult2, <<"query">>),
            verify_blocklist(QueryEl2, BlocklistChange1),

            BlocklistChange2 = [{user, allow, <<"user@", Domain/binary>>},
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
            user_leave(?ROOM, Bob, [{Alice, owner}, {Kate, member}]),

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
            user_leave(?ROOM, Bob, [{Alice, owner}, {Kate, member}]),

            % Alice tries to create new room with Bob but Bob is not added
            escalus:send(Alice, stanza_create_room(<<"new">>, [], [{Bob, member}])),
            verify_aff_bcast([{Alice, owner}], [{Alice, owner}]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            verify_no_stanzas([Alice, Bob, Kate]),

            % But Kate can add Bob to the main room!
            BobReadd = [{Bob, member}],
            SuccessStanza = stanza_aff_set(?ROOM, BobReadd),
            escalus:send(Kate, SuccessStanza),
            verify_aff_bcast([{Alice, owner}, {Bob, member}, {Kate, member}], BobReadd),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Kate)),
            verify_no_stanzas([Alice, Bob, Kate])

        end).

blocking_disabled(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            escalus:send(Alice, stanza_blocking_get()),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Alice)),
            Domain = domain(),
            BlocklistChange1 = [{user, deny, <<"user@", Domain/binary>>},
                                {room, deny, room_bin_jid(?ROOM)}],
            escalus:send(Alice, stanza_blocking_set(BlocklistChange1)),
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Alice))
        end).

%%--------------------------------------------------------------------
%% Subroutines
%%--------------------------------------------------------------------

-spec change_room_config(escalus:client(), [escalus:client()], list({binary(), binary()})) -> term().
change_room_config(User, Users, ConfigChange) ->
    escalus:send(User, stanza_config_set(?ROOM, ConfigChange)),
    foreach_recipient(Users, config_msg_verify_fun(ConfigChange)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(User)).

-spec get_disco_rooms(User :: escalus:client()) -> list(xmlel()).
get_disco_rooms(User) ->
    DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), ?MUCHOST),
    escalus:send(User, DiscoStanza),
    Stanza =  escalus:wait_for_stanza(User),
    XNamespaces = exml_query:paths(Stanza, [{element, <<"query">>}, {attr, <<"xmlns">>}]),
    true = lists:member(?NS_DISCO_ITEMS, XNamespaces),
    escalus:assert(is_stanza_from, [?MUCHOST], Stanza),
    exml_query:paths(Stanza, [{element, <<"query">>}, {element, <<"item">>}]).

-spec generate_buggy_aff_staza(RoomName :: binary(), Username :: binary()) -> xmlel().
generate_buggy_aff_staza(RoomName, Username) ->
    BuggyJid = <<Username/binary, "@muclight.localhost">>,
    BuggyUser = #client{jid = BuggyJid},
    stanza_create_room(RoomName, [], [{BuggyUser, member}]).

%%--------------------------------------------------------------------
%% IQ getters
%%--------------------------------------------------------------------

-spec stanza_blocking_get() -> xmlel().
stanza_blocking_get() ->
    escalus_stanza:to(escalus_stanza:iq_get(?NS_MUC_LIGHT_BLOCKING, []), ?MUCHOST).

-spec stanza_config_get(Room :: binary(), Ver :: binary()) -> xmlel().
stanza_config_get(Room, Ver) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_LIGHT_CONFIGURATION, [version_el(Ver)]), room_bin_jid(Room)).

-spec stanza_info_get(Room :: binary(), Ver :: binary()) -> xmlel().
stanza_info_get(Room, Ver) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_LIGHT_INFO, [version_el(Ver)]), room_bin_jid(Room)).

-spec stanza_aff_get(Room :: binary(), Ver :: binary()) -> xmlel().
stanza_aff_get(Room, Ver) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_LIGHT_AFFILIATIONS, [version_el(Ver)]), room_bin_jid(Room)).

%%--------------------------------------------------------------------
%% IQ setters
%%--------------------------------------------------------------------

-spec stanza_destroy_room(Room :: binary()) -> xmlel().
stanza_destroy_room(Room) ->
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_DESTROY, []), room_bin_jid(Room)).

-spec stanza_config_set(Room :: binary(), ConfigChanges :: [muc_light_helper:config_item()]) -> xmlel().
stanza_config_set(Room, ConfigChanges) ->
    Items = [ kv_el(Key, Value) || {Key, Value} <- ConfigChanges],
    escalus_stanza:to(
      escalus_stanza:iq_set(?NS_MUC_LIGHT_CONFIGURATION, Items), room_bin_jid(Room)).

%%--------------------------------------------------------------------
%% Verifiers
%%--------------------------------------------------------------------

-spec verify_blocklist(Query :: xmlel(), ProperBlocklist :: [ct_block_item()]) -> [].
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
        ResultItemList = exml_query:paths(Incomming,
                                          [{element, <<"query">>},
                                           {element, <<"item">>}]),
        ResultJids = [exml_query:attr(ResultItem, <<"jid">>) || ResultItem <- ResultItemList],
        {SortedResult, SortedExptected} = {lists:sort(JidList), lists:sort(ResultJids)},
        SortedResult = SortedExptected
    end.


-spec verify_no_stanzas(Users :: [escalus:client()]) -> ok.
verify_no_stanzas(Users) ->
    lists:foreach(
      fun(User) ->
              {false, _} = {escalus_client:has_stanzas(User), User}
      end, Users).

-spec verify_config(ConfigRoot :: xmlel(), Config :: [muc_light_helper:config_item()]) -> ok.
verify_config(ConfigRoot, Config) ->
    lists:foreach(
      fun({Key, Val}) ->
              Val = exml_query:path(ConfigRoot, [{element, Key}, cdata])
      end, Config).

%%--------------------------------------------------------------------
%% Verification funs generators
%%--------------------------------------------------------------------

-spec config_msg_verify_fun(RoomConfig :: [muc_light_helper:config_item()]) -> verify_fun().
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

-spec config_iq_verify_fun(RoomConfig :: [muc_light_helper:config_item()]) -> verify_fun().
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

-spec info_iq_verify_fun(AffUsers :: ct_aff_users(), Version :: binary(),
                         ConfigKV :: [muc_light_helper:config_item()]) -> verify_fun().
info_iq_verify_fun(AffUsers, Version, ConfigKV) ->
    BinAffUsers = bin_aff_users(AffUsers),
    fun(Incoming) ->
            [Query] = exml_query:subelements(Incoming, <<"query">>),
            ?NS_MUC_LIGHT_INFO = exml_query:attr(Query, <<"xmlns">>),
            Version = exml_query:path(Query, [{element, <<"version">>}, cdata]),
            UsersItems = exml_query:paths(Query, [{element, <<"occupants">>},
                                          {element, <<"user">>}]),
            verify_aff_users(UsersItems, BinAffUsers),
            ConfigurationEl = exml_query:subelement(Query, <<"configuration">>),
            verify_config(ConfigurationEl, ConfigKV)
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

-spec version_el(Version :: binary()) -> xmlel().
version_el(Version) ->
    #xmlel{ name = <<"version">>, children = [#xmlcdata{ content = Version }] }.

-spec assert_process_memory_not_growing(pid(), integer(), integer()) -> any().
assert_process_memory_not_growing(_, _, Counter) when Counter > 4 ->
    throw({memory_consumption_is_growing});
assert_process_memory_not_growing(_, _, Counter) when Counter == 0 ->
    ok;
assert_process_memory_not_growing(Pid, OldMemory, Counter) ->
    {memory, Memory} = rpc(erlang, process_info, [Pid, memory]),
    timer:sleep(1000),
    NewCounter = case Memory =< OldMemory of
                   true ->
                     Counter - 1;
                   _ ->
                     Counter + 1
                 end,
  assert_process_memory_not_growing(Pid, Memory, NewCounter).

assert_cache_hit_event(TS, Count, Jid) ->
    assert_cache_event(TS, Count, Jid, hits).

assert_cache_miss_event(TS, Count, Jid) ->
    assert_cache_event(TS, Count, Jid, misses).

assert_cache_event(TS, Count, BinJid, CacheResult) ->
    F = fun(#{jid := Jid, CacheResult := 1, latency := T}) ->
            eq_bjid(Jid, BinJid) andalso pos_int(T)
        end,
    instrument_helper:assert(
        user_cache_lookup,
        #{host_type => host_type(), cache_name => cache_name()},
        F,
        % Due to implementation, for every lookup the event is raised two times
        #{min_timestamp => TS, expected_count => Count * 2}
    ).

-spec custom_schema() -> [muc_light_helper:schema_item()].
custom_schema() ->
    % This list needs to be sorted
    [{<<"background">>, <<"builtin:hell">>, background, binary},
     {<<"music">>, <<"builtin:screams">>, music, binary}].
