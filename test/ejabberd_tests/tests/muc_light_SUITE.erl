-module(muc_light_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-import(escalus_ejabberd, [rpc/3]).

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
-type verify_fun() :: fun((Incoming :: #xmlel{}) -> any()).

-define(DEFAULT_AFF_USERS, [{Alice, owner}, {Bob, member}, {Kate, member}]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, entity},
     {group, occupant},
     {group, owner},
     {group, blocking}
    ].

groups() ->
    [
     {entity, [sequence], [
                            disco_service,
                            disco_features,
                            disco_rooms,
                            unauthorized_stanza
                         ]},
     {occupant, [sequence], [
                             send_message,
                             change_subject,
                             all_can_configure,
                             set_config_deny,
                             get_room_config,
                             get_room_occupants,
                             get_room_info,
                             leave_room,
                             change_other_aff_deny,
                             simple_mam
                            ]},
     {owner, [sequence], [
                          create_room,
                          create_room_unique,
                          create_room_with_equal_occupants,
                          create_existing_room_deny,
                          destroy_room,
                          set_config,
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
    Config1 = escalus:init_per_suite(Config),
    escalus:create_users(Config1, {by_name, [alice, bob, kate, mike]}).

end_per_suite(Config) ->
    clear_db(),
    Config1 = escalus:delete_users(Config, {by_name, [alice, bob, kate, mike]}),
    escalus:end_per_suite(Config1).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(mam_simple, Config) ->
    escalus:init_per_testcase(mam_simple, Config);
init_per_testcase(CaseName, Config) ->
    set_default_mod_config(),
    create_room(?ROOM, ?MUCHOST, alice, [bob, kate], Config, ver(1)),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    clear_db(),
    escalus:end_per_testcase(CaseName, Config).

%% ---------------------- Helpers ----------------------

create_room(RoomU, MUCHost, Owner, Members, Config, Version) ->
    DefaultConfig = default_config(),
    RoomUS = {RoomU, MUCHost},
    AffUsers = [{to_lus(Owner, Config), owner}
                | [ {to_lus(Member, Config), member} || Member <- Members ]],
    {ok, _RoomUS} = rpc(?BACKEND, create_room, [RoomUS, DefaultConfig, AffUsers, Version]).

% Currently not used
add_occupant(RoomU, MUCHost, User, Config, Version) ->
    UserLJID = to_lus(User, Config),
    RoomUS = {RoomU, MUCHost},
    NewAff = [{UserLJID, member}],
    {ok, _, _, _, _} = rpc(?BACKEND, modify_aff_users, [RoomUS, NewAff, ?CHECK_FUN, Version]).

clear_db() ->
    rpc(?BACKEND, force_clear, []).

%%--------------------------------------------------------------------
%% MUC light tests
%%--------------------------------------------------------------------

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

disco_rooms(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            {ok, {?ROOM2, ?MUCHOST}} = create_room(?ROOM2, ?MUCHOST, kate, [], Config, ver(0)),
            DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), ?MUCHOST),
            escalus:send(Alice, DiscoStanza),
            %% we should get 1 room, Alice is not in the second one
            Stanza = escalus:wait_for_stanza(Alice),
            [Item] = exml_query:paths(Stanza, [{element, <<"query">>}, {element, <<"item">>}]),
            ProperJID = room_bin_jid(?ROOM),
            ProperJID = exml_query:attr(Item, <<"jid">>),
            ProperVer = ver(1),
            ProperVer = exml_query:attr(Item, <<"version">>),
            escalus:assert(is_stanza_from, [?MUCHOST], Stanza)
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
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(?ROOM), Msg),
            foreach_occupant([Alice, Bob, Kate], Stanza, gc_message_verify_fun(?ROOM, Msg))
        end).

change_subject(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"subject">>, <<"new subject">>}],
            Stanza = stanza_config_set(?ROOM, ConfigChange),
            foreach_occupant([Alice, Bob, Kate], Stanza, config_msg_verify_fun(ConfigChange))
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

            escalus:send(Bob, stanza_config_get(?ROOM, ver(1))),
            IQRes = escalus:wait_for_stanza(Bob),
            escalus:assert(is_iq_result, IQRes),
            undefined = exml_query:subelement(IQRes, <<"query">>)
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
              end, {?DEFAULT_AFF_USERS, []}, [Alice, Bob, Kate])
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

simple_mam(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            escalus:send(Alice, stanza_create_room(?ROOM2, [], [])),
            verify_aff_bcast([{Alice, owner}], [{Alice, owner}]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

            MsgBody1 = <<"Message 1">>,
            Stanza1 = escalus_stanza:groupchat_to(room_bin_jid(?ROOM2), MsgBody1),
            foreach_occupant([Alice], Stanza1, gc_message_verify_fun(?ROOM2, MsgBody1)),
            MsgBody2 = <<"Message 2">>,
            Stanza2 = escalus_stanza:groupchat_to(room_bin_jid(?ROOM2), MsgBody2),
            foreach_occupant([Alice], Stanza2, gc_message_verify_fun(?ROOM2, MsgBody2)),
            escalus:send(Alice, stanza_aff_set(?ROOM2, [{Bob, member}])),
            verify_aff_bcast([{Alice, owner}, {Bob, member}], [{Bob, member}]),

            ArchReq = escalus_stanza:iq_get(?NS_MAM, []),
            escalus:send(Bob, escalus_stanza:to(ArchReq, room_bin_jid(?ROOM2))),
            [CreateEvent, Msg1, Msg2, BobAdd, IQRes] = escalus:wait_for_stanzas(Bob, 5),
            escalus:assert(is_iq_result, [ArchReq], IQRes),
            MsgBody1 = exml_query:path(extract_forwarded(Msg1), [{element, <<"body">>}, cdata]),
            MsgBody2 = exml_query:path(extract_forwarded(Msg2), [{element, <<"body">>}, cdata]),
            verify_archived_aff_msg(extract_forwarded(CreateEvent), [{Alice, owner}], true),
            verify_archived_aff_msg(extract_forwarded(BobAdd), [{Bob, member}], false)
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

set_config(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"roomname">>, <<"The Coven">>}],
            escalus:send(Alice, stanza_config_set(?ROOM, ConfigChange)),
            foreach_recipient([Alice, Bob, Kate], config_msg_verify_fun(ConfigChange)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
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
            escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                           escalus:wait_for_stanza(Bob)),

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
                           escalus:wait_for_stanza(Bob)),

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

-spec verify_archived_aff_msg(
        Stanza :: #xmlel{}, AffUsersChanges :: [{escalus:client(), binary()}],
        IsCreate :: boolean()) -> [].
verify_archived_aff_msg(Stanza, AffUsersChanges, IsCreate) ->
    BinAffUsersChanges = bin_aff_users(AffUsersChanges),
    [X] = exml_query:subelements(Stanza, <<"x">>),
    ?NS_MUC_LIGHT_AFFILIATIONS = exml_query:attr(X, <<"xmlns">>),
    undefined = exml_query:subelement(X, <<"prev-version">>),
    Version = exml_query:path(X, [{element, <<"version">>}, cdata]),
    true = IsCreate orelse is_binary(Version),
    Items = exml_query:subelements(X, <<"user">>),
    verify_aff_users(Items, BinAffUsersChanges).

-spec verify_keytake(Result :: {value, Item :: tuple(), Acc :: list()}, JID :: binary(),
                     Aff :: binary(), AffAcc :: list()) -> list().
verify_keytake({value, {_, Aff}, NewAffAcc}, _JID, Aff, _AffAcc) -> NewAffAcc.

%%--------------------------------------------------------------------
%% Generic iterators
%%--------------------------------------------------------------------

-spec foreach_occupant(
        Users :: [escalus:client()], Stanza :: #xmlel{}, VerifyFun :: verify_fun()) -> ok.
foreach_occupant(Users, Stanza, VerifyFun) ->
    lists:foreach(
      fun(Sender) ->
              escalus:send(Sender, Stanza),
              case exml_query:path(Stanza, [{attr, <<"type">>}]) of
                  <<"get">> ->
                      Incoming = escalus:wait_for_stanza(Sender),
                      escalus:assert(is_iq_result, Incoming),
                      VerifyFun(Incoming);
                  _ ->
                      foreach_recipient(Users, VerifyFun),
                      case Stanza of
                          #xmlel{ name = <<"iq">> } ->
                              escalus:assert(is_iq_result, escalus:wait_for_stanza(Sender));
                          _ ->
                              ok
                      end
              end
      end, Users).

-spec foreach_recipient(Users :: [escalus:client()], VerifyFun :: verify_fun()) -> ok.
foreach_recipient(Users, VerifyFun) ->
    lists:foreach(
      fun(Recipient) ->
              VerifyFun(escalus:wait_for_stanza(Recipient))
      end, Users).

%%--------------------------------------------------------------------
%% Verification funs generators
%%--------------------------------------------------------------------

-spec gc_message_verify_fun(Room :: binary(), MsgText :: binary()) -> verify_fun().
gc_message_verify_fun(Room, MsgText) ->
    fun(Incoming) ->
            escalus:assert(is_groupchat_message, [MsgText], Incoming),
            [RoomBareJID, FromNick] = binary:split(exml_query:attr(Incoming, <<"from">>), <<"/">>),
            [Room, ?MUCHOST] = binary:split(RoomBareJID, <<"@">>),
            [_] = binary:split(FromNick, <<"/">>) % nick is bare JID
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
room_bin_jid(Room) ->
    <<Room/binary, $@, (?MUCHOST)/binary>>.

-spec to_lus(Config :: list(), UserAtom :: atom()) -> {binary(), binary()}.
to_lus(UserAtom, Config) ->
    {lbin(escalus_users:get_username(Config, UserAtom)),
     lbin(escalus_users:get_server(Config, UserAtom))}.

-spec lbin(Bin :: binary()) -> binary().
lbin(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).

-spec default_config() -> list().
default_config() ->
    rpc(mod_muc_light, default_config, []).

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
       {max_occupants, infinity}
      ]).

-spec set_mod_config(K :: atom(), V :: any()) -> ok.
set_mod_config(K, V) ->
    rpc(mod_muc_light, set_service_opt, [K, V]).

-spec extract_forwarded(Stanza :: #xmlel{}) -> #xmlel{}.
extract_forwarded(Stanza) ->
    exml_query:path(Stanza, [{element, <<"result">>}, {element, <<"forwarded">>},
                             {element, <<"message">>}]).

