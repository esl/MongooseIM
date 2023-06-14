-module(muc_light_legacy_SUITE).

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-compile([export_all, nowarn_export_all]).

-import(escalus_ejabberd, [rpc/3]).
-import(muc_helper, [foreach_occupant/3, foreach_recipient/2]).
-import(distributed_helper, [subhost_pattern/1]).
-import(domain_helper, [host_type/0, domain/0]).
-import(muc_light_helper, [
                           bin_aff_users/1,
                           to_lus/2,
                           lbin/1,
                           create_room/6,
                           default_config/1,
                           default_schema/0
                          ]).
-import(config_parser_helper, [mod_config/2]).

-define(ROOM, <<"testroom">>).
-define(ROOM2, <<"testroom2">>).

-define(NS_MUC_LIGHT, <<"urn:xmpp:muclight:0">>).
-define(NS_MUC_ROOMCONFIG, <<"http://jabber.org/protocol/muc#roomconfig">>).

-define(MUCHOST, (muc_helper:muc_host())).

-type ct_aff_user() :: {EscalusClient :: escalus:client(), Aff :: atom()}.
-type ct_aff_users() :: [ct_aff_user()].
-type ct_block_item() :: {What :: atom(), Action :: atom(), Who :: binary()}.
-type verify_fun() :: muc_helper:verify_fun().
-type xmlel() :: exml:element().

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
                               disco_features_with_mam,
                               disco_info,
                               disco_info_with_mam,
                               disco_rooms,
                               disco_rooms_rsm,
                               unauthorized_stanza
                              ]},
         {occupant, [sequence], [
                                 send_message,
                                 change_subject,
                                 all_can_configure,
                                 set_config_deny,
                                 get_room_config,
                                 get_room_occupants,
                                 leave_room,
                                 change_other_aff_deny
                                ]},
         {owner, [sequence], [
                              create_room,
                              create_room_with_equal_occupants,
                              create_existing_room_deny,
                              destroy_room,
                              set_config,
                              set_config_errors,
                              assorted_config_doesnt_lead_to_duplication,
                              remove_and_add_users,
                              explicit_owner_change,
                              implicit_owner_change,
                              edge_case_owner_change
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
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    Config2 = escalus:init_per_suite(Config1),
    escalus:create_users(Config2, escalus:get_users([alice, bob, kate, mike])).

end_per_suite(Config) ->
    clear_db(),
    escalus_fresh:clean(),
    Config1 = escalus:delete_users(Config, escalus:get_users([alice, bob, kate, mike])),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config1).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) when CaseName =:= disco_rooms_rsm;
                                         CaseName =:= block_room;
                                         CaseName =:= block_user ->
    dynamic_modules:ensure_modules(host_type(), required_modules(CaseName)),
    create_room(?ROOM, ?MUCHOST, alice, [bob, kate], Config),
    create_room(?ROOM2, ?MUCHOST, alice, [kate], Config),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(create_existing_room_deny = CaseName, Config) ->
    dynamic_modules:ensure_modules(host_type(), required_modules(CaseName)),
    create_room(?ROOM, ?MUCHOST, alice, [], Config),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CaseName, Config) when CaseName =:= disco_features_with_mam;
                                         CaseName =:= disco_info_with_mam ->
    case mam_helper:backend() of
        disabled ->
            {skip, "No MAM backend available"};
        Backend ->
            dynamic_modules:ensure_modules(host_type(), required_modules(CaseName, Backend)),
            create_room(?ROOM, ?MUCHOST, alice, [bob, kate], Config),
            escalus:init_per_testcase(CaseName, Config)
    end;
init_per_testcase(CaseName, Config) ->
    dynamic_modules:ensure_modules(host_type(), required_modules(CaseName)),
    create_room(?ROOM, ?MUCHOST, alice, [bob, kate], Config),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    clear_db(),
    escalus:end_per_testcase(CaseName, Config).

%% Module configuration per test case

required_modules(CaseName, MAMBackend) ->
    [{mod_mam, mam_helper:config_opts(#{backend => MAMBackend,
                                        muc => #{host => subhost_pattern(?MUCHOST)}})} |
     common_required_modules(CaseName)].

required_modules(CaseName) ->
    [{mod_mam, stopped} | common_required_modules(CaseName)].

common_required_modules(CaseName) ->
    Opts = maps:merge(common_muc_light_opts(), muc_light_opts(CaseName)),
    [{mod_muc_light, mod_config(mod_muc_light, Opts)}].

muc_light_opts(disco_rooms_rsm) ->
    #{rooms_per_page => 1};
muc_light_opts(all_can_configure) ->
    #{all_can_configure => true};
muc_light_opts(create_room_with_equal_occupants) ->
    #{equal_occupants => true};
muc_light_opts(block_user) ->
    #{all_can_invite => true};
muc_light_opts(blocking_disabled) ->
    #{blocking => false};
muc_light_opts(_) ->
    #{}.

common_muc_light_opts() ->
    #{host => subhost_pattern(muc_helper:muc_host_pattern()),
      backend => mongoose_helper:mnesia_or_rdbms_backend(),
      legacy_mode => true}.

%% ---------------------- Helpers ----------------------

create_room(RoomU, MUCHost, Owner, Members, Config) ->
    create_room(RoomU, MUCHost, Owner, Members, Config, <<"-">>).

clear_db() ->
    muc_light_helper:clear_db(host_type()).

%%--------------------------------------------------------------------
%% MUC light tests
%%--------------------------------------------------------------------

%% ---------------------- Disco ----------------------

disco_service(Config) ->
    muc_helper:disco_service_story(Config).

disco_features(Config) ->
    muc_helper:disco_features_story(Config, [?NS_MUC]).

disco_features_with_mam(Config) ->
    muc_helper:disco_features_story(Config, [?NS_MUC] ++ mam_helper:namespaces()).

disco_info(Config) ->
    muc_helper:disco_info_story(Config, [?NS_MUC]).

disco_info_with_mam(Config) ->
    muc_helper:disco_info_story(Config, [?NS_MUC] ++ mam_helper:namespaces()).

disco_rooms(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            {ok, {?ROOM2, _}} = create_room(?ROOM2, ?MUCHOST, kate, [], Config),
            DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), ?MUCHOST),
            escalus:send(Alice, DiscoStanza),
            %% we should get 1 room, Alice is not in the second one
            Stanza = escalus:wait_for_stanza(Alice),
            [Item] = exml_query:paths(Stanza, [{element, <<"query">>}, {element, <<"item">>}]),
            ProperJID = room_bin_jid(?ROOM),
            ProperJID = exml_query:attr(Item, <<"jid">>),
            escalus:assert(is_stanza_from, [?MUCHOST], Stanza)
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
                               children = [#xmlcdata{ content = <<"oops@", (?MUCHOST)/binary>> }] },
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

unauthorized_stanza(Config) ->
    escalus:story(Config, [{alice, 1}, {kate, 1}], fun(Alice, Kate) ->
            {ok, {?ROOM2, _}} = create_room(?ROOM2, ?MUCHOST, kate, [], Config),
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
            Id = <<"LegacyId">>,
            Stanza = escalus_stanza:set_id(
                       escalus_stanza:groupchat_to(room_bin_jid(?ROOM), Msg), Id),
            foreach_occupant([Alice, Bob, Kate], Stanza, gc_message_verify_fun(?ROOM, Msg, Id))
        end).

change_subject(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            Subject = <<"new subject">>,
            SubjectStanza = #xmlel{name = <<"message">>,
                                   attrs = [{<<"type">>, <<"groupchat">>}],
                                   children = [#xmlel{
                                                  name = <<"subject">>,
                                                  children = [#xmlcdata{content = Subject}]
                                                 }]
                                  },
            foreach_occupant([Alice, Bob, Kate],
                             escalus_stanza:to(SubjectStanza, room_bin_jid(?ROOM)),
                             subject_message_verify_fun(?ROOM, Subject))
        end).

all_can_configure(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"roomname">>, <<"new subject">>}],
            Stanza = stanza_config_set(?ROOM, ConfigChange),
            foreach_occupant([Alice, Bob, Kate], Stanza, config_msg_verify_fun())
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
            Stanza = stanza_config_get(?ROOM),
            ConfigKV = default_config(default_schema()),
            foreach_occupant([Alice, Bob, Kate], Stanza, config_iq_verify_fun(ConfigKV))
        end).

get_room_occupants(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsers = [{Alice, owner}, {Bob, member}, {Kate, member}],
            foreach_occupant([Alice, Bob, Kate], stanza_aff_get(?ROOM), aff_iq_verify_fun(AffUsers))
        end).

leave_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            % Users will leave one by one, owner last
            lists:foldr(
              fun(User, {Occupants, Outsiders}) ->
                      NewOccupants = lists:keydelete(User, 1, Occupants),
                      user_leave(User, [ U || {U, _} <- NewOccupants]),
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

            AffUsersChanges3 = [{Mike, member}],
            escalus:send(Kate, stanza_aff_set(?ROOM, AffUsersChanges3)),
            escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>],
                           escalus:wait_for_stanza(Kate)),

            verify_no_stanzas([Alice, Bob, Kate, Mike])
        end).

%% ---------------------- owner ----------------------

create_room(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
            escalus:send(Bob, stanza_create_room(<<"bobroom">>, Bob)),
            Result = escalus:wait_for_stanza(Bob),
            presence_verify(Bob, owner, Result)
        end).

create_room_with_equal_occupants(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
            escalus:send(Bob, stanza_create_room(<<"bobroom">>, Bob)),
            Result = escalus:wait_for_stanza(Bob),
            presence_verify(Bob, member, Result)
        end).

create_existing_room_deny(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
            escalus:send(Bob, stanza_create_room(?ROOM, Bob)),
            Result = escalus:wait_for_stanza(Bob),
            escalus:assert(is_presence_with_type, [<<"error">>], Result),
            escalus:assert(is_error, [<<"auth">>, <<"registration-required">>], Result),
            X = exml_query:subelement(Result, <<"x">>),
            ?NS_MUC = exml_query:attr(X, <<"xmlns">>)
        end).

destroy_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            escalus:send(Alice, stanza_destroy_room(?ROOM)),
            AffUsersChanges = [{Alice, none}, {Bob, none}, {Kate, none}],
            verify_aff_bcast([], AffUsersChanges, [], Alice),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

set_config(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"roomname">>, <<"The Coven">>}],
            escalus:send(Alice, stanza_config_set(?ROOM, ConfigChange)),
            foreach_recipient([Alice, Bob, Kate], config_msg_verify_fun()),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            Stanza = stanza_config_get(?ROOM),
            foreach_occupant([Alice, Bob, Kate], Stanza, config_iq_verify_fun(ConfigChange))
        end).

set_config_errors(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Alice) ->
              ConfigChange = [{<<"roomname">>, <<"The Coven">>}],
              Req = stanza_config_set(?ROOM, ConfigChange),
              escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                             escalus:send_and_wait(Alice, form_helper:remove_form_types(Req))),
              escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                             escalus:send_and_wait(Alice, form_helper:remove_form_ns(Req))),
              escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                             escalus:send_and_wait(Alice, form_helper:remove_forms(Req)))
      end).

assorted_config_doesnt_lead_to_duplication(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"subject">>, <<"Elixirs">>},
                            {<<"roomname">>, <<"The Coven">>},
                            {<<"subject">>, <<"Elixirs">>}],
            escalus:send(Alice, stanza_config_set(?ROOM, ConfigChange)),
            foreach_recipient([Alice, Bob, Kate], config_msg_verify_fun()),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

            Stanza = stanza_config_get(?ROOM),
            VerifyFun = fun(Incoming) ->
                                Fields = exml_query:paths(Incoming, [{element, <<"query">>},
                                                                     {element, <<"x">>},
                                                                     {element, <<"field">>}]),
                                ConfigKV = [{exml_query:attr(F, <<"var">>),
                                             exml_query:path(F, [{element, <<"value">>}, cdata])}
                                            || F <- Fields],
                                Length = length(ConfigKV),
                                Length = length(lists:ukeysort(1, ConfigKV))
                        end,
            foreach_occupant([Alice, Bob, Kate], Stanza, VerifyFun)
         end).

remove_and_add_users(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsersChanges1 = [{Bob, none}, {Kate, none}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges1)),
            verify_aff_bcast([Alice], AffUsersChanges1, [], Alice),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            AffUsersChanges2 = [{Bob, member}, {Kate, member}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges2)),
            verify_aff_bcast([Alice, Bob, Kate], AffUsersChanges2, [Bob, Kate], Alice),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

explicit_owner_change(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsersChanges1 = [{Bob, none}, {Alice, none}, {Kate, owner}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges1)),
            verify_aff_bcast([Kate], AffUsersChanges1, [], Alice),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

implicit_owner_change(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsersChanges1 = [{Bob, none}, {Alice, member}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges1)),
            verify_aff_bcast([Kate, Alice], [{Kate, owner} | AffUsersChanges1], [], Alice),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

edge_case_owner_change(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsersChanges1 = [{Alice, member}, {Bob, none}, {Kate, none}],
            escalus:send(Alice, stanza_aff_set(?ROOM, AffUsersChanges1)),
            verify_aff_bcast([Alice], [{Kate, none}, {Bob, none}], [], Alice),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
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
            user_leave(Bob, [Alice, Kate]),

            % Alice tries to readd Bob to the room but fails
            BobReadd = [{Bob, member}],
            escalus:send(Alice, stanza_aff_set(?ROOM, BobReadd)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            verify_no_stanzas([Alice, Bob, Kate]),

            % But Alice can add Bob to another room!
            escalus:send(Alice, stanza_aff_set(?ROOM2, BobReadd)),
            verify_aff_bcast([Alice, Bob, Kate], BobReadd, [Bob], Alice),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

block_user(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AliceJIDBin = lbin(escalus_client:short_jid(Alice)),
            BlocklistChange = [{user, deny, AliceJIDBin}],
            escalus:send(Bob, stanza_blocking_set(BlocklistChange)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

            % Alice tries to add Bob to the room but fails
            BobAdd = [{Bob, member}],
            escalus:send(Alice, stanza_aff_set(?ROOM2, BobAdd)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            verify_no_stanzas([Alice, Bob, Kate]),

            % But Kate can add Bob to the room!
            escalus:send(Kate, stanza_aff_set(?ROOM2, BobAdd)),
            verify_aff_bcast([Alice, Bob, Kate], BobAdd, [Bob], Kate),
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

-spec user_leave(User :: escalus:client(), RemainingOccupants :: [escalus:client()]) -> ok.
user_leave(User, RemainingOccupants) ->
    AffUsersChanges = [{User, none}],
    Stanza = stanza_aff_set(?ROOM, AffUsersChanges),
    escalus:send(User, Stanza),
    verify_aff_bcast(RemainingOccupants, AffUsersChanges, [], User),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(User)).

%%--------------------------------------------------------------------
%% IQ getters
%%--------------------------------------------------------------------

-spec stanza_blocking_get() -> xmlel().
stanza_blocking_get() ->
    escalus_stanza:to(
      escalus_stanza:privacy_get_lists([?NS_MUC_LIGHT]), ?MUCHOST).

-spec stanza_config_get(Room :: binary()) -> xmlel().
stanza_config_get(Room) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_OWNER, []), room_bin_jid(Room)).

-spec stanza_aff_get(Room :: binary()) -> xmlel().
stanza_aff_get(Room) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_ADMIN, []), room_bin_jid(Room)).

%%--------------------------------------------------------------------
%% IQ setters
%%--------------------------------------------------------------------

-spec stanza_blocking_set(BlocklistChanges :: [ct_block_item()]) -> xmlel().
stanza_blocking_set(BlocklistChanges) ->
    Items = [ encode_privacy_item(What, Action, Who) || {What, Action, Who} <- BlocklistChanges ],
    Stanza = escalus_stanza:privacy_set_list(escalus_stanza:privacy_list(?NS_MUC_LIGHT, Items)),
    escalus_stanza:to(Stanza, ?MUCHOST).

encode_privacy_item(What, Action, Who) ->
    Value = case What of
                room -> Who;
                user -> <<(?MUCHOST)/binary, $/, Who/binary>>
            end,
    ActionBin = atom_to_binary(Action, utf8),
    escalus_stanza:privacy_list_item(<<"1">>, ActionBin, <<"jid">>, Value, []).

-spec stanza_create_room(RoomNode :: binary(), Creator :: escalus:client()) -> xmlel().
stanza_create_room(RoomNode, Creator) ->
    ToBinJID = <<(room_bin_jid(RoomNode))/binary, $/,
                 (lbin(escalus_client:short_jid(Creator)))/binary>>,
    X = #xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC}] },
    escalus_stanza:to(escalus_stanza:presence(<<"available">>, [X]), ToBinJID).

-spec stanza_destroy_room(Room :: binary()) -> xmlel().
stanza_destroy_room(Room) ->
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_OWNER, [#xmlel{ name = <<"destroy">> }]),
                      room_bin_jid(Room)).

-spec stanza_config_set(Room :: binary(), ConfigChanges :: [muc_light_helper:config_item()]) -> xmlel().
stanza_config_set(Room, ConfigChanges) ->
    IQ = escalus_stanza:iq_set(?NS_MUC_OWNER, [form_x_el(ConfigChanges)]),
    escalus_stanza:to(IQ, room_bin_jid(Room)).

-spec form_x_el(Fields :: [map()]) -> xmlel().
form_x_el(Fields) ->
    FieldSpecs = [#{var => Var, values => [Value], type => <<"text-single">>}
                  || {Var, Value} <- Fields],
    form_helper:form(#{ns => ?NS_MUC_ROOMCONFIG, fields => FieldSpecs}).

-spec stanza_aff_set(Room :: binary(), AffUsers :: ct_aff_users()) -> xmlel().
stanza_aff_set(Room, AffUsers) ->
    Items = [#xmlel{ name = <<"item">>, attrs = [{<<"affiliation">>, AffBin},
                                                 {<<"jid">>, UserBin}] }
             || {UserBin, AffBin} <- bin_aff_users(AffUsers)],
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_ADMIN, Items), room_bin_jid(Room)).

%%--------------------------------------------------------------------
%% Verifiers
%%--------------------------------------------------------------------

-spec verify_blocklist(Query :: xmlel(), ProperBlocklist :: [ct_block_item()]) -> [].
verify_blocklist(Query, ProperBlocklist) ->
    ?NS_PRIVACY = exml_query:attr(Query, <<"xmlns">>),
    RawItems = exml_query:paths(Query, [{element, <<"list">>}, {element, <<"item">>}]),
    Blocklist = [ parse_blocked_item(Item) || Item <- RawItems ],
    ProperBlocklistLen = length(ProperBlocklist),
    ProperBlocklistLen = length(Blocklist),
    [] = lists:foldl(fun lists:delete/2, Blocklist, ProperBlocklist).

-spec parse_blocked_item(Item :: xmlel()) -> ct_block_item().
parse_blocked_item(Item) ->
    <<"deny">> = exml_query:attr(Item, <<"action">>),
    <<"jid">> = exml_query:attr(Item, <<"type">>),
    Value = exml_query:attr(Item, <<"value">>),
    MucHost = ?MUCHOST,
    case binary:split(Value, <<"/">>) of
        [MucHost, User] -> {user, deny, User};
        [Room] -> {room, deny, Room};
        Other ->
            CfgHost = rpc(gen_mod, get_module_opt, [host_type(), mod_muc_light, host, undefined]),
            ct:fail(#{what => parse_blocked_item_failed,
                      muc_host => MucHost, other => Other,
                      cfg_host => CfgHost})
    end.

-spec verify_aff_bcast(CurrentOccupants :: [escalus:client()], AffUsersChanges :: ct_aff_users(),
                       Newcomers :: [escalus:client()], Changer :: escalus:client()) -> ok.
verify_aff_bcast(CurrentOccupants, AffUsersChanges, Newcomers, Changer) ->
    MucHost = ?MUCHOST,
    PredList = [ presence_verify_fun(AffUser) || AffUser <- AffUsersChanges ],
    lists:foreach(
      fun(Occupant) ->
              case lists:member(Occupant, Newcomers) of
                  false ->
                      Stanzas = escalus:wait_for_stanzas(Occupant, length(PredList)),
                      escalus_new_assert:mix_match(PredList, Stanzas);
                  true ->
                      ok
              end
      end, CurrentOccupants),
    lists:foreach(
      fun(Newcomer) ->
              #xmlel{ name = <<"message">> } = Incoming = escalus:wait_for_stanza(Newcomer),
              RoomBareJIDBin = exml_query:attr(Incoming, <<"from">>),
              [_, MucHost] = binary:split(RoomBareJIDBin, <<"@">>),
              X = exml_query:subelement(Incoming, <<"x">>),
              ?NS_MUC_USER = exml_query:attr(X, <<"xmlns">>),
              [Invite] = exml_query:subelements(X, <<"invite">>),
              ChangerBareJIDBin = lbin(escalus_client:short_jid(Changer)),
              ChangerBareJIDBin = exml_query:attr(Invite, <<"from">>)
      end, Newcomers),
    lists:foreach(
      fun({Leaver, none}) ->
              presence_verify(Leaver, none, escalus:wait_for_stanza(Leaver));
         (_) ->
              ignore
      end, AffUsersChanges).

-spec verify_no_stanzas(Users :: [escalus:client()]) -> ok.
verify_no_stanzas(Users) ->
    lists:foreach(
      fun(User) ->
              {false, _} = {escalus_client:has_stanzas(User), User}
      end, Users).

-spec verify_config(ConfigFields :: [xmlel()], Config :: [muc_light_helper:config_item()]) -> ok.
verify_config(ConfigFields, Config) ->
    [] = lists:foldl(
           fun(Field, ConfigAcc) ->
                   Key = exml_query:attr(Field, <<"var">>),
                   Val = exml_query:path(Field, [{element, <<"value">>}, cdata]),
                   case lists:keytake(Key, 1, ConfigAcc) of
                       {value, {_, ProperVal}, NewConfig} when Val =:= ProperVal -> NewConfig;
                       false -> ConfigAcc
                   end
           end, Config, ConfigFields).

-spec verify_aff_users(Items :: [xmlel()], BinAffUsers :: [{binary(), binary()}]) -> [].
verify_aff_users(Items, BinAffUsers) ->
    true = (length(Items) == length(BinAffUsers)),
    [] = lists:foldl(
           fun(Item, AffAcc) ->
                   JID = exml_query:attr(Item, <<"jid">>),
                   JID = exml_query:attr(Item, <<"nick">>),
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
    MucHost = ?MUCHOST,
    fun(Incoming) ->
            escalus:assert(is_groupchat_message, [MsgText], Incoming),
            [RoomBareJID, FromNick] = binary:split(exml_query:attr(Incoming, <<"from">>), <<"/">>),
            [Room, MucHost] = binary:split(RoomBareJID, <<"@">>),
            [_] = binary:split(FromNick, <<"/">>), % nick is bare JID
            Id = exml_query:attr(Incoming, <<"id">>)
    end.

-spec subject_message_verify_fun(Room :: binary(), Subject :: binary()) -> verify_fun().
subject_message_verify_fun(Room, Subject) ->
    MucHost = ?MUCHOST,
    fun(Incoming) ->
            escalus:assert(is_groupchat_message, Incoming),
            Subject = exml_query:path(Incoming, [{element, <<"subject">>}, cdata]),
            RoomBareJID = exml_query:attr(Incoming, <<"from">>),
            [Room, MucHost] = binary:split(RoomBareJID, <<"@">>)
    end.

-spec config_msg_verify_fun() -> verify_fun().
config_msg_verify_fun() ->
    fun(Incoming) ->
            escalus:assert(is_groupchat_message, Incoming),
            [X] = exml_query:subelements(Incoming, <<"x">>),
            ?NS_MUC_USER = exml_query:attr(X, <<"xmlns">>),
            <<"104">> = exml_query:path(X, [{element, <<"status">>}, {attr, <<"code">>}])
    end.

-spec config_iq_verify_fun(RoomConfig :: [muc_light_helper:config_item()]) -> verify_fun().
config_iq_verify_fun(RoomConfig) ->
    fun(Incoming) ->
            Fields = exml_query:paths(Incoming, [{element, <<"query">>}, {element, <<"x">>},
                                                  {element, <<"field">>}]),
            ?NS_MUC_OWNER = exml_query:path(Incoming, [{element, <<"query">>},
                                                       {attr, <<"xmlns">>}]),
            verify_config(Fields, RoomConfig)
    end.

-spec aff_iq_verify_fun(AffUsers :: ct_aff_users()) -> verify_fun().
aff_iq_verify_fun(AffUsers) ->
    BinAffUsers = bin_aff_users(AffUsers),
    fun(Incoming) ->
            [Query] = exml_query:subelements(Incoming, <<"query">>),
            ?NS_MUC_ADMIN = exml_query:attr(Query, <<"xmlns">>),
            Items = exml_query:subelements(Query, <<"item">>),
            verify_aff_users(Items, BinAffUsers)
    end.

-spec presence_verify_fun(AffUser :: ct_aff_user()) -> verify_fun().
presence_verify_fun({User, UserAff}) ->
    fun(Incoming) ->
            true == (catch presence_verify(User, UserAff, Incoming))
    end.

-spec presence_verify(User :: escalus:client(), UserAff :: none | member | owner,
                      Incoming :: xmlel()) -> true.
presence_verify(User, UserAff, #xmlel{ name = <<"presence">> } = Incoming) ->
    MucHost = ?MUCHOST,
    UserJIDBin = lbin(escalus_client:short_jid(User)),
    [RoomBareJIDBin, UserJIDBin] = binary:split(exml_query:attr(Incoming, <<"from">>), <<"/">>),
    [_, MucHost] = binary:split(RoomBareJIDBin, <<"@">>),
    X = exml_query:subelement(Incoming, <<"x">>),
    HasDestroy = exml_query:subelement(X, <<"destroy">>) =/= undefined,
    {ProperAff, ProperRole}
    = case {UserAff, exml_query:attr(Incoming, <<"type">>)} of
          {none, _} ->
              <<"unavailable">> = exml_query:attr(Incoming, <<"type">>),
              case HasDestroy of
                  false ->
                      <<"321">> = exml_query:path(X, [{element, <<"status">>}, {attr, <<"code">>}]);
                  true ->
                      ok
              end,
              {<<"none">>, <<"none">>};
          {_, Type} when Type =/= <<"unavailable">> ->
              case UserAff of
                  member -> {<<"member">>, <<"participant">>};
                  owner -> {<<"owner">>, <<"moderator">>}
              end
      end,
    ?NS_MUC_USER = exml_query:attr(X, <<"xmlns">>),
    [Item] = exml_query:subelements(X, <<"item">>),
    ProperAff = exml_query:attr(Item, <<"affiliation">>),
    ProperRole = exml_query:attr(Item, <<"role">>),
    case exml_query:subelements(X, <<"status">>) of
        [_, _] -> % room create request
            [<<"110">>, <<"201">>]
            = lists:sort(exml_query:paths(X, [{element, <<"status">>}, {attr, <<"code">>}]));
        _ ->
            case HasDestroy of
                false -> UserJIDBin = exml_query:attr(Item, <<"jid">>);
                true -> ok
            end
    end,
    true.

%%--------------------------------------------------------------------
%% Other helpers
%%--------------------------------------------------------------------

-spec room_bin_jid(Room :: binary()) -> binary().
room_bin_jid(Room) ->
    <<Room/binary, $@, (muc_helper:muc_host())/binary>>.
