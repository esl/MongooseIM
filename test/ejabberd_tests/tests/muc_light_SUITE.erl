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

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, disco},
     {group, occupant},
     {group, owner},
     {group, blocking}
    ].

groups() ->
    [
     {disco, [sequence], [
                            disco_service,
                            disco_features,
                            disco_rooms
                         ]},
     {occupant, [sequence], [
                             send_message,
                             change_subject,
                             get_room_config,
                             get_room_occupants,
                             get_room_info,
                             leave_room
                            ]},
     {owner, [sequence], [
                          create_room,
                          destroy_room,
                          set_config,
                          remove_and_add_users
                         ]},
     {blocking, [sequence], [
                             manage_blocklist,
                             block_room,
                             block_user
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
    clear_db(),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob, kate]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob, kate]}).

init_per_testcase(CaseName, Config) ->
    set_default_mod_config(),
    create_room(?ROOM, ?MUCHOST, alice, Config, ver(-1)),
    add_occupant(?ROOM, ?MUCHOST, bob, Config, ver(0)),
    add_occupant(?ROOM, ?MUCHOST, kate, Config, ver(1)),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    clear_db(),
    escalus:end_per_testcase(CaseName, Config).

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
            {?ROOM2, ?MUCHOST} = create_room(?ROOM2, ?MUCHOST, kate, Config, ver(0)),
            DiscoStanza = escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), ?MUCHOST),
            escalus:send(Alice, DiscoStanza),
            %% we should get 1 room, Alice is not in the second one
            Stanza = escalus:wait_for_stanza(Alice),
            [Item] = exml_query:paths(Stanza, [{element, <<"query">>}, {element, <<"item">>}]),
            ProperJID = room_bin_jid(),
            ProperJID = exml_query:path(Item, [{attr, <<"jid">>}]),
            ProperVer = ver(1),
            ProperVer = exml_query:path(Item, [{attr, <<"version">>}]),
            escalus:assert(is_stanza_from, [?MUCHOST], Stanza)
        end).

%% ---------------------- Occupant ----------------------

send_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            Msg = <<"Heyah!">>,
            Stanza = escalus_stanza:groupchat_to(room_bin_jid(), Msg),
            foreach_occupant([Alice, Bob, Kate], Stanza,
                            fun(Incoming) ->
                                    escalus:assert(is_groupchat_message, [Msg], Incoming)
                            end)
        end).

change_subject(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"subject">>, <<"new subject">>}],
            Stanza = stanza_config_set(ConfigChange),
            foreach_occupant([Alice, Bob, Kate], Stanza, config_msg_verify_fun(ConfigChange))
        end).

get_room_config(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            Stanza = stanza_config_get(<<"oldver">>),
            ConfigKV = [{version, ver(1)} | default_config()],
            ConfigKVBin = [{list_to_binary(atom_to_list(Key)), Val} || {Key, Val} <- ConfigKV],
            foreach_occupant([Alice, Bob, Kate], Stanza, config_iq_verify_fun(ConfigKVBin))
        end).
            
get_room_occupants(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            foreach_occupant([Alice, Bob, Kate], stanza_aff_get(<<"oldver">>),
                             aff_iq_verify_fun(Config, default_aff_users(), ver(1)))
        end).

get_room_info(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            Stanza = stanza_info_get(<<"oldver">>),
            ConfigKV = default_config(),
            ConfigKVBin = [{list_to_binary(atom_to_list(Key)), Val} || {Key, Val} <- ConfigKV],
            foreach_occupant([Alice, Bob, Kate], Stanza,
                             info_iq_verify_fun(Config, default_aff_users(), ver(1), ConfigKVBin))
        end).

leave_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            InitOccupants = [
                             {Alice, alice},
                             {Bob, bob},
                             {Kate, kate}
                            ],
            lists:foldr(
              fun(User, {Occupants, Outsiders}) ->
                      {value, {_, UserAtom}, NewOccupants} = lists:keytake(User, 1, Occupants),
                      user_leave(User, UserAtom, NewOccupants, Config),
                      no_stanzas(Outsiders),
                      {NewOccupants, [User | Outsiders]}
              end, {InitOccupants, []}, [Alice, Bob, Kate])
        end).

%% ---------------------- owner ----------------------

create_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            InitOccupants = [
                             {Alice, alice, member},
                             {Kate, kate, member}
                            ],
            InitConfig = [{<<"roomname">>, <<"Bob's room">>}],
            RoomNode = <<"bobroom">>,
            escalus:send(Bob, stanza_create_room(Config, RoomNode, InitConfig, InitOccupants)),
            verify_aff_bcast(
              Config, [{Bob, owner}, {Alice, member}, {Kate, member}],
              [{Bob, bob, owner} | InitOccupants]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob))
        end).

destroy_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            escalus:send(Alice, stanza_destroy_room()),
            verify_aff_bcast(Config, [], [{Bob, bob, none},
                                          {Alice, alice, none},
                                          {Kate, kate, none}], [?NS_MUC_LIGHT_DESTROY]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).


set_config(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            ConfigChange = [{<<"roomname">>, <<"The Coven">>}],
            escalus:send(Alice, stanza_config_set(ConfigChange)),
            foreach_recipient([Alice, Bob, Kate], config_msg_verify_fun(ConfigChange)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

remove_and_add_users(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AffUsersChanges1 = [{Bob, bob, none},
                                {Kate, kate, none}],
            escalus:send(Alice, stanza_aff_set(Config, AffUsersChanges1)),
            verify_aff_bcast(Config, [{Alice, alice}], AffUsersChanges1),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            AffUsersChanges2 = [{Bob, bob, member},
                                {Kate, kate, member}],
            escalus:send(Alice, stanza_aff_set(Config, AffUsersChanges2)),
            verify_aff_bcast(Config, [{Alice, alice}, {Bob, bob}, {Kate, kate}], AffUsersChanges2),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).


%% ---------------------- blocking ----------------------

manage_blocklist(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
            escalus:send(Alice, stanza_blocking_get()),
            GetResult1 = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, GetResult1),
            [QueryEl1] = exml_query:paths(GetResult1, [{element, <<"query">>}]),
            verify_blocklist(QueryEl1, []),
            
            BlocklistChange1 = [
                                {user, deny, <<"user@localhost">>},
                                {room, deny, room_bin_jid()}
                               ],
            escalus:send(Alice, stanza_blocking_set(BlocklistChange1)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            escalus:send(Alice, stanza_blocking_get()),
            GetResult2 = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, GetResult2),
            [QueryEl2] = exml_query:paths(GetResult2, [{element, <<"query">>}]),
            verify_blocklist(QueryEl2, BlocklistChange1),
            
            BlocklistChange2 = [
                                {user, allow, <<"user@localhost">>},
                                {room, allow, room_bin_jid()}
                               ],
            escalus:send(Alice, stanza_blocking_set(BlocklistChange2)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            escalus:send(Alice, stanza_blocking_get()),
            GetResult3 = escalus:wait_for_stanza(Alice),
            escalus:assert(is_iq_result, GetResult3),
            % Match below checks for empty list
            [QueryEl1] = exml_query:paths(GetResult3, [{element, <<"query">>}])
        end).

block_room(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            BlocklistChange = [{room, deny, room_bin_jid()}],
            escalus:send(Bob, stanza_blocking_set(BlocklistChange)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
            user_leave(Bob, bob, [{Alice, alice}, {Kate, kate}], Config),

            % Alice tries to readd Bob to the room but fails
            BobReadd = [{Bob, bob, member}],
            FailStanza = stanza_aff_set(Config, BobReadd),
            escalus:send(Alice, FailStanza),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            no_stanzas([Alice, Bob, Kate]),

            % But Alice can add Bob to another room!
            InitOccupants = [{Bob, bob, member}],
            escalus:send(Alice, stanza_create_room(Config, <<"newroom">>, [], InitOccupants)),
            verify_aff_bcast(Config, [{Alice, owner}, {Bob, member}],
                             [{Alice, alice, owner} | InitOccupants]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
        end).

block_user(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
            AliceJIDBin = binary_to_lower(escalus_users:get_jid(Config, alice)),
            BlocklistChange = [{user, deny, AliceJIDBin}],
            escalus:send(Bob, stanza_blocking_set(BlocklistChange)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
            user_leave(Bob, bob, [{Alice, alice}, {Kate, kate}], Config),
            
            % Alice tries to create new room with Bob but Bob is not added
            escalus:send(Alice, stanza_create_room(Config, <<"new">>, [], [{Bob, bob, member}])),
            verify_aff_bcast(Config, [{Alice, owner}], [{Alice, alice, owner}]),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            no_stanzas([Alice, Bob, Kate]),

            % But Kate can add Bob to the main room!
            set_mod_config(all_can_invite, true),
            BobReadd = [{Bob, bob, member}],
            SuccessStanza = stanza_aff_set(Config, BobReadd),
            escalus:send(Kate, SuccessStanza),
            verify_aff_bcast(
              Config, [{Alice, owner}, {Bob, member}, {Kate, member}], BobReadd),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Kate)),
            no_stanzas([Alice, Bob, Kate])

        end).

%%--------------------------------------------------------------------
%% Blocklist manipulation
%%--------------------------------------------------------------------

stanza_blocking_get() ->
    escalus_stanza:to(escalus_stanza:iq_get(?NS_MUC_LIGHT_BLOCKING, []), ?MUCHOST).

stanza_blocking_set(BlocklistChanges) ->
    Items = [#xmlel{ name = list_to_binary(atom_to_list(What)),
                     attrs = [{<<"action">>, list_to_binary(atom_to_list(Action))}],
                     children = [#xmlcdata{ content = Who }] }
             || {What, Action, Who} <- BlocklistChanges],
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_BLOCKING, Items), ?MUCHOST).

verify_blocklist(Query, ProperBlocklist) ->
    ?NS_MUC_LIGHT_BLOCKING = exml_query:path(Query, [{attr, <<"xmlns">>}]),
    BlockedRooms = exml_query:paths(Query, [{element, <<"room">>}]),
    BlockedUsers = exml_query:paths(Query, [{element, <<"user">>}]),
    BlockedItems = [{list_to_atom(binary_to_list(What)),
                     list_to_atom(binary_to_list(Action)),
                     Who} || #xmlel{name = What,
                                    attrs = [{<<"action">>, Action}],
                                    children = [#xmlcdata{ content = Who }]}
                             <- BlockedRooms ++ BlockedUsers],
    true = (length(BlockedItems) == length(ProperBlocklist)),
    [] = lists:foldl(fun lists:delete/2, BlockedItems, ProperBlocklist).

%%--------------------------------------------------------------------
%% Room configuration manipulation
%%--------------------------------------------------------------------

stanza_config_get(Version) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_LIGHT_CONFIGURATION, [version_el(Version)]), room_bin_jid()).

stanza_info_get(Version) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_LIGHT_INFO, [version_el(Version)]), room_bin_jid()).

stanza_config_set(ConfigChanges) ->
    Items = [#xmlel{ name = Key, children = [#xmlcdata{ content = Value }] }
             || {Key, Value} <- ConfigChanges],
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_CONFIGURATION, Items), room_bin_jid()).

%%--------------------------------------------------------------------
%% Affiliations manipulation
%%--------------------------------------------------------------------

stanza_aff_get(Version) ->
    escalus_stanza:to(
      escalus_stanza:iq_get(?NS_MUC_LIGHT_AFFILIATIONS, [version_el(Version)]), room_bin_jid()).

stanza_aff_set(Config, AffUsers0) ->
    NormalizedAffUsers = normalize_aff_users(Config, AffUsers0),
    Items = [#xmlel{ name = <<"user">>, attrs = [{<<"affiliation">>, AffBin}],
                     children = [#xmlcdata{ content = UserBin }] }
             || {UserBin, AffBin} <- NormalizedAffUsers],
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_AFFILIATIONS, Items), room_bin_jid()).

verify_aff_bcast(Config, CurrentOccupants, AffUsersChanges) ->
    verify_aff_bcast(Config, CurrentOccupants, AffUsersChanges, []).

verify_aff_bcast(Config, CurrentOccupants, AffUsersChanges, ExtraNSs) ->
    NormalizedAffUsersChanges = normalize_aff_users(Config, AffUsersChanges),
    foreach_recipient([ User || {User, _} <- CurrentOccupants ],
                      aff_msg_verify_fun(NormalizedAffUsersChanges)),
    lists:foreach(
      fun({Leaver, LeaverAtom}) ->
              Incoming = escalus:wait_for_stanza(Leaver),
              {[X], []} = lists:foldl(
                            fun(XEl, {XAcc, NSAcc}) ->
                                    XMLNS = exml_query:attr(XEl, <<"xmlns">>),
                                    case lists:member(XMLNS, NSAcc) of
                                        true -> {XAcc, lists:delete(XMLNS, NSAcc)};
                                        false -> {[XEl | XAcc], NSAcc}
                                    end
                            end, {[], ExtraNSs}, exml_query:subelements(Incoming, <<"x">>)),
              ?NS_MUC_LIGHT_AFFILIATIONS = exml_query:path(X, [{attr, <<"xmlns">>}]),
              [Item] = exml_query:paths(X, [{element, <<"user">>}]),
              <<"none">> = exml_query:path(Item, [{attr, <<"affiliation">>}]),
              LeaverJIDBin = binary_to_lower(escalus_users:get_jid(Config, LeaverAtom)),
              LeaverJIDBin = exml_query:path(Item, [cdata])
      end, [ {User, UserAtom} || {User, UserAtom, _Aff} <- AffUsersChanges, _Aff == none]).

-spec user_leave(User :: any(), UserAtom :: atom(),
                 RemainingOccupants :: [{User :: any(), UserAtom :: atom()}],
                 Config :: list()) -> any().
user_leave(User, UserAtom, RemainingOccupants, Config) ->
    AffUsersChanges = [{User, UserAtom, none}],
    Stanza = stanza_aff_set(Config, AffUsersChanges),
    escalus:send(User, Stanza),
    % bcast
    verify_aff_bcast(Config, RemainingOccupants, AffUsersChanges),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(User)).

%%--------------------------------------------------------------------
%% Room create & destroy
%%--------------------------------------------------------------------

stanza_create_room(Config, RoomNode, InitConfig, InitOccupants) ->
    RoomBinJID = case RoomNode of
                     undefined -> ?MUCHOST;
                     _ -> <<RoomNode/binary, $@, (?MUCHOST)/binary>>
                 end,
    ConfigItem = #xmlel{ name = <<"configuration">>,
                         children = [ kv_el(K, V) || {K, V} <- InitConfig ] },
    OccupantsItems = [ #xmlel{ name = <<"user">>,
                               attrs = [{<<"affiliation">>, NormAff}],
                               children = [#xmlcdata{ content = NormJID }] }
                       || {NormJID, NormAff} <- normalize_aff_users(Config, InitOccupants) ],
    OccupantsItem = #xmlel{ name = <<"occupants">>, children = OccupantsItems },
    escalus_stanza:to(escalus_stanza:iq_set(
                        ?NS_MUC_LIGHT_CREATE, [ConfigItem, OccupantsItem]), RoomBinJID).

stanza_destroy_room() ->
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_LIGHT_DESTROY, []), room_bin_jid()).

%%--------------------------------------------------------------------
%% Iterators
%%--------------------------------------------------------------------

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

foreach_recipient(Users, VerifyFun) ->
    lists:foreach(
      fun(Recipient) ->
              VerifyFun(escalus:wait_for_stanza(Recipient))
      end, Users).

%%--------------------------------------------------------------------
%% Verification funs generators
%%--------------------------------------------------------------------

config_msg_verify_fun(Config) ->
    fun(Incoming) ->
            escalus:assert(is_groupchat_message, Incoming),
            [X] = exml_query:paths(Incoming, [{element, <<"x">>}]),
            ?NS_MUC_LIGHT_CONFIGURATION = exml_query:path(X, [{attr, <<"xmlns">>}]),
            PrevVersion = exml_query:path(X, [{element, <<"prev-version">>}, cdata]),
            Version = exml_query:path(X, [{element, <<"version">>}, cdata]),
            true = is_binary(Version),
            true = is_binary(PrevVersion),
            true = Version =/= PrevVersion,
            lists:foreach(
              fun({Key, Val}) ->
                      Val = exml_query:path(X, [{element, Key}, cdata])
              end, Config)
    end.

config_iq_verify_fun(Config) ->
    fun(Incoming) ->
            [Query] = exml_query:paths(Incoming, [{element, <<"query">>}]),
            ?NS_MUC_LIGHT_CONFIGURATION = exml_query:path(Query, [{attr, <<"xmlns">>}]),
            verify_config(Query, Config)
    end.

aff_iq_verify_fun(Config, AffUsers0, Version) ->
    NormalizedAffUsers = normalize_aff_users(Config, AffUsers0),
    fun(Incoming) ->
            [Query] = exml_query:paths(Incoming, [{element, <<"query">>}]),
            ?NS_MUC_LIGHT_AFFILIATIONS = exml_query:path(Query, [{attr, <<"xmlns">>}]),
            Version = exml_query:path(Query, [{element, <<"version">>}, cdata]),
            Items = exml_query:paths(Query, [{element, <<"user">>}]),
            verify_aff_users(Items, NormalizedAffUsers)
    end.

aff_msg_verify_fun(NormalizedAffUsersChanges) ->
    fun(Incoming) ->
            [X] = exml_query:subelements(Incoming, <<"x">>),
            ?NS_MUC_LIGHT_AFFILIATIONS = exml_query:path(X, [{attr, <<"xmlns">>}]),
            PrevVersion = exml_query:path(X, [{element, <<"prev-version">>}, cdata]),
            Version = exml_query:path(X, [{element, <<"version">>}, cdata]),
            [Item | RItems] = Items = exml_query:subelements(X, <<"user">>),
            [ToBin | _] = binary:split(exml_query:attr(Incoming, <<"to">>), <<"/">>),
            true = is_binary(Version),
            true = Version =/= PrevVersion,
            case {ToBin == exml_query:cdata(Item), RItems} of
                {true, []} ->
                    {_, ProperAff} = lists:keyfind(ToBin, 1, NormalizedAffUsersChanges),
                    ProperAff = exml_query:attr(Item, <<"affiliation">>);
                _ ->
                    true = is_binary(PrevVersion),
                    verify_aff_users(Items, NormalizedAffUsersChanges)
            end
    end.

info_iq_verify_fun(Config, AffUsers0, Version, ConfigKVBin) ->
    NormalizedAffUsers = normalize_aff_users(Config, AffUsers0),
    fun(Incoming) ->
            [Query] = exml_query:paths(Incoming, [{element, <<"query">>}]),
            ?NS_MUC_LIGHT_INFO = exml_query:path(Query, [{attr, <<"xmlns">>}]),
            Version = exml_query:path(Query, [{element, <<"version">>}, cdata]),
            UsersItems = exml_query:paths(Query, [{element, <<"occupants">>}, {element, <<"user">>}]),
            verify_aff_users(UsersItems, NormalizedAffUsers),
            ConfigurationEl = exml_query:path(Query, [{element, <<"configuration">>}]),
            verify_config(ConfigurationEl, ConfigKVBin)
    end.

verify_config(ConfigRoot, Config) ->
    lists:foreach(
      fun({Key, Val}) ->
              Val = exml_query:path(ConfigRoot, [{element, Key}, cdata])
      end, Config).

verify_aff_users(Items, NormalizedAffUsers) ->
    true = (length(Items) == length(NormalizedAffUsers)),
    [] = lists:foldl(
           fun(Item, AffAcc) ->
                   JID = exml_query:path(Item, [cdata]),
                   Aff = exml_query:path(Item, [{attr, <<"affiliation">>}]),
                   verify_keytake(lists:keytake(JID, 1, AffAcc), JID, Aff, AffAcc)
           end, NormalizedAffUsers, Items).

verify_keytake({value, {_, Aff}, NewAffAcc}, _JID, Aff, _AffAcc) -> NewAffAcc.

%%--------------------------------------------------------------------
%% Other helpers
%%--------------------------------------------------------------------

ver(Int) ->
    <<"ver-", (list_to_binary(integer_to_list(Int)))/binary>>.

version_el(Version) ->
    #xmlel{ name = <<"version">>, children = [#xmlcdata{ content = Version }] }.

default_aff_users() ->
    [
     {alice, owner},
     {bob, member},
     {kate, member}
    ].

normalize_aff_users(Config, AffUsers0) ->
    [normalize_aff_user(AffUser, Config) || AffUser <- AffUsers0].

normalize_aff_user({UserAtom, Aff}, Config) ->
    {binary_to_lower(escalus_users:get_jid(Config, UserAtom)), list_to_binary(atom_to_list(Aff))};
normalize_aff_user({_User, UserAtom, Aff}, Config) ->
    normalize_aff_user({UserAtom, Aff}, Config).

create_room(RoomU, MUCHost, Owner, Config, Version) ->
    DefaultConfig = default_config(),
    RoomUS = {RoomU, MUCHost},
    OwnerUS = to_lus(jid(Owner, Config)),
    {ok, US} = rpc(backend(), create_room, [RoomUS, DefaultConfig, [{OwnerUS, owner}], Version]),
    US.

clear_db() ->
    rpc(backend(), force_clear, []).

room_exists(RoomU, MUCHost) ->
    rpc(backend(), room_exists, [{RoomU, MUCHost}]).

jid(User, Config) ->
    UserU = escalus_users:get_username(Config, User),
    UserS = escalus_users:get_server(Config, User),
    make_jid(UserU, UserS).

room_bin_jid() ->
    <<(?ROOM)/binary, $@, (?MUCHOST)/binary>>.

add_occupant(RoomU, MUCHost, User, Config, Version) ->
    UserLJID = to_lus(jid(User, Config)),
    RoomUS = {RoomU, MUCHost},
    NewAff = [{UserLJID, member}],
    {ok, _, _, _, _} = rpc(backend(), modify_aff_users, [RoomUS, NewAff, ?CHECK_FUN, Version]).

make_jid(User, Server) ->
    escalus_ejabberd:rpc(jlib, make_jid, [User, Server, <<>>]).

to_lus(JID) ->
    escalus_ejabberd:rpc(jlib, jid_to_lus, [JID]).

binary_to_lower(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).

backend() ->
    mod_muc_light_db_backend.

default_config() ->
    rpc(mod_muc_light, default_config, []).

no_stanzas(Users) ->
    lists:foreach(
      fun(User) ->
              {false, _} = {escalus_client:has_stanzas(User), User}
      end, Users).

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

set_mod_config(K, V) ->
    rpc(mod_muc_light, set_service_opt, [K, V]).

kv_el(K, V) ->
    #xmlel{ name = K, children = [ #xmlcdata{ content = V } ] }.

