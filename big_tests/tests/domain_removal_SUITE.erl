-module(domain_removal_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4, subhost_pattern/1]).
-import(domain_helper, [host_type/0, domain_to_host_type/2, domain/0]).
-import(config_parser_helper, [mod_config/2]).

-include("mam_helper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("jid/include/jid.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, auth_removal},
     {group, cache_removal},
     {group, mam_removal},
     {group, mam_removal_incremental},
     {group, inbox_removal},
     {group, inbox_removal_incremental},
     {group, muc_light_removal},
     {group, muc_removal},
     {group, private_removal},
     {group, roster_removal},
     {group, offline_removal},
     {group, markers_removal},
     {group, vcard_removal},
     {group, last_removal},
     {group, removal_failures}
    ].

groups() ->
    [
     {auth_removal, [], [auth_removal]},
     {cache_removal, [], [cache_removal]},
     {mam_removal, [], [mam_pm_removal,
                        mam_muc_removal]},
     {mam_removal_incremental, [], [mam_pm_removal,
                                    mam_muc_removal]},
     {inbox_removal, [], [inbox_removal]},
     {inbox_removal_incremental, [], [inbox_removal]},
     {muc_light_removal, [], [muc_light_removal,
                              muc_light_blocking_removal]},
     {muc_removal, [], [muc_removal]},
     {private_removal, [], [private_removal]},
     {roster_removal, [], [roster_removal]},
     {offline_removal, [], [offline_removal]},
     {markers_removal, [], [markers_removal]},
     {vcard_removal, [], [vcard_removal]},
     {last_removal, [], [last_removal]},
     {removal_failures, [], [removal_stops_if_handler_fails]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================

init_per_group(auth_removal = Group, Config) ->
    case is_internal_or_rdbms() of
        true ->
            HostTypes = domain_helper:host_types(),
            Config2 = dynamic_modules:save_modules(HostTypes, Config),
            [dynamic_modules:ensure_modules(HostType, group_to_modules(Group)) ||
                HostType <- HostTypes],
            Config2;
        false ->
            {skip, require_internal_or_rdbms}
    end;
init_per_group(Group, Config) ->
    case mongoose_helper:is_rdbms_enabled(host_type()) of
        true ->
            HostTypes = domain_helper:host_types(),
            Config2 = dynamic_modules:save_modules(HostTypes, Config),
            [dynamic_modules:ensure_modules(HostType, group_to_modules(Group)) ||
                HostType <- HostTypes],
            Config2;
        false ->
            {skip, require_rdbms}
    end.

end_per_group(auth_removal, Config) ->
    case is_internal_or_rdbms() of
        true ->
            dynamic_modules:restore_modules(Config);
        false ->
            ok
    end;
end_per_group(_GroupName, Config) ->
    case mongoose_helper:is_rdbms_enabled(host_type()) of
        true ->
            dynamic_modules:restore_modules(Config);
        false ->
            ok
    end,
    ok.

group_to_modules(removal_failures) ->
    group_to_modules(mam_removal);
group_to_modules(auth_removal) ->
    [];
group_to_modules(cache_removal) ->
    [{mod_cache_users, config_parser_helper:default_mod_config(mod_cache_users)},
     {mod_mam, mam_helper:config_opts(#{pm => #{}})}];
group_to_modules(mam_removal) ->
    MucHost = subhost_pattern(muc_light_helper:muc_host_pattern()),
    [{mod_mam, mam_helper:config_opts(#{pm => #{}, muc => #{host => MucHost}})},
     {mod_muc_light, mod_config(mod_muc_light, #{backend => rdbms})}];
group_to_modules(mam_removal_incremental) ->
    MucHost = subhost_pattern(muc_light_helper:muc_host_pattern()),
    [{mod_mam, mam_helper:config_opts(#{delete_domain_limit => 1, pm => #{}, muc => #{host => MucHost}})},
     {mod_muc_light, mod_config(mod_muc_light, #{backend => rdbms})}];
group_to_modules(muc_light_removal) ->
    [{mod_muc_light, mod_config(mod_muc_light, #{backend => rdbms})}];
group_to_modules(muc_removal) ->
    MucHost = subhost_pattern(muc_helper:muc_host_pattern()),
    OnlineBackend = ct_helper:get_internal_database(),
    Opts = #{backend => rdbms, online_backend => OnlineBackend, host => MucHost},
    [{mod_muc, muc_helper:make_opts(Opts)}];
group_to_modules(inbox_removal) ->
    [{mod_inbox, inbox_helper:inbox_opts()}];
group_to_modules(inbox_removal_incremental) ->
    [{mod_inbox, (inbox_helper:inbox_opts())#{delete_domain_limit => 1}}];
group_to_modules(private_removal) ->
    [{mod_private, #{iqdisc => one_queue, backend => rdbms}}];
group_to_modules(roster_removal) ->
    [{mod_roster, mod_config(mod_roster, #{backend => rdbms})}];
group_to_modules(offline_removal) ->
    [{mod_offline, mod_config(mod_offline, #{backend => rdbms})}];
group_to_modules(markers_removal) ->
    [{mod_smart_markers, config_parser_helper:default_mod_config(mod_smart_markers)}];
group_to_modules(vcard_removal) ->
    [{mod_vcard, mod_config(mod_vcard, #{backend => rdbms})}];
group_to_modules(last_removal) ->
    [{mod_last, mod_config(mod_last, #{backend => rdbms})}].

is_internal_or_rdbms() ->
    AuthMods = mongoose_helper:auth_modules(),
    Pred = fun(E) -> E == ejabberd_auth_internal orelse E == ejabberd_auth_rdbms end,
    lists:any(Pred, AuthMods).

%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================

init_per_testcase(muc_removal, Config) ->
    muc_helper:load_muc(),
    mongoose_helper:ensure_muc_clean(),
    escalus:init_per_testcase(muc_removal, Config);
init_per_testcase(roster_removal, ConfigIn) ->
    Config = roster_helper:set_versioning(true, true, ConfigIn),
    escalus:init_per_testcase(roster_removal, Config);
init_per_testcase(TestCase, Config) ->
    escalus:init_per_testcase(TestCase, Config).

end_per_testcase(muc_removal, Config) ->
    mongoose_helper:ensure_muc_clean(),
    muc_helper:unload_muc(),
    escalus:end_per_testcase(muc_removal, Config);
end_per_testcase(TestCase, Config) ->
    escalus:end_per_testcase(TestCase, Config).

%%%===================================================================
%%% Test Cases
%%%===================================================================

auth_removal(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [{alice, 1}, {alice_bis, 1}]),
    AliceSpec = escalus_users:get_userspec(FreshConfig, alice),
    AliceBisSpec = escalus_users:get_userspec(FreshConfig, alice_bis),
    connect_and_disconnect(AliceSpec),
    connect_and_disconnect(AliceBisSpec),
    ?assertMatch([_Alice], rpc(mim(), ejabberd_auth, get_vh_registered_users, [domain()])),
    run_remove_domain(),
    ?assertMatch({error, {connection_step_failed, _, _}}, escalus_connection:start(AliceSpec)),
    connect_and_disconnect(AliceBisSpec), % different domain - not removed
    ?assertEqual([], rpc(mim(), ejabberd_auth, get_vh_registered_users, [domain()])).

cache_removal(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [{alice, 1}, {alice_bis, 1}]),
    F = fun(Alice, AliceBis) ->
                escalus:send(Alice, escalus_stanza:chat_to(AliceBis, <<"Hi!">>)),
                escalus:wait_for_stanza(AliceBis),
                mam_helper:wait_for_archive_size(Alice, 1),
                mam_helper:wait_for_archive_size(AliceBis, 1)
        end,
    escalus:story(FreshConfig, [{alice, 1}, {alice_bis, 1}], F),
    %% Storing the message in MAM should have populated the cache for both users
    ?assertEqual({stop, true}, does_cached_user_exist(FreshConfig, alice)),
    ?assertEqual({stop, true}, does_cached_user_exist(FreshConfig, alice_bis)),
    run_remove_domain(),
    %% Cache removed only for Alice's domain
    ?assertEqual({ok, false}, does_cached_user_exist(FreshConfig, alice)),
    ?assertEqual({stop, true}, does_cached_user_exist(FreshConfig, alice_bis)).

mam_pm_removal(Config) ->
    F = fun(Alice, Bob) ->
        N = 3,
        [ escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)) || _ <- lists:seq(1, N) ],
        escalus:wait_for_stanzas(Bob, N),
        mam_helper:wait_for_archive_size(Alice, N),
        mam_helper:wait_for_archive_size(Bob, N),
        run_remove_domain(),
        mam_helper:wait_for_archive_size(Alice, 0),
        mam_helper:wait_for_archive_size(Bob, 0)
        end,
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], F).

mam_muc_removal(Config0) ->
    F = fun(Config, Alice) ->
        N = 3,
        Room = muc_helper:fresh_room_name(),
        MucHost = muc_light_helper:muc_host(),
        muc_light_helper:create_room(Room, MucHost, alice,
                                     [], Config, muc_light_helper:ver(1)),
        RoomAddr = <<Room/binary, "@", MucHost/binary>>,
        [ escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, <<"text">>)) || _ <- lists:seq(1, N) ],
        escalus:wait_for_stanzas(Alice, N),
        mam_helper:wait_for_room_archive_size(MucHost, Room, N),
        run_remove_domain(),
        mam_helper:wait_for_room_archive_size(MucHost, Room, 0)
        end,
    escalus_fresh:story_with_config(Config0, [{alice, 1}], F).

inbox_removal(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:wait_for_stanza(Bob),
        inbox_helper:get_inbox(Alice, #{count => 1}),
        inbox_helper:get_inbox(Bob, #{count => 1}),
        run_remove_domain(),
        inbox_helper:get_inbox(Alice, #{count => 0, unread_messages => 0, active_conversations => 0}),
        inbox_helper:get_inbox(Bob, #{count => 0, unread_messages => 0, active_conversations => 0})
      end).

muc_removal(Config0) ->
    muc_helper:story_with_room(Config0, [{persistent, true}], [{alice, 1}], fun(Config, Alice) ->
        AliceJid= jid:from_binary(escalus_client:full_jid(Alice)),
        {_, Domain} = jid:to_lus(AliceJid),
        MucHost = muc_helper:muc_host(),
        % Alice joins room and registers nick
        EnterRoom = muc_helper:stanza_muc_enter_room(?config(room, Config), <<"alice">>),
        escalus:send(Alice, EnterRoom),
        escalus:wait_for_stanzas(Alice, 2),
        muc_helper:set_nick(Alice, <<"alice2">>),
        % check muc tables
        ?assertMatch([_], get_muc_rooms(MucHost)),
        ?assertMatch([_], get_muc_room_aff(Domain)),
        ?assertMatch({ok, _}, get_muc_registered(MucHost, AliceJid)),
        % remove domain and check muc tables
        run_remove_domain(),
        ?assertMatch([], get_muc_rooms(MucHost)),
        ?assertMatch([], get_muc_room_aff(Domain)),
        ?assertMatch({error, not_registered}, get_muc_registered(MucHost, AliceJid))
    end).

muc_light_removal(Config0) ->
    F = fun(Config, Alice) ->
        %% GIVEN a room
        Room = muc_helper:fresh_room_name(),
        MucHost = muc_light_helper:muc_host(),
        RoomAddr = <<Room/binary, "@", MucHost/binary>>,
        muc_light_helper:create_room(Room, MucHost, alice,
                                     [], Config, muc_light_helper:ver(1)),
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, <<"text">>)),
        escalus:wait_for_stanza(Alice),
        RoomID = select_room_id(host_type(), Room, MucHost),
        {selected, [_]} = select_affs_by_room_id(host_type(), RoomID),
        {selected, [_|_]} = select_config_by_room_id(host_type(), RoomID),
        {ok, _RoomConfig, _AffUsers, _Version} = get_room_info(host_type(), Room, MucHost),
        %% WHEN domain hook called
        run_remove_domain(),
        %% THEN Room info not available
        {error, not_exists} = get_room_info(host_type(), Room, MucHost),
        %% THEN Tables are empty
        {selected, []} = select_affs_by_room_id(host_type(), RoomID),
        {selected, []} = select_config_by_room_id(host_type(), RoomID)
        end,
    escalus_fresh:story_with_config(Config0, [{alice, 1}], F).

muc_light_blocking_removal(Config0) ->
    F = fun(Config, Alice, Bob) ->
        %% GIVEN a room
        Room = muc_helper:fresh_room_name(),
        MucHost = muc_light_helper:muc_host(),
        muc_light_helper:create_room(Room, MucHost, alice,
                                     [], Config, muc_light_helper:ver(1)),
        block_muclight_user(Bob, Alice),
        [_] = get_blocking(host_type(), Bob, MucHost),
        %% WHEN domain hook called
        run_remove_domain(),
        [] = get_blocking(host_type(), Bob, MucHost)
        end,
    escalus_fresh:story_with_config(Config0, [{alice, 1}, {bob, 1}], F).

private_removal(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        NS = <<"alice:private:ns">>,
        Tag = <<"my_element">>,
        %% Alice stores some data in her private storage
        IqSet = escalus_stanza:private_set(my_banana(NS)),
        IqGet = escalus_stanza:private_get(NS, Tag),
        escalus:send_iq_and_wait_for_result(Alice, IqSet),
        %% Compare results before and after removal
        Res1 = escalus_client:send_iq_and_wait_for_result(Alice, IqGet),
        run_remove_domain(),
        Res2 = escalus_client:send_iq_and_wait_for_result(Alice, IqGet),
        escalus:assert(is_private_result, Res1),
        escalus:assert(is_private_result, Res2),
        Val1 = get_private_data(Res1, Tag, NS),
        Val2 = get_private_data(Res2, Tag, NS),
        ?assert_equal_extra(<<"banana">>, Val1, #{stanza => Res1}),
        ?assert_equal_extra(<<>>, Val2, #{stanza => Res2})
      end).

offline_removal(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun(FreshConfig, Alice, Bob) ->
        mongoose_helper:logout_user(FreshConfig, Bob),
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"msgtxt">>)),
        % wait until message is stored
        BobJid = jid:from_binary(escalus_client:full_jid(Bob)),
        {LUser, LServer} = jid:to_lus(BobJid),
        wait_helper:wait_until(
          fun() -> mongoose_helper:total_offline_messages({LUser, LServer}) end, 1),
        % check messages in DB
        ?assertMatch({ok, [_]}, rpc(mim(), mod_offline_rdbms, fetch_messages, [host_type(), BobJid])),
        run_remove_domain(),
        ?assertMatch({ok, []}, rpc(mim(), mod_offline_rdbms, fetch_messages, [host_type(), BobJid]))
    end).

markers_removal(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Body = <<"Hello Bob!">>,
        MsgId = escalus_stanza:id(),
        Msg = escalus_stanza:set_id(escalus_stanza:chat_to(Bob, Body), MsgId),
        escalus:send(Alice, Msg),
        escalus:wait_for_stanza(Bob),
        ChatMarker = escalus_stanza:chat_marker(Alice, <<"displayed">>, MsgId),
        escalus:send(Bob, ChatMarker),
        escalus:wait_for_stanza(Alice),
        wait_helper:wait_until(
          fun() -> 1 =< mongoose_helper:generic_count(mod_smart_markers) end, true),
        % check messages in DB
        AliceJid = jid:from_binary(escalus_client:full_jid(Alice)),
        ?assertMatch([_], rpc(mim(), mod_smart_markers_backend, get_chat_markers,
                                    [host_type(), AliceJid, undefined, 0])),
        run_remove_domain(),
        ?assertMatch([], rpc(mim(), mod_smart_markers_backend, get_chat_markers,
                                   [host_type(), AliceJid, undefined, 0]))
    end).

roster_removal(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% add contact
        Stanza = escalus_stanza:roster_add_contact(Bob, [<<"friends">>], <<"Bobby">>),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_roster_set, is_iq_result], Received),

        %% check roster
        BobJid = escalus_client:short_jid(Bob),
        Received2 = escalus:send_iq_and_wait_for_result(Alice, escalus_stanza:roster_get()),
        escalus:assert(is_roster_result, Received2),
        escalus:assert(roster_contains, [BobJid], Received2),
        escalus:assert(count_roster_items, [1], Received2),
        ?assertMatch([_], select_from_roster("rosterusers")),
        ?assertMatch([_], select_from_roster("rostergroups")),
        ?assertMatch([_], select_from_roster("roster_version")),

        %% remove domain and check roster
        run_remove_domain(),
        Received3 = escalus:send_iq_and_wait_for_result(Alice, escalus_stanza:roster_get()),
        escalus:assert(is_roster_result, Received3),
        escalus:assert(count_roster_items, [0], Received3),
        ?assertMatch([], select_from_roster("rosterusers")),
        ?assertMatch([], select_from_roster("rostergroups")),
        ?assertMatch([], select_from_roster("roster_version"))
    end).

vcard_removal(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Client) ->
        DirJID = <<"vjud.", (domain())/binary>>,
        {LUser, LServer} = jid:to_lus(jid:from_binary(escalus_client:full_jid(Client))),
        VCardFields = [{<<"FN">>, <<"Old name">>}],
        FilterFields = [{<<"fn">>, <<"Old name">>}],
        DbFilterFields = [{<<"fn">>, [<<"Old name">>]}],
        %create vcard for alice
        UpdateResult = escalus:send_and_wait(Client,
                                             escalus_stanza:vcard_update(VCardFields)),
        escalus:assert(is_iq_result, UpdateResult),
        %check before domain removal
        RequestResult = escalus:send_and_wait(Client, escalus_stanza:vcard_request()),
        ?assertMatch(<<"Old name">>, get_vcard_fn(RequestResult)),
        SearchResult = escalus:send_and_wait(Client,
                                             search_vcard_fields(DirJID, FilterFields)),
        ?assertMatch(<<"1">>, get_vcard_search_query_count(SearchResult)),
        ?assertMatch({ok, _}, rpc(mim(), mod_vcard_rdbms, get_vcard,
                                  [host_type(), LUser, LServer])),
        ?assertMatch([_], rpc(mim(), mod_vcard_rdbms, search,
                              [host_type(), LServer, DbFilterFields])),
        %check after domain removal
        run_remove_domain(),
        RequestResult2 = escalus:send_and_wait(Client, escalus_stanza:vcard_request()),
        escalus:assert(is_iq_error, RequestResult2),
        SearchResult2 = escalus:send_and_wait(Client,
                                              search_vcard_fields(DirJID, FilterFields)),
        ?assertMatch(<<"0">>, get_vcard_search_query_count(SearchResult2)),
        ?assertMatch({error, _}, rpc(mim(), mod_vcard_rdbms, get_vcard,
                                     [host_type(), LUser, LServer])),
        ?assertMatch([], rpc(mim(), mod_vcard_rdbms, search,
                             [host_type(), LServer, DbFilterFields]))
    end).

last_removal(Config0) ->
    F = fun(Config2, Alice, Bob) ->
            escalus_story:make_all_clients_friends([Alice, Bob]),

            %% Bob logs out with a status
            Status = escalus_stanza:tags([{<<"status">>, <<"I am a banana!">>}]),
            Presence = escalus_stanza:presence(<<"unavailable">>, Status),
            escalus_client:send(Bob, Presence),

            escalus_client:stop(Config2, Bob),
            timer:sleep(1024), % more than a second

            PresUn = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_presence_with_type, [<<"unavailable">>], PresUn),

            %% Alice asks for Bob's last availability
            BobShortJID = escalus_client:short_jid(Bob),
            GetLast = escalus_stanza:last_activity(BobShortJID),
            Stanza = escalus_client:send_iq_and_wait_for_result(Alice, GetLast),

            %% Alice receives Bob's status and last online time > 0
            escalus:assert(is_last_result, Stanza),
            true = (1 =< get_last_activity(Stanza)),
            <<"I am a banana!">> = get_last_status(Stanza),

            run_remove_domain(),
            escalus_client:send(Alice, GetLast),
            Error = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], Error)
        end,
    escalus:fresh_story_with_config(Config0, [{alice, 1}, {bob, 1}], F).

removal_stops_if_handler_fails(Config0) ->
    mongoose_helper:inject_module(?MODULE),
    F = fun(Config, Alice) ->
        start_domain_removal_hook(),
        Room = muc_helper:fresh_room_name(),
        MucHost = muc_light_helper:muc_host(),
        muc_light_helper:create_room(Room, MucHost, alice, [], Config, muc_light_helper:ver(1)),
        RoomAddr = <<Room/binary, "@", MucHost/binary>>,
        escalus:send(Alice, escalus_stanza:groupchat_to(RoomAddr, <<"text">>)),
        escalus:wait_for_stanza(Alice),
        mam_helper:wait_for_room_archive_size(MucHost, Room, 1),
        run_remove_domain(),
        mam_helper:wait_for_room_archive_size(MucHost, Room, 1),
        stop_domain_removal_hook(),
        run_remove_domain(),
        mam_helper:wait_for_room_archive_size(MucHost, Room, 0)
        end,
    escalus_fresh:story_with_config(Config0, [{alice, 1}], F).

%% Helpers
start_domain_removal_hook() ->
    rpc(mim(), ?MODULE, rpc_start_domain_removal_hook, [host_type()]).

stop_domain_removal_hook() ->
    rpc(mim(), ?MODULE, rpc_stop_domain_removal_hook, [host_type()]).

rpc_start_domain_removal_hook(HostType) ->
    gen_hook:add_handler(remove_domain, HostType,
                         fun ?MODULE:domain_removal_hook_fn/3,
                         #{}, 30). %% Priority is so that it comes before muclight and mam

rpc_stop_domain_removal_hook(HostType) ->
    gen_hook:delete_handler(remove_domain, HostType,
                            fun ?MODULE:domain_removal_hook_fn/3,
                            #{}, 30).

domain_removal_hook_fn(Acc, _Params, _Extra) ->
    F = fun() -> throw(first_time_needs_to_fail) end,
    mongoose_domain_api:remove_domain_wrapper(Acc, F, ?MODULE).

connect_and_disconnect(Spec) ->
    {ok, Client, _} = escalus_connection:start(Spec),
    escalus_connection:stop(Client).

does_cached_user_exist(Config, User) ->
    Jid = #jid{lserver = Domain} = jid:from_binary(escalus_users:get_jid(Config, User)),
    HostType = domain_to_host_type(mim(), Domain),
    rpc(mim(), mod_cache_users, does_cached_user_exist, [false, #{jid =>Jid, request_type => stored}, #{host_type => HostType}]).

search_vcard_fields(DirJID, Filters) ->
    escalus_stanza:search_iq(DirJID, escalus_stanza:search_fields(Filters)).

get_vcard_fn(Element) ->
    exml_query:path(Element, [{element, <<"vCard">>},
                              {element, <<"FN">>},
                              cdata]).

get_vcard_search_query_count(Element) ->
    exml_query:path(Element, [{element, <<"query">>},
                              {element, <<"set">>},
                              {element, <<"count">>},
                              cdata]).

get_muc_registered(MucHost, UserJid) ->
    rpc(mim(), mod_muc_rdbms, get_nick, [host_type(), MucHost, UserJid]).

get_muc_rooms(MucHost) ->
    {ok, Rooms} = rpc(mim(), mod_muc_rdbms, get_rooms, [host_type(), MucHost]),
    Rooms.

get_muc_room_aff(Domain) ->
    Query = "SELECT * FROM muc_room_aff WHERE lserver = '" ++ binary_to_list(Domain) ++ "'",
    {selected, Affs} = rpc(mim(), mongoose_rdbms, sql_query, [host_type(), Query]),
    Affs.

select_from_roster(Table) ->
    Query = "SELECT * FROM " ++ Table ++ " WHERE server='" ++ binary_to_list(domain()) ++ "'",
    {selected, Res} = rpc(mim(), mongoose_rdbms, sql_query, [host_type(), Query]),
    Res.

run_remove_domain() ->
    rpc(mim(), mongoose_hooks, remove_domain, [host_type(), domain()]).

get_room_info(HostType, RoomU, RoomS) ->
    rpc(mim(), mod_muc_light_db_backend, get_info, [HostType, {RoomU, RoomS}]).

select_room_id(MainHost, RoomU, RoomS) ->
    {selected, [{DbRoomID}]} =
        rpc(mim(), mod_muc_light_db_rdbms, select_room_id, [MainHost, RoomU, RoomS]),
    rpc(mim(), mongoose_rdbms, result_to_integer, [DbRoomID]).

select_affs_by_room_id(MainHost, RoomID) ->
    rpc(mim(), mod_muc_light_db_rdbms, select_affs_by_room_id, [MainHost, RoomID]).

select_config_by_room_id(MainHost, RoomID) ->
    rpc(mim(), mod_muc_light_db_rdbms, select_config_by_room_id, [MainHost, RoomID]).

get_blocking(HostType, User, MUCServer) ->
    Jid = jid:from_binary(escalus_client:short_jid(User)),
    {LUser, LServer, _} = jid:to_lower(Jid),
    rpc(mim(), mod_muc_light_db_rdbms, get_blocking, [HostType, {LUser, LServer}, MUCServer]).

block_muclight_user(Bob, Alice) ->
    %% Bob blocks Alice
    AliceJIDBin = escalus_client:short_jid(Alice),
    BlocklistChange = [{user, deny, AliceJIDBin}],
    escalus:send(Bob, muc_light_helper:stanza_blocking_set(BlocklistChange)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)).

my_banana(NS) ->
    #xmlel{
        name = <<"my_element">>,
        attrs = #{<<"xmlns">> => NS},
        children = [#xmlcdata{content = <<"banana">>}]}.

get_private_data(Elem, Tag, NS) ->
    Path = [{element, <<"query">>}, {element_with_ns, Tag, NS}, cdata],
    exml_query:path(Elem, Path).

get_last_activity(Stanza) ->
    S = exml_query:path(Stanza, [{element, <<"query">>}, {attr, <<"seconds">>}]),
    list_to_integer(binary_to_list(S)).

get_last_status(Stanza) ->
    exml_query:path(Stanza, [{element, <<"query">>}, cdata]).
