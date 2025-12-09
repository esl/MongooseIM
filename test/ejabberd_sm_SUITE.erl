-module(ejabberd_sm_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("jid/include/jid.hrl").
-include_lib("session.hrl").
-compile([export_all, nowarn_export_all]).

-define(eq(E, I), ?assertEqual(E, I)).

-define(B(C), (proplists:get_value(backend, C))).
-define(MAX_USER_SESSIONS, 2).

-import(config_parser_helper, [config/2, default_config/1]).

all() -> [{group, mnesia}, {group, redis}, {group, cets}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    application:ensure_all_started(exometer_core),
    mongoose_config:set_opts(opts()),
    async_helper:start(Config, [{ejabberd_sm_backend_sup, start_link, []},
                                {mongoose_instrument, start_link, []}]).

end_per_suite(Config) ->
    async_helper:stop_all(Config),
    mongoose_config:erase_opts(),
    application:stop(exometer),
    application:stop(exometer_core).

opts() ->
    #{instrumentation => config_parser_helper:default_config([instrumentation]),
      hosts => [<<"localhost">>, <<"otherhost">>],
      host_types => []}.

groups() ->
    [{mnesia, [], tests()},
     {redis, [], tests() ++ redis_only_tests()},
     {cets, [], tests()}].

tests() ->
    [open_session,
     get_full_session_list,
     get_vh_session_list,
     get_sessions_2,
     get_sessions_3,
     session_is_updated_when_created_twice,
     delete_session,
     clean_up,
     clean_up_with_colon_in_resource,
     too_many_sessions,
     unique_count,
     session_info_is_stored,
     session_info_is_updated_if_keys_match,
     session_info_is_updated_properly_if_session_conflicts,
     session_info_is_extended_if_new_keys_present,
     session_info_keys_not_truncated_if_session_opened_with_empty_infolist,
     kv_can_be_stored_for_session,
     kv_can_be_updated_for_session,
     kv_can_be_removed_for_session,
     store_info_sends_message_to_the_session_owner,
     remove_info_sends_message_to_the_session_owner,
     parse_session_key_s5_format,
     parse_session_key_s4_format,
     parse_session_key_s4_format_with_colon_in_sid
    ].

redis_only_tests() ->
    [clean_up_s4_backward_compatibility].

init_per_group(mnesia, Config) ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    [{backend, ejabberd_sm_mnesia} | Config];
init_per_group(redis, Config) ->
    init_redis_group(is_redis_running(), Config);
init_per_group(cets, Config) ->
    DiscoOpts = #{name => mongoose_cets_discovery, disco_file => "does_not_exist.txt"},
    {ok, Pid} = cets_discovery:start(DiscoOpts),
    [{backend, ejabberd_sm_cets}, {cets_disco_pid, Pid} | Config].

init_redis_group({true, ConnType}, Config) ->
    Self = self(),
    proc_lib:spawn(fun() ->
                  register(test_helper, self()),
                  mongoose_wpool:ensure_started(),
                  % This would be started via outgoing_pools in normal case
                  Pool = config([outgoing_pools, redis, default], redis_pool_config(ConnType)),
                  mongoose_wpool:start_configured_pools([Pool], [], []),
                  Self ! ready,
                  receive stop -> ok end
          end),
    receive ready -> ok after timer:seconds(30) -> ct:fail(test_helper_not_ready) end,
    [{backend, ejabberd_sm_redis} | Config];
init_redis_group(_, _) ->
    {skip, "redis not running"}.

redis_pool_config(plain) ->
    #{};
redis_pool_config(tls) ->
    #{conn_opts => #{tls => #{verify_mode => none}}}.

end_per_group(mnesia, Config) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    Config;
end_per_group(cets, Config) ->
    exit(proplists:get_value(cets_disco_pid, Config), kill),
    Config;
end_per_group(redis, Config) ->
    whereis(test_helper) ! stop,
    Config.

init_per_testcase(too_many_sessions, Config) ->
    set_test_case_meck(?MAX_USER_SESSIONS, true),
    setup_sm(Config),
    Config;
init_per_testcase(Case, Config) when Case =:= parse_session_key_s5_format;
                                      Case =:= parse_session_key_s4_format;
                                      Case =:= parse_session_key_s4_format_with_colon_in_sid ->
    %% No setup needed - these are pure function tests
    Config;
init_per_testcase(Case, Config) ->
    set_test_case_meck(infinity, should_meck_c2s(Case)),
    setup_sm(Config),
    Config.

should_meck_c2s(store_info_sends_message_to_the_session_owner) -> false;
should_meck_c2s(remove_info_sends_message_to_the_session_owner) -> false;
should_meck_c2s(_) -> true.

end_per_testcase(Case, Config) when Case =:= parse_session_key_s5_format;
                                     Case =:= parse_session_key_s4_format;
                                     Case =:= parse_session_key_s4_format_with_colon_in_sid ->
    Config;
end_per_testcase(_, Config) ->
    clean_sessions(Config),
    terminate_sm(),
    unload_meck().

open_session(C) ->
    {Sid, USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR),
    verify_session_opened(C, Sid, USR).

get_full_session_list(C) ->
    ManyUsers = generate_many_random_users(5, [<<"localhost">>, <<"otherhost">>]),
    ManyUsersLen = length(ManyUsers),
    [given_session_opened(Sid, USR) || {Sid, USR} <- ManyUsers],
    AllSessions = ejabberd_sm:get_full_session_list(),
    AllSessionsLen = length(AllSessions),
    AllSessionsLen = ManyUsersLen,
    [verify_session_opened(C, Sid, USR) || {Sid, USR} <- ManyUsers].

get_vh_session_list(C) ->
    ManyUsersLocal = generate_many_random_users(5, [<<"localhost">>]),
    ManyUsersOther = generate_many_random_users(5, [<<"otherhost">>]),
    ManyUsersLocalLen = length(ManyUsersLocal),
    [given_session_opened(Sid, USR) || {Sid, USR} <- ManyUsersLocal ++ ManyUsersOther],
    LocalhostSessions = ejabberd_sm:get_vh_session_list(<<"localhost">>),
    LocalhostSessionsLen = length(LocalhostSessions),
    LocalhostSessionsLen = ManyUsersLocalLen,
    ManyUsersLocalLen = ejabberd_sm:get_vh_session_number(<<"localhost">>),
    [verify_session_opened(C, Sid, USR) || {Sid, USR} <- ManyUsersLocal].

get_sessions_2(C) ->
    UsersWithManyResources = generate_many_random_res(5, 3, [<<"localhost">>, <<"otherhost">>]),
    [given_session_opened(Sid, USR) || {Sid, USR} <- UsersWithManyResources],
    USDict = get_unique_us_dict(UsersWithManyResources),
    [verify_session_opened(C, U, S, dict:fetch({U, S}, USDict)) || {U, S} <- dict:fetch_keys(USDict)],
    [verify_session_opened(C, Sid, USR) || {Sid, USR} <- UsersWithManyResources].


get_sessions_3(C) ->
    UserRes = generate_many_random_res(1, 3, [<<"localhost">>]),
    AllSessions = length(UserRes),
    {_, {User, Server, _}} = hd(UserRes),
    [given_session_opened(Sid, USR) || {Sid, USR} <- UserRes],
    Sessions_2 = ?B(C):get_sessions(User, Server),
    AllSessions = length(Sessions_2),
    F = fun({Sid, {U, S, R} = USR}) ->
        [#session{sid = Sid} = Session] = ?B(C):get_sessions(U, S, R),
        Session = lists:keyfind(Sid, #session.sid, Sessions_2),
        Session = lists:keyfind(USR, #session.usr, Sessions_2),
        true
    end,
    true = lists:all(F, UserRes).

session_is_updated_when_created_twice(C) ->
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR),
    verify_session_opened(C, Sid, USR),

    given_session_opened(Sid, USR, 20),
    verify_session_opened(C, Sid, USR),

    [#session{usr = USR, sid = Sid, priority = 20}] = ?B(C):get_sessions(),
    [#session{usr = USR, sid = Sid, priority = 20}] = ?B(C):get_sessions(S),
    [#session{priority = 20}] = ?B(C):get_sessions(U, S).

session_info_is_stored(C) ->
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    [#session{sid = Sid, info = #{key1 := val1}}]
     = ?B(C):get_sessions(U,S).

session_info_is_updated_if_keys_match(C) ->
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_opened(Sid, USR, 1, [{key1, val2}]),

    [#session{sid = Sid, info = #{key1 := val2}}]
     = ?B(C):get_sessions(U,S).

%% Same resource but different sids
session_info_is_updated_properly_if_session_conflicts(C) ->
    %% We use 2 SIDs here
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    %% Sid2 > Sid in this case, because SIDs have a timestamp in them
    %% We cannot test store_info for Sid2 though, because it has a different pid
    Sid2 = make_sid(),

    %% Two sessions for the same USR are registered after that:
    given_session_opened(Sid, USR, 1, [{key1, val1}, {key2, a}]),
    given_session_opened(Sid2, USR, 1, [{key1, val2}, {key3, b}]),

    %% Each call to open_session overwrites the previous data without merging.
    %% The current version of mongoose_c2s calls open_session only once per SID.
    %% Still, we want to test what happens if we call open_session the second time.
    when_session_opened(Sid, USR, 1, [{key1, val3}, {key4, c}]),

    [#session{sid = Sid, info = Info1}, #session{sid = Sid2, info = Info2}]
        = lists:keysort(#session.sid, ?B(C):get_sessions(U, S)),
    [{key1, val3}, {key4, c}] = maps:to_list(Info1),
    [{key1, val2}, {key3, b}] = maps:to_list(Info2).

session_info_is_extended_if_new_keys_present(C) ->
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_opened(Sid, USR, 1, [{key1, val1}, {key2, val2}]),

    [#session{sid = Sid, info = #{key1 := val1, key2 := val2}}]
     = ?B(C):get_sessions(U,S).

session_info_keys_not_truncated_if_session_opened_with_empty_infolist(C) ->
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    %% Should not be called twice in the real life
    when_session_opened(Sid, USR, 1, []),

    [#session{sid = Sid, info = #{}}]
     = ?B(C):get_sessions(U,S).


kv_can_be_stored_for_session(C) ->
    {Sid, {U, S, R} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),
    when_session_info_stored(Sid, U, S, R, {key2, newval}),
    ?assertMatch([#session{sid = Sid, info = #{key1 := val1, key2 := newval}}],
                 ?B(C):get_sessions(U,S)).

kv_can_be_updated_for_session(C) ->
    {Sid, {U, S, R} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_info_stored(Sid, U, S, R, {key2, newval}),
    when_session_info_stored(Sid, U, S, R, {key2, override}),

    ?assertMatch([#session{sid = Sid, info = #{key1 := val1, key2 := override}}],
                 ?B(C):get_sessions(U, S)).

kv_can_be_removed_for_session(C) ->
    {Sid, {U, S, R} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_info_stored(Sid, U, S, R, {key2, newval}),

    [#session{sid = Sid, info = #{key1 := val1, key2 := newval}}]
     = ?B(C):get_sessions(U, S),

    when_session_info_removed(Sid, U, S, R, key2),

    [#session{sid = Sid, info = #{key1 := val1}}]
     = ?B(C):get_sessions(U, S),

    when_session_info_removed(Sid, U, S, R, key1),

    [#session{sid = Sid, info = #{}}]
     = ?B(C):get_sessions(U, S).

store_info_sends_message_to_the_session_owner(C) ->
    SID = {erlang:system_time(microsecond), self()},
    U = <<"alice2">>,
    S = <<"localhost">>,
    R = <<"res1">>,
    Session = #session{sid = SID, usr = {U, S, R}, us = {U, S}, priority = 1, info = #{}},
    %% Create session in one process
    ?B(C):set_session(U, S, R, Session),
    %% but call store_info from another process
    JID = jid:make_noprep(U, S, R),
    spawn_link(fun() -> ejabberd_sm:store_info(JID, SID, cc, undefined) end),
    %% The original process receives a message
    receive
        {'$gen_cast', {async_with_state,
                       _Fun,
                       [SID, #jid{luser = User, lserver = Server, lresource = Resource},
                        K, V]}} ->
            ?eq(U, User),
            ?eq(S, Server),
            ?eq(R, Resource),
            ?eq({cc, undefined}, {K, V}),
            ok;
        Message ->
            ct:fail("unexpected message: ~p", [Message])
    after 5000 ->
        ct:fail("store_info_sends_message_to_the_session_owner=timeout")
    end.

remove_info_sends_message_to_the_session_owner(C) ->
    SID = {erlang:timestamp(), self()},
    U = <<"alice2">>,
    S = <<"localhost">>,
    R = <<"res1">>,
    Session = #session{sid = SID, usr = {U, S, R}, us = {U, S}, priority = 1, info = #{}},
    %% Create session in one process
    ?B(C):set_session(U, S, R, Session),
    %% but call remove_info from another process
    JID = jid:make_noprep(U, S, R),
    spawn_link(fun() -> ejabberd_sm:remove_info(JID, SID, cc) end),
    %% The original process receives a message
    receive
        {'$gen_cast', {async_with_state,
                       _Fun,
                       [SID, #jid{luser = User, lserver = Server, lresource = Resource},
                        Key, undefined]}} ->
            ?eq(U, User),
            ?eq(S, Server),
            ?eq(R, Resource),
            ?eq(cc, Key),
            ok;
        Message ->
            ct:fail("unexpected message: ~p", [Message])
    after 5000 ->
        ct:fail("remove_info_sends_message_to_the_session_owner=timeout")
    end.

delete_session(C) ->
    {Sid, {U, S, R} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR),
    verify_session_opened(C, Sid, USR),

    ?B(C):delete_session(Sid, U, S, R),

    [] = ?B(C):get_sessions(),
    [] = ?B(C):get_sessions(S),
    [] = ?B(C):get_sessions(U, S),
    [] = ?B(C):get_sessions(U, S, R).



clean_up(C) ->
    UsersWithManyResources = generate_many_random_res(5, 3, [<<"localhost">>, <<"otherhost">>]),
    [given_session_opened(Sid, USR) || {Sid, USR} <- UsersWithManyResources],
    ?B(C):cleanup(node()),
    %% give sm backend some time to clean all sessions
    ensure_empty(C, 10, ?B(C):get_sessions()).

clean_up_with_colon_in_resource(C) ->
    %% Test that resources containing colons are handled correctly during cleanup
    Users = [generate_user(<<"user1">>, <<"localhost">>, <<"device:with:colons">>),
             generate_user(<<"user2">>, <<"localhost">>, <<"smile:)">>),
             generate_user(<<"user3">>, <<"otherhost">>, <<"a]b:c[d">>)],
    [given_session_opened(Sid, USR) || {Sid, USR} <- Users],
    ?B(C):cleanup(node()),
    %% give sm backend some time to clean all sessions
    ensure_empty(C, 10, ?B(C):get_sessions()).

%% Redis-only test: verify backward compatibility with old s4: key format
clean_up_s4_backward_compatibility(_C) ->
    %% This test verifies that old s4: format keys are still parsed correctly
    %% during cleanup (for rolling upgrades)
    U = <<"testuser">>,
    S = <<"localhost">>,
    R = <<"resource">>,
    Sid = make_sid(),
    %% Create session using the new format (s5:)
    Session = #session{sid = Sid, usr = {U, S, R}, us = {U, S}, priority = 1, info = #{}},
    ejabberd_sm_redis:set_session(U, S, R, Session),
    %% Manually insert an old-format s4: key into Redis to simulate legacy data
    OldFormatKey = iolist_to_binary(["s4:", U, ":", S, ":", R, ":", term_to_binary(Sid)]),
    mongoose_redis:cmd(["SADD", n(node()), OldFormatKey]),
    %% Also add the session data so delete_session can find it
    Sid2 = make_sid(),
    Session2 = #session{sid = Sid2, usr = {U, S, R}, us = {U, S}, priority = 1, info = #{}},
    BSession2 = term_to_binary(Session2),
    mongoose_redis:cmd(["SADD", hash(U, S), BSession2]),
    mongoose_redis:cmd(["SADD", hash(U, S, R), BSession2]),
    %% Cleanup should handle both s4: and s5: keys
    ejabberd_sm_redis:cleanup(node()),
    %% Verify cleanup succeeded (node set should be empty)
    [] = mongoose_redis:cmd(["SMEMBERS", n(node())]).

parse_session_key_s5_format(_Config) ->
    %% Test new s5: format with hex-encoded resource
    U = <<"user">>,
    S = <<"server">>,
    R = <<"resource">>,
    Sid = {erlang:timestamp(), self()},
    HexResource = binary:encode_hex(R, lowercase),
    Key = iolist_to_binary(["s5:", U, ":", S, ":", HexResource, ":", term_to_binary(Sid)]),
    {U, S, R, Sid} = ejabberd_sm_redis:parse_session_key(Key).

parse_session_key_s4_format(_Config) ->
    %% Test old s4: format parsing
    U = <<"user">>,
    S = <<"server">>,
    R = <<"resource">>,
    Sid = {erlang:timestamp(), self()},
    Key = iolist_to_binary(["s4:", U, ":", S, ":", R, ":", term_to_binary(Sid)]),
    {U, S, R, Sid} = ejabberd_sm_redis:parse_session_key(Key).

parse_session_key_s4_format_with_colon_in_sid(_Config) ->
    %% Test s4: format where BinarySID happens to contain colon bytes
    %% The parser handles this by joining SIDEncoded parts back together
    U = <<"user">>,
    S = <<"server">>,
    R = <<"resource">>,
    Sid = {erlang:timestamp(), self()},
    BinarySid = term_to_binary(Sid),
    Key = iolist_to_binary(["s4:", U, ":", S, ":", R, ":", BinarySid]),
    {U, S, R, Sid} = ejabberd_sm_redis:parse_session_key(Key).

ensure_empty(_C, 0, Sessions) ->
    [] = Sessions;
ensure_empty(C, N, Sessions) ->
    case Sessions of
        [] ->
            ok;
        _ ->
            timer:sleep(50),
            ensure_empty(C, N-1, ?B(C):get_sessions())
    end.

too_many_sessions(_C) ->
    %% Max sessions set to ?MAX_USER_SESSIONS in init_per_testcase
    UserSessions = [generate_random_user(<<"a">>, <<"localhost">>) ||
                       _ <- lists:seq(1, ?MAX_USER_SESSIONS + 1)],
    [given_session_opened(Sid, USR) || {Sid, USR} <- UserSessions],

    receive
        {forwarded, _Sid, {'$gen_cast', {exit, {replaced, _Pid}}}} ->
            ok;
        Message ->
            ct:fail("Unexpected message: ~p", [Message])
    after 10 ->
        ct:fail("replaced message not sent")
    end.



unique_count(_C) ->
    UsersWithManyResources = generate_many_random_res(5, 3, [<<"localhost">>, <<"otherhost">>]),
    [given_session_opened(Sid, USR) || {Sid, USR} <- UsersWithManyResources],
    USDict = get_unique_us_dict(UsersWithManyResources),
    UniqueCount = ejabberd_sm:get_unique_sessions_number(),
    UniqueCount = dict:size(USDict).

unload_meck() ->
    meck:unload(acl),
    meck:unload(gen_hook),
    meck:unload(mongoose_domain_api),
    catch ets:delete(test_c2s_info),
    catch meck:unload(mongoose_c2s).

set_test_case_meck(MaxUserSessions, MeckC2s) ->
    ets:new(test_c2s_info, [public, named_table]),
    meck:new(acl, []),
    meck:expect(acl, match_rule, fun(_, _, _, _) -> MaxUserSessions end),
    meck:new(gen_hook, []),
    meck:expect(gen_hook, run_fold, fun(_, _, Acc, _) -> {ok, Acc} end),
    meck:new(mongoose_domain_api, []),
    meck:expect(mongoose_domain_api, get_domain_host_type, fun(H) -> {ok, H} end),
    do_meck_c2s(MeckC2s).

do_meck_c2s(false) ->
    ok;
do_meck_c2s(true) ->
    %% Very simple mock, not even reproducing async behaviour
    %% It is for a limited use only, for more complex tests use a real c2s process (i.e. probably big tests)
    meck:new(mongoose_c2s, [passthrough]),
    meck:expect(mongoose_c2s, async_with_state, fun async_with_state/3),
    meck:expect(mongoose_c2s, get_info, fun get_info/1),
    meck:expect(mongoose_c2s, set_info, fun set_info/2),
    %% Just return same thing all the time
    meck:expect(mongoose_c2s, get_mod_state, fun(_C2sState, _Mod) -> {error, not_found} end).

async_with_state(Pid, Fun, Args) ->
    apply(Fun, [{ministate, Pid}|Args]),
    ok.

get_info({ministate, Pid}) ->
    case ets:lookup(test_c2s_info, Pid) of
        [] ->
            #{};
        [{Pid, Info}] ->
            Info
    end.

set_info({ministate, Pid} = S, Info) ->
    ets:insert(test_c2s_info, {Pid, Info}),
    S.

set_test_case_meck_unique_count_crash(Backend) ->
    F = get_fun_for_unique_count(Backend),
    meck:new(Backend, []),
    meck:expect(Backend, unique_count, F).

get_fun_for_unique_count(ejabberd_sm_mnesia) ->
    fun() ->
        mnesia:abort({badarg,[session,{{1442,941593,580189},list_to_pid("<0.23291.6>")}]})
    end;
get_fun_for_unique_count(ejabberd_sm_cets) ->
    fun() -> error(oops) end;
get_fun_for_unique_count(ejabberd_sm_redis) ->
    fun() ->
        %% The code below is on purpose, it's to crash with badarg reason
        length({error, timeout})
    end.

make_sid() ->
    %% A sid consists of a timestamp and a pid.
    %% Timestamps can repeat, and a unique pid is needed to avoid sid duplication.
    TestPid = self(),
    Pid = spawn_link(fun() ->
                             Sid = ejabberd_sm:make_new_sid(),
                             TestPid ! {sid, Sid},
                             forward_messages(Sid, TestPid)
                     end),
    receive
        {sid, Sid = {_, Pid}} -> Sid
    after
        1000 -> ct:fail("Timeout waiting for sid")
    end.

forward_messages(Sid, Target) ->
    receive
        Msg -> Target ! {forwarded, Sid, Msg}
    end,
    forward_messages(Sid, Target).

given_session_opened(Sid, USR) ->
    given_session_opened(Sid, USR, 1).

given_session_opened(Sid, {U, S, R}, Priority) ->
    given_session_opened(Sid, {U, S, R}, Priority, []).

given_session_opened(Sid, {U, S, R}, Priority, Info) ->
    {_, Pid} = Sid,
    Map = maps:from_list(Info),
    %% open_session is called by c2s usually, but here in tests the function is called by the test.
    %% we still need to remember the initial info for tests to work though.
    ets:insert_new(test_c2s_info, {Pid, Map}),
    JID = jid:make_noprep(U, S, R),
    ejabberd_sm:open_session(S, Sid, JID, Priority, Map).

when_session_opened(Sid, {U, S, R}, Priority, Info) ->
    given_session_opened(Sid, {U, S, R}, Priority, Info).

when_session_info_stored(SID, U, S, R, {K, V}) ->
    JID = jid:make_noprep(U, S, R),
    ejabberd_sm:store_info(JID, SID, K, V).

when_session_info_removed(SID, U, S, R, Key) ->
    JID = jid:make_noprep(U, S, R),
    ejabberd_sm:remove_info(JID, SID, Key).

verify_session_opened(C, Sid, USR) ->
    do_verify_session_opened(?B(C), Sid, USR).

do_verify_session_opened(ejabberd_sm_mnesia, Sid, {U, S, R} = USR) ->
    general_session_check(ejabberd_sm_mnesia, Sid, USR, U, S, R);
do_verify_session_opened(ejabberd_sm_cets, Sid, {U, S, R} = USR) ->
    general_session_check(ejabberd_sm_cets, Sid, USR, U, S, R);
do_verify_session_opened(ejabberd_sm_redis, Sid, {U, S, R} = USR) ->
    UHash = iolist_to_binary(hash_v2(U, S, R, Sid)),
    Hashes = mongoose_redis:cmd(["SMEMBERS", n(node())]),
    true = lists:member(UHash, Hashes),
    SessionsUSEncoded = mongoose_redis:cmd(["SMEMBERS", hash(U, S)]),
    SessionsUS = [binary_to_term(Entry) || Entry <- SessionsUSEncoded],
    true = lists:keymember(Sid, 2, SessionsUS),
    [SessionUSREncoded] = mongoose_redis:cmd(["SMEMBERS", hash(U, S, R)]),
    SessionUSR = binary_to_term(SessionUSREncoded),
    #session{sid = Sid} = SessionUSR,
    general_session_check(ejabberd_sm_redis, Sid, USR, U, S, R).

verify_session_opened(C, U, S, Resources) ->
    Sessions = ?B(C):get_sessions(U, S),
    F = fun({Sid, USR}) ->
        #session{} = Session = lists:keyfind(Sid, #session.sid, Sessions),
        Session == lists:keyfind(USR, #session.usr, Sessions)
    end,
    true = lists:all(F, Resources).

general_session_check(M, Sid, USR, U, S, R) ->
    [#session{sid = Sid, usr = USR, us = {U, S}}] = M:get_sessions(U, S, R).

clean_sessions(C) ->
    case ?B(C) of
        ejabberd_sm_mnesia ->
            mnesia:clear_table(session);
        ejabberd_sm_redis ->
            mongoose_redis:cmd(["FLUSHALL"]);
        ejabberd_sm_cets ->
            ets:delete_all_objects(cets_session)
    end.

generate_random_user(S) ->
    U = binary:encode_hex(crypto:strong_rand_bytes(5)),
    generate_random_user(U, S).

generate_random_user(U, S) ->
    R = binary:encode_hex(crypto:strong_rand_bytes(5)),
    generate_user(U, S, R).

generate_user(U, S, R) ->
    Sid = make_sid(),
    {Sid, {U, S, R}}.

generate_many_random_users(PerServerCount, Servers) ->
    Users = [generate_random_users(PerServerCount, Server) || Server <- Servers],
    lists:flatten(Users).

generate_random_users(Count, Server) ->
    [generate_random_user(Server) || _ <- lists:seq(1, Count)].

generate_many_random_res(UsersPerServer, ResourcesPerUser, Servers) ->
    Usernames = [binary:encode_hex(crypto:strong_rand_bytes(5)) || _ <- lists:seq(1, UsersPerServer)],
    [generate_random_user(U, S) || U <- Usernames, S <- Servers, _ <- lists:seq(1, ResourcesPerUser)].

get_unique_us_dict(USRs) ->
    F = fun({_, {U, S, _}} = I, SetAcc) ->
        dict:append({U, S}, I, SetAcc)
    end,
    lists:foldl(F, dict:new(), USRs).

%% Taken from ejabberd_sm_redis

-spec hash(binary()) -> iolist().
hash(Val1) ->
    ["s3:*:", Val1, ":*"].


-spec hash(binary(), binary()) -> iolist().
hash(Val1, Val2) ->
    ["s2:", Val1, ":", Val2].


-spec hash(binary(), binary(), binary()) -> iolist().
hash(Val1, Val2, Val3) ->
    ["s3:", Val1, ":", Val2, ":", Val3].


-spec hash(binary(), binary(), binary(), binary()) -> iolist().
hash(Val1, Val2, Val3, Val4) ->
    ["s4:", Val1, ":", Val2, ":", Val3, ":", term_to_binary(Val4)].

%% New format with hex-encoded resource (matches ejabberd_sm_redis:hash_v2/4)
-spec hash_v2(binary(), binary(), binary(), binary()) -> iolist().
hash_v2(User, Server, Resource, SID) ->
    ["s5:", User, ":", Server, ":", binary:encode_hex(Resource, lowercase), ":", term_to_binary(SID)].


-spec n(atom()) -> iolist().
n(Node) ->
    ["n:", atom_to_list(Node)].


is_redis_running() ->
    ct:log("Checking if Redis is running..."),
    % Try plain connection first (for local development)
    case eredis:start_link([{host, "127.0.0.1"}]) of
        {ok, Client} ->
            ct:log("Plain TCP connection to Redis succeeded"),
            case check_redis_ping(Client, plain) of
                {true, plain} ->
                    {true, plain};
                false ->
                    %% tcp_closed case handled inside check_redis_ping returns false
                    is_redis_running_tls()
            end;
        PlainError ->
            ct:log("Plain TCP connection to Redis failed: ~p, trying TLS...", [PlainError]),
            is_redis_running_tls()
    end.

is_redis_running_tls() ->
    ct:log("Attempting TLS connection to Redis on 127.0.0.1:6379"),
    try
        TlsOpts = just_tls:make_client_opts(#{verify_mode => none}),
        try_redis_tls_connection(TlsOpts)
    catch
        Error ->
            ct:log("TLS connection attempt failed with exception: ~p", [Error]),
            false
    end.

try_redis_tls_connection(TlsOpts) ->
    case eredis:start_link([{host, "127.0.0.1"}, {port, 6379}, {tls, TlsOpts}]) of
        {ok, Client} ->
            ct:log("TLS connection to Redis succeeded"),
            check_redis_ping(Client, tls);
        TlsError ->
            ct:log("TLS connection to Redis failed: ~p", [TlsError]),
            false
    end.

check_redis_ping(Client, ConnType) ->
    Result = eredis:q(Client, [<<"PING">>], 5000),
    eredis:stop(Client),
    case Result of
        {ok, <<"PONG">>} ->
            ct:log("Redis ~p connection: PING successful", [ConnType]),
            {true, ConnType};
        Error ->
            ct:log("Redis ~p connection: PING failed with ~p", [ConnType, Error]),
            false
    end.

setup_sm(Config) ->
    mongoose_config:set_opt(sm_backend, sm_backend(?config(backend, Config))),
    set_meck(),
    ejabberd_sm:start_link(),
    case ?config(backend, Config) of
        ejabberd_sm_redis ->
            mongoose_redis:cmd(["FLUSHALL"]);
        ejabberd_sm_mnesia ->
            ok;
        ejabberd_sm_cets ->
            ok
    end.

terminate_sm() ->
    gen_server:stop(ejabberd_sm),
    mongoose_config:unset_opt(sm_backend).

sm_backend(ejabberd_sm_redis) -> redis;
sm_backend(ejabberd_sm_mnesia) -> mnesia;
sm_backend(ejabberd_sm_cets) -> cets.

set_meck() ->
    meck:expect(gen_hook, add_handler, fun(_, _, _, _, _) -> ok end),
    meck:expect(gen_hook, add_handlers, fun(_) -> ok end),
    ok.
