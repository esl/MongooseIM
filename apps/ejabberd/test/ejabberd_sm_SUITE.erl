-module(ejabberd_sm_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ejabberd/src/ejabberd_c2s.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-compile([export_all]).


-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(ne(E, I), ?assert(E =/= I)).

-define(B(C), (proplists:get_value(backend, C))).
-define(MAX_USER_SESSIONS, 2).


all() -> [{group, mnesia}, {group, redis}].

init_per_suite(C) ->
    application:start(stringprep),
    application:ensure_all_started(exometer),
    F = fun() ->
        ejabberd_sm_backend_sup:start_link(),
        receive stop -> ok end
    end,
    Pid = spawn(F),
    [{pid, Pid} | C].

end_per_suite(C) ->
    Pid = ?config(pid, C),
    Pid ! stop,
    application:stop(exometer),
    application:stop(exometer_core).

groups() ->
    [{mnesia, [], tests()},
     {redis, [], tests()}].

tests() ->
    [open_session,
     get_full_session_list,
     get_vh_session_list,
     get_sessions_2,
     get_sessions_3,
     update_session,
     delete_session,
     clean_up,
     too_much_sessions,
     unique_count,
     unique_count_while_removing_entries,
     session_info_is_stored,
     session_info_is_updated_if_keys_match,
     session_info_is_extended_if_new_keys_present,
     session_info_keys_not_truncated_if_session_opened_with_empty_infolist,
     kv_can_be_stored_for_session,
     kv_can_be_updated_for_session
    ].

init_per_group(mnesia, Config) ->
    application:start(mnesia),
    set_meck({mnesia, []}),
    ejabberd_sm:start_link(),
    unload_meck(),
    [{backend, ejabberd_sm_mnesia} | Config];
init_per_group(redis, Config) ->
    init_redis_group(is_redis_running(), Config).

init_redis_group(true, Config) ->
    set_meck({redis, [{pool_size, 3}, {worker_config, [{host, "localhost"}, {port, 6379}]}]}),
    ejabberd_sm:start_link(),
    ejabberd_redis:cmd(["FLUSHALL"]),
    unload_meck(),
    [{backend, ejabberd_sm_redis} | Config];
init_redis_group(_, _) ->
    {skip, "redis not running"}.

end_per_group(_, Config) ->
    Config.

init_per_testcase(too_much_sessions, Config) ->
    set_test_case_meck(?MAX_USER_SESSIONS),
    Config;
init_per_testcase(_, Config) ->
    set_test_case_meck(infinity),
    Config.

end_per_testcase(_, Config) ->
    clean_sessions(Config),
    meck:unload(acl),
    meck:unload(ejabberd_config),
    meck:unload(ejabberd_hooks),
    Config.

open_session(C) ->
    {Sid, USR} =  generate_random_user(<<"localhost">>),
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

update_session(C) ->
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR),
    verify_session_opened(C, Sid, USR),

    given_session_opened(Sid, USR, 20),
    verify_session_opened(C, Sid, USR),

    [{USR, Sid, 20, _}] = ?B(C):get_sessions(),
    [{USR, Sid, 20, _}] = ?B(C):get_sessions(S),
    [#session{priority = 20}] = ?B(C):get_sessions(U, S).

session_info_is_stored(C) ->
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    [#session{sid = Sid, info = [{key1, val1}]}]
     = ?B(C):get_sessions(U,S).

session_info_is_updated_if_keys_match(C) ->
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_opened(Sid, USR, 1, [{key1, val2}]),

    [#session{sid = Sid, info = [{key1, val2}]}]
     = ?B(C):get_sessions(U,S).

session_info_is_extended_if_new_keys_present(C) ->
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_opened(Sid, USR, 1, [{key1, val1}, {key2, val2}]),

    [#session{sid = Sid, info = [{key1, val1}, {key2, val2}]}]
     = ?B(C):get_sessions(U,S).

session_info_keys_not_truncated_if_session_opened_with_empty_infolist(C) ->
    {Sid, {U, S, _} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_opened(Sid, USR, 1, []),

    [#session{sid = Sid, info = [{key1, val1}]}]
     = ?B(C):get_sessions(U,S).


kv_can_be_stored_for_session(C) ->
    {Sid, {U, S, R} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_info_stored(U, S, R, {key2, newval}),

    [#session{sid = Sid, info = [{key1, val1}, {key2, newval}]}]
     = ?B(C):get_sessions(U,S).

kv_can_be_updated_for_session(C) ->
    {Sid, {U, S, R} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_info_stored(U, S, R, {key2, newval}),
    when_session_info_stored(U, S, R, {key2, override}),

    [#session{sid = Sid, info = [{key1, val1}, {key2, override}]}]
     = ?B(C):get_sessions(U,S).


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

too_much_sessions(C) ->
    %% Max sessions set to ?MAX_USER_SESSIONS in init_per_testcase
    UserSessions = [generate_random_user(<<"a">>, <<"localhost">>) || _ <- lists:seq(1, ?MAX_USER_SESSIONS)],
    {AddSid, AddUSR} = generate_random_user(<<"a">>, <<"localhost">>),

    [given_session_opened(Sid, USR) || {Sid, USR} <- UserSessions],

    given_session_opened(AddSid, AddUSR),

    receive
        replaced ->
            ok
    after 10 ->
        ct:fail("replaced message not sent")
    end.



unique_count(C) ->
    UsersWithManyResources = generate_many_random_res(5, 3, [<<"localhost">>, <<"otherhost">>]),
    [given_session_opened(Sid, USR) || {Sid, USR} <- UsersWithManyResources],
    USDict = get_unique_us_dict(UsersWithManyResources),
    UniqueCount = ejabberd_sm:get_unique_sessions_number(),
    UniqueCount = dict:size(USDict).


unique_count_while_removing_entries(C) ->
    unique_count(C),
    UniqueCount = ejabberd_sm:get_unique_sessions_number(),
    %% Register more sessions and mock the crash
    UsersWithManyResources = generate_many_random_res(10, 3, [<<"localhost">>, <<"otherhost">>]),
    [given_session_opened(Sid, USR) || {Sid, USR} <- UsersWithManyResources],
    set_test_case_meck_unique_count_crash(?B(C)),
    USDict = get_unique_us_dict(UsersWithManyResources),
    %% Check if unique count equals prev cached value
    UniqueCount = ejabberd_sm:get_unique_sessions_number(),
    meck:unload(?B(C)),
    true = UniqueCount /= dict:size(USDict) + UniqueCount.

set_meck(SMBackend) ->
    meck:new(ejabberd_config, []),
    meck:expect(ejabberd_config, get_global_option,
                fun(sm_backend) -> SMBackend;
                    (hosts) -> [<<"localhost">>] end),
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),

    meck:new(ejabberd_hooks, []),
    meck:expect(ejabberd_hooks, add, fun(_, _, _, _, _) -> ok end),

    meck:new(ejabberd_commands, []),
    meck:expect(ejabberd_commands, register_commands, fun(_) -> ok end),

    ok.

unload_meck() ->
    meck:unload(ejabberd_config),
    meck:unload(ejabberd_hooks),
    meck:unload(ejabberd_commands).

set_test_case_meck(MaxUserSessions) ->
    meck:new(ejabberd_config, []),
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),
    meck:new(acl, []),
    meck:expect(acl, match_rule, fun(_, _, _) -> MaxUserSessions end),
    meck:new(ejabberd_hooks, []),
    meck:expect(ejabberd_hooks, run, fun(_, _, _) -> ok end).

set_test_case_meck_unique_count_crash(Backend) ->
    F = get_fun_for_unique_count(Backend),
    meck:new(Backend, []),
    meck:expect(Backend, unique_count, F).

get_fun_for_unique_count(ejabberd_sm_mnesia) ->
    fun() ->
        mnesia:abort({badarg,[session,{{1442,941593,580189},list_to_pid("<0.23291.6>")}]})
    end;
get_fun_for_unique_count(ejabberd_sm_redis) ->
    fun() ->
        length({error, timeout})
    end.

make_sid() ->
    {now(), self()}.

given_session_opened(Sid, USR) ->
    given_session_opened(Sid, USR, 1).

given_session_opened(Sid, {U, S, R}, Priority) ->
    given_session_opened(Sid, {U,S,R}, Priority, []).

given_session_opened(Sid, {U,S,R}, Priority, Info) ->
    ejabberd_sm:open_session(Sid, U, S, R, Priority, Info).

when_session_opened(Sid, {U,S,R}, Priority, Info) ->
    given_session_opened(Sid, {U,S,R}, Priority, Info).

when_session_info_stored(U, S, R, {_,_}=KV) ->
    ejabberd_sm:store_info(U, S, R, KV).

verify_session_opened(C, Sid, USR) ->
    do_verify_session_opened(?B(C), Sid, USR).

do_verify_session_opened(ejabberd_sm_mnesia, Sid, {U, S, R} = USR) ->
    general_session_check(ejabberd_sm_mnesia, Sid, USR, U, S, R);
do_verify_session_opened(ejabberd_sm_redis, Sid, {U, S, R} = USR) ->
    UHash = iolist_to_binary(hash(U, S, R, Sid)),
    Hashes = ejabberd_redis:cmd(["SMEMBERS", n(node())]),
    true = lists:member(UHash, Hashes),
    SessionsUSEncoded = ejabberd_redis:cmd(["SMEMBERS", hash(U, S)]),
    SessionsUS = [binary_to_term(Entry) || Entry <- SessionsUSEncoded],
    true = lists:keymember(Sid, 2, SessionsUS),
    [SessionUSREncoded] = ejabberd_redis:cmd(["SMEMBERS", hash(U, S, R)]),
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
            ejabberd_redis:cmd(["FLUSHALL"])
    end.

generate_random_user(S) ->
    U = base16:encode(crypto:rand_bytes(5)),
    generate_random_user(U, S).

generate_random_user(U, S) ->
    R = base16:encode(crypto:rand_bytes(5)),
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
    Usernames = [base16:encode(crypto:rand_bytes(5)) || _ <- lists:seq(1, UsersPerServer)],
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


-spec n(atom()) -> iolist().
n(Node) ->
    ["n:", atom_to_list(Node)].


is_redis_running() ->
    [] =/= os:cmd("ps aux | grep '[r]edis'").
