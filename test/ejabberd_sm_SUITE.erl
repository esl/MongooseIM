-module(ejabberd_sm_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("ejabberd_c2s.hrl").
-include("mongoose.hrl").
-include("jid.hrl").
-include_lib("session.hrl").
-compile([export_all]).


-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(ne(E, I), ?assert(E =/= I)).

-define(B(C), (proplists:get_value(backend, C))).
-define(MAX_USER_SESSIONS, 2).


all() -> [{group, mnesia}, {group, redis}].

init_per_suite(C) ->
    ok = stringprep:start(),
    application:ensure_all_started(exometer_core),
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
     session_is_updated_when_created_twice,
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
     kv_can_be_updated_for_session,
     kv_can_be_removed_for_session,
     store_info_sends_message_to_the_session_owner,
     remove_info_sends_message_to_the_session_owner,
     cannot_reproduce_race_condition_in_store_info
    ].

init_per_group(mnesia, Config) ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    [{backend, ejabberd_sm_mnesia} | Config];
init_per_group(redis, Config) ->
    init_redis_group(is_redis_running(), Config).

init_redis_group(true, Config) ->
    Self = self(),
    proc_lib:spawn(fun() ->
                  register(test_helper, self()),
                  mongoose_wpool:ensure_started(),
                  % This would be started via outgoing_pools in normal case
                  Pool = {redis, global, default, [{strategy, random_worker}, {workers, 10}], []},
                  mongoose_wpool:start_configured_pools([Pool], []),
                  Self ! ready,
                  receive stop -> ok end
          end),
    receive ready -> ok after timer:seconds(30) -> ct:fail(test_helper_not_ready) end,
    [{backend, ejabberd_sm_redis} | Config];
init_redis_group(_, _) ->
    {skip, "redis not running"}.

end_per_group(mnesia, Config) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    Config;
end_per_group(_, Config) ->
    whereis(test_helper) ! stop,
    Config.

init_per_testcase(too_much_sessions, Config) ->
    set_test_case_meck(?MAX_USER_SESSIONS),
    setup_sm(Config),
    Config;
init_per_testcase(_, Config) ->
    set_test_case_meck(infinity),
    setup_sm(Config),
    Config.

end_per_testcase(_, Config) ->
    clean_sessions(Config),
    terminate_sm(),
    unload_meck(),
    Config.

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

    ?assertMatch([#session{sid = Sid, info = [{key1, val1}, {key2, newval}]}],
                 ?B(C):get_sessions(U,S)).

kv_can_be_updated_for_session(C) ->
    {Sid, {U, S, R} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_info_stored(U, S, R, {key2, newval}),
    when_session_info_stored(U, S, R, {key2, override}),

    ?assertMatch([#session{sid = Sid, info = [{key1, val1}, {key2, override}]}],
                 ?B(C):get_sessions(U, S)).

kv_can_be_removed_for_session(C) ->
    {Sid, {U, S, R} = USR} = generate_random_user(<<"localhost">>),
    given_session_opened(Sid, USR, 1, [{key1, val1}]),

    when_session_info_stored(U, S, R, {key2, newval}),

    [#session{sid = Sid, info = [{key1, val1}, {key2, newval}]}]
     = ?B(C):get_sessions(U, S),

    when_session_info_removed(U, S, R, key2),

    [#session{sid = Sid, info = [{key1, val1}]}]
     = ?B(C):get_sessions(U, S),

    when_session_info_removed(U, S, R, key1),

    [#session{sid = Sid, info = []}]
     = ?B(C):get_sessions(U, S).

cannot_reproduce_race_condition_in_store_info(C) ->
    ok = try_to_reproduce_race_condition(C).

store_info_sends_message_to_the_session_owner(C) ->
    SID = {erlang:timestamp(), self()},
    U = <<"alice2">>,
    S = <<"localhost">>,
    R = <<"res1">>,
    Session = #session{sid = SID, usr = {U, S, R}, us = {U, S}, priority = 1, info = []},
    %% Create session in one process
    ?B(C):create_session(U, S, R, Session),
    %% but call store_info from another process
    JID = jid:make_noprep(U, S, R),
    spawn_link(fun() -> ejabberd_sm:store_info(JID, {cc, undefined}) end),
    %% The original process receives a message
    receive {store_session_info,
             #jid{luser = User, lserver = Server, lresource = Resource},
             KV, _FromPid} ->
        ?eq(U, User),
        ?eq(S, Server),
        ?eq(R, Resource),
        ?eq({cc, undefined}, KV),
        ok
        after 5000 ->
            ct:fail("store_info_sends_message_to_the_session_owner=timeout")
    end.

remove_info_sends_message_to_the_session_owner(C) ->
    SID = {erlang:timestamp(), self()},
    U = <<"alice2">>,
    S = <<"localhost">>,
    R = <<"res1">>,
    Session = #session{sid = SID, usr = {U, S, R}, us = {U, S}, priority = 1, info = []},
    %% Create session in one process
    ?B(C):create_session(U, S, R, Session),
    %% but call remove_info from another process
    JID = jid:make_noprep(U, S, R),
    spawn_link(fun() -> ejabberd_sm:remove_info(JID, cc) end),
    %% The original process receives a message
    receive {remove_session_info,
             #jid{luser = User, lserver = Server, lresource = Resource},
             Key, _FromPid} ->
        ?eq(U, User),
        ?eq(S, Server),
        ?eq(R, Resource),
        ?eq(cc, Key),
        ok
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

too_much_sessions(_C) ->
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



unique_count(_C) ->
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

unload_meck() ->
    meck:unload(acl),
    meck:unload(ejabberd_config),
    meck:unload(ejabberd_hooks),
    meck:unload(ejabberd_commands).

set_test_case_meck(MaxUserSessions) ->
    meck:new(ejabberd_config, []),
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),
    meck:new(acl, []),
    meck:expect(acl, match_rule, fun(_, _, _) -> MaxUserSessions end),
    meck:new(ejabberd_hooks, []),
    meck:expect(ejabberd_hooks, run, fun(_, _, _) -> ok end),
    meck:expect(ejabberd_hooks, run_fold, fun(_, _, Acc, _) -> Acc end).

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
        %% The code below is on purpose, it's to crash with badarg reason
        length({error, timeout})
    end.

make_sid() ->
    {erlang:timestamp(), self()}.

given_session_opened(Sid, USR) ->
    given_session_opened(Sid, USR, 1).

given_session_opened(Sid, {U, S, R}, Priority) ->
    given_session_opened(Sid, {U, S, R}, Priority, []).

given_session_opened(Sid, {U, S, R}, Priority, Info) ->
    JID = jid:make_noprep(U, S, R),
    ejabberd_sm:open_session(Sid, JID, Priority, Info).

when_session_opened(Sid, {U, S, R}, Priority, Info) ->
    given_session_opened(Sid, {U, S, R}, Priority, Info).

when_session_info_stored(U, S, R, {_,_}=KV) ->
    JID = jid:make_noprep(U, S, R),
    ejabberd_sm:store_info(JID, KV).

when_session_info_removed(U, S, R, Key) ->
    JID = jid:make_noprep(U, S, R),
    ejabberd_sm:remove_info(JID, Key).

verify_session_opened(C, Sid, USR) ->
    do_verify_session_opened(?B(C), Sid, USR).

do_verify_session_opened(ejabberd_sm_mnesia, Sid, {U, S, R} = USR) ->
    general_session_check(ejabberd_sm_mnesia, Sid, USR, U, S, R);
do_verify_session_opened(ejabberd_sm_redis, Sid, {U, S, R} = USR) ->
    UHash = iolist_to_binary(hash(U, S, R, Sid)),
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
            mongoose_redis:cmd(["FLUSHALL"])
    end.

generate_random_user(S) ->
    U = base16:encode(crypto:strong_rand_bytes(5)),
    generate_random_user(U, S).

generate_random_user(U, S) ->
    R = base16:encode(crypto:strong_rand_bytes(5)),
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
    Usernames = [base16:encode(crypto:strong_rand_bytes(5)) || _ <- lists:seq(1, UsersPerServer)],
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
    case eredis:start_link() of
        {ok, Client} ->
            Result = eredis:q(Client, [<<"PING">>], 5000),
            eredis:stop(Client),
            case Result of
                {ok,<<"PONG">>} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

try_to_reproduce_race_condition(Config) ->
    SID = {erlang:timestamp(), self()},
    U = <<"alice">>,
    S = <<"localhost">>,
    R = <<"res1">>,
    Session = #session{sid = SID, usr = {U, S, R}, us = {U, S}, priority = 1, info = []},
    ?B(Config):create_session(U, S, R, Session),
    Parent = self(),
    %% Add some instrumentation to simulate race conditions
    %% The goal is to delete the session after other process reads it
    %% but before it updates it. In other words, delete a record
    %% between get_sessions and create_session in ejabberd_sm:store_info
    %% Step1 prepare concurrent processes
    DeleterPid = spawn_link(fun() ->
                                    receive start -> ok end,
                                    ?B(Config):delete_session(SID, U, S, R),
                                    Parent ! p1_done
                            end),
    SetterPid = spawn_link(fun() ->
                                   receive start -> ok end,
                                   when_session_info_stored(U, S, R, {cc, undefined}),
                                   Parent ! p2_done
                           end),
    %% Step2 setup mocking for some ejabbers_sm_mnesia functions
    meck:new(?B(Config), []),
    %% When the first get_sessions (run from ejabberd_sm:store_info)
    %% is executed, the start msg is sent to Deleter process
    %% Thanks to that, setter will get not empty list of sessions
    PassThrough3 = fun(A, B, C) ->
                           DeleterPid ! start,
                           meck:passthrough([A, B, C]) end,
    meck:expect(?B(Config), get_sessions, PassThrough3),
    %% Wait some time before setting the sessions
    %% so we are sure delete operation finishes
    meck:expect(?B(Config), create_session,
                fun(U1, S1, R1, Session1) ->
                        timer:sleep(100),
                        meck:passthrough([U1, S1, R1, Session1])
                end),
    PassThrough4 = fun(A, B, C, D) -> meck:passthrough([A, B, C, D]) end,
    meck:expect(?B(Config), delete_session, PassThrough4),
    %% Start the play from setter process
    SetterPid ! start,
    %% Wait for both process to finish
    receive p1_done -> ok end,
    receive p2_done -> ok end,
    meck:unload(?B(Config)),
    %% Session should not exist
    case ?B(Config):get_sessions(U, S, R) of
        [] ->
            ok;
        Other ->
            error_logger:error_msg("issue=reproduced, sid=~p, other=~1000p",
                                   [SID, Other]),
            {error, reproduced}
    end.

setup_sm(Config) ->
    case proplists:get_value(backend, Config) of
        ejabberd_sm_redis ->
            set_meck({redis, [{pool_size, 3}, {worker_config, [{host, "localhost"}, {port, 6379}]}]}),
            ejabberd_sm:start_link(),
            mongoose_redis:cmd(["FLUSHALL"]);
        ejabberd_sm_mnesia ->
            set_meck({mnesia, []}),
            ejabberd_sm:start_link()
    end.

terminate_sm() ->
    gen_server:stop(ejabberd_sm).

set_meck(SMBackend) ->
    meck:expect(ejabberd_config, get_global_option,
        fun(sm_backend) -> SMBackend;
            (hosts) -> [<<"localhost">>] end),
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),
    meck:expect(ejabberd_hooks, add, fun(_) -> ok end),
    meck:expect(ejabberd_hooks, add, fun(_, _, _, _, _) -> ok end),

    meck:new(ejabberd_commands, []),
    meck:expect(ejabberd_commands, register_commands, fun(_) -> ok end),
    ok.
