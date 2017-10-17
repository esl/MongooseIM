-module(mongoose_rdbms_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").

-compile([export_all]).

-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(ne(E, I), ?assert(E =/= I)).

-define(KEEPALIVE_INTERVAL, 1).
-define(KEEPALIVE_QUERY, <<"SELECT 1;">>).
-define(MAX_INTERVAL, 30).

all() ->
    [{group, odbc},
     {group, mysql},
     {group, pgsql}].

init_per_suite(Config) ->
    application:ensure_all_started(exometer),
    Config.

end_per_suite(_Config) ->
    meck:unload(),
    application:stop(exometer),
    application:stop(exometer_core).

groups() ->
    [{odbc, [], tests()},
     {mysql, [], tests()},
     {pgsql, [], tests()}].

tests() ->
    [keepalive_interval,
     does_backoff_exceed,
     does_backoff_increase,
     keepalive_exit].

init_per_group(odbc, Config) ->
    case code:ensure_loaded(odbc) of
        {module, odbc} ->
            [{db_type, odbc} | Config];
        _ ->
            {skip, no_odbc_application}
    end;
init_per_group(Group, Config) ->
    [{db_type, Group} | Config].

end_per_group(_, Config) ->
    Config.

init_per_testcase(does_backoff_exceed, Config) ->
    DbType = ?config(db_type, Config),
    meck_config(DbType, ?KEEPALIVE_INTERVAL, ?MAX_INTERVAL),
    meck_db(DbType),
    meck_connection_error(DbType),
    Config;
init_per_testcase(does_backoff_increase, Config) ->
    DbType = ?config(db_type, Config),
    meck_config(DbType, 2, 10),
    meck_db(DbType),
    meck_connection_error(DbType),
    Config;
init_per_testcase(_, Config) ->
    DbType = ?config(db_type, Config),
    meck_config(DbType, ?KEEPALIVE_INTERVAL, ?MAX_INTERVAL),
    meck_db(DbType),
    Config.

end_per_testcase(T, Config) when T =:= does_backoff_increase,
                                 T =:= does_backoff_exceed ->
    Db = ?config(db_type, Config),
    meck_connection_error_unload(Db),
    meck_unload(Db),
    Config;
end_per_testcase(_, Config) ->
    meck_unload(?config(db_type, Config)),
    Config.

%% Test cases
keepalive_interval(Config) ->
    {ok, Pid} = gen_server:start_link(mongoose_rdbms, default, []),
    true = erlang:unlink(Pid),
    timer:sleep(5500),
    ?eq(5, query_calls(Config)),
    true = erlang:exit(Pid, kill),
    ok.

keepalive_exit(Config) ->
    {ok, Pid} = gen_server:start_link(mongoose_rdbms, default, []),
    true = erlang:unlink(Pid),
    Monitor = erlang:monitor(process, Pid),
    timer:sleep(3500),
    ?eq(3, query_calls(Config)),
    meck_error(?config(db_type, Config)),
    receive
        {'DOWN', Monitor, process, Pid, {keepalive_failed, _}} ->
            ok
    after 1500 ->
        ct:fail(no_down_message)
    end.

%% 5 retries. Max retry 1 and is 1 as well.
%% 5 times one second.
%% Proccess should die after 5 seconds. No longer.
does_backoff_exceed(Config) ->
    {Time, {error, _}} = timer:tc(gen_server, start, [mongoose_rdbms, default, []]),
    ct:log("Time: ~p~n", [Time]),
    ?assert(Time < 5100 * 1000).

%% 5 retries. Max retry 10. Iniitial retry 2.
%% We should get a sequence: 2 -> 4 -> 10 -> 10 -> 10.
does_backoff_increase(Config) ->
    meck_rand(),    % move to init per testcase
    {error, _} = gen_server:start(mongoose_rdbms, default, []),
    % We expect to have 2 at the begininng, then values up to 10 and 10 three times in total
    receive_backoffs(2, 10, 3),
    meck_unload_rand(). % move to end per testcase

receive_backoffs(InitialValue, MaxValue, MaxCount) ->
    receive_backoffs(InitialValue, MaxValue, MaxCount, 0).

receive_backoffs(ExpectedVal, MaxValue, MaxCountExpected, MaxCount) ->
    receive
        {backoff, MaxValue} when MaxCount =:= MaxCountExpected - 1 ->
            ok;
        {backoff, MaxValue} ->
            receive_backoffs(MaxValue, MaxValue, MaxCountExpected, MaxCount + 1);
        {backoff, ExpectedVal} ->
            receive_backoffs(min(ExpectedVal * ExpectedVal, MaxValue), MaxValue, MaxCountExpected, MaxCount)
    after 5000 -> % Lower this
            ct:fail(no_backoff)
    end.

%% Mocks

meck_rand() ->
    meck:new(rand, [unstick, no_link]),
    Self = self(),
    Fun = fun(Val) -> ct:log("sending backoff: ~p to pid: ~p~n", [Val, Self]), Self ! {backoff, Val}, 0 end,
    meck:expect(rand, uniform, Fun).

meck_unload_rand() ->
    meck:unload(rand).

meck_config(Server, KeepaliveInterval, MaxInterval) ->
    meck:new(ejabberd_config, [no_link]),
    meck:expect(ejabberd_config, get_local_option,
                fun(max_fsm_queue) -> 1024;
                   (all_metrics_are_global) -> false
                end),
    meck:new(mongoose_rdbms_sup, [no_link, passthrough]),
    meck:expect(mongoose_rdbms_sup, get_option,
                fun(default, odbc_keepalive_interval) -> KeepaliveInterval;
                   (default, odbc_start_interval) -> MaxInterval;
                   (default, odbc_server) -> server(Server)
                end).

meck_db(odbc) ->
    meck:new(odbc, [no_link]),
    meck:expect(mongoose_rdbms_odbc, disconnect, fun(_) -> ok end),
    meck:expect(odbc, connect, fun(_, _) -> {ok, self()} end),
    meck:expect(odbc, sql_query, fun(_Ref, _Query, _Timeout) -> {selected, ["row"]} end);
meck_db(mysql) ->
    meck:new(mysql, [no_link]),
    meck:expect(mongoose_rdbms_mysql, disconnect, fun(_) -> ok end),
    meck:expect(mysql, start_link, fun(_) -> {ok, self()} end),
    meck:expect(mysql, query, fun(_Ref, _Query) -> {ok, [], []} end);
meck_db(pgsql) ->
    meck:new(epgsql, [no_link]),
    meck:expect(mongoose_rdbms_pgsql, disconnect, fun(_) -> ok end),
    meck:expect(epgsql, connect, fun(_) -> {ok, self()} end),
    meck:expect(epgsql, squery,
                fun(_Ref, <<"SET", _/binary>>)       -> {ok, 0};
                   (_Ref, [<<"SET", _/binary>> | _]) -> {ok, 0};
                   (_Ref, [<<"SELECT", _/binary>>])  -> {ok, [], [{1}]} end).

meck_connection_error(pgsql) ->
    meck:delete(epgsql, connect, 1),
    meck:expect(epgsql, connect, fun(_) -> connection_error end);
meck_connection_error(odbc) ->
    meck:delete(odbc, connect, 2),
    meck:expect(odbc, connect, fun(_, _) -> connection_error end);
meck_connection_error(mysql) ->
    meck:delete(mongoose_rdbms_mysql, connect, 2),
    meck:expect(mongoose_rdbms_mysql, connect, fun(_, _) -> {error, connection_error} end).

meck_connection_error_unload(pgsql) ->
    meck:unload(epgsql);
meck_connection_error_unload(odbc) ->
    meck:unload(odbc);
meck_connection_error_unload(mysql) ->
    meck:delete(mongoose_rdbms_msql, connect, 2),
    meck:unload(mongoose_rdbms_mysql).


meck_error(odbc) ->
    meck:expect(odbc, sql_query,
                fun(_Ref, _Query, _Timeout) ->
                        {error, "connection broken"}
                end);
meck_error(mysql) ->
    meck:expect(mysql, query, fun(_Ref, _Query) -> {error, {123, "", <<"connection broken">>}} end);
meck_error(pgsql) ->
    meck:expect(epgsql, connect, fun(_) -> {ok, self()} end),
    meck:expect(epgsql, squery,
                fun(_Ref, _Query) -> {error, {error, 2, 3, 4, <<"connection broken">>, 5}} end).

meck_unload(DbType) ->
    meck:unload(ejabberd_config),
    meck:unload(mongoose_rdbms_sup),
    do_meck_unload(DbType).

do_meck_unload(odbc) ->
    meck:unload(odbc);
do_meck_unload(mysql) ->
    meck:unload(mysql);
do_meck_unload(pgsql) ->
    meck:unload(epgsql).

query_calls(Config) ->
    DbType = ?config(db_type, Config),
    {M, F} = mf(DbType),
    meck:num_calls(M, F, a(DbType)).

mf(odbc) ->
    {odbc, sql_query};
mf(mysql) ->
    {mysql, query};
mf(pgsql) ->
    {epgsql, squery}.

a(odbc) ->
    ['_', [?KEEPALIVE_QUERY], '_'];
a(mysql) ->
    ['_', [?KEEPALIVE_QUERY]];
a(pgsql) ->
    ['_', [?KEEPALIVE_QUERY]].

server(odbc) ->
    "fake-connection-string";
server(mysql) ->
    {mysql, "fake-host", "fake-db", "fake-user", "fake-pass"};
server(pgsql) ->
    {pgsql, "fake-host", "fake-db", "fake-user", "fake-pass"}.
