-module(mongoose_rdbms_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

-define(eq(E, I), ?assertEqual(E, I)).

-define(KEEPALIVE_INTERVAL, 1).
-define(KEEPALIVE_QUERY, <<"SELECT 1;">>).
-define(MAX_INTERVAL, 30).

all() ->
    [{group, mysql},
     {group, pgsql}].

init_per_suite(Config) ->
    application:ensure_all_started(exometer_core),
    set_opts(),
    async_helper:start(Config, mongoose_instrument, start_link, []).

end_per_suite(Config) ->
    meck:unload(),
    async_helper:stop_all(Config),
    unset_opts(),
    application:stop(exometer_core).

groups() ->
    [{mysql, [], tests()},
     {pgsql, [], tests()}].

tests() ->
    [keepalive_interval,
     does_backoff_increase_to_a_point,
     keepalive_exit].

init_per_group(Group, Config) ->
    mongoose_rdbms_backend:init(Group),
    [{db_type, Group} | Config].

end_per_group(_, Config) ->
    % clean up after mongoose_backend:init
    persistent_term:erase({backend_module, global, mongoose_rdbms}),
    Config.

init_per_testcase(does_backoff_increase_to_a_point, Config) ->
    DbType = ?config(db_type, Config),
    meck_db(DbType),
    meck_connection_error(DbType),
    meck_rand(),
    ServerOpts = server_opts(DbType),
    [{db_opts, ServerOpts#{query_timeout => 5000, keepalive_interval => 2, max_start_interval => 10}} | Config];
init_per_testcase(_, Config) ->
    DbType = ?config(db_type, Config),
    meck_db(DbType),
    ServerOpts = server_opts(DbType),
    [{db_opts, ServerOpts#{query_timeout => 5000, keepalive_interval => ?KEEPALIVE_INTERVAL,
                           max_start_interval => ?MAX_INTERVAL}} | Config].

end_per_testcase(does_backoff_increase_to_a_point, Config) ->
    meck_unload_rand(),
    Db = ?config(db_type, Config),
    meck_config_and_db_unload(Db),
    Config;
end_per_testcase(_, Config) ->
    meck_config_and_db_unload(?config(db_type, Config)),
    Config.

%% Test cases
keepalive_interval(Config) ->
    {ok, Pid} = gen_server:start(mongoose_rdbms, ?config(db_opts, Config), []),
    timer:sleep(5500),
    ?eq(5, query_calls(Config)),
    true = erlang:exit(Pid, kill),
    ok.

keepalive_exit(Config) ->
    {ok, Pid} = gen_server:start(mongoose_rdbms, ?config(db_opts, Config), []),
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

%% 5 retries. Max retry 10. Initial retry 2.
%% We should get a sequence: 2 -> 4 -> 10 -> 10 -> 10.
does_backoff_increase_to_a_point(Config) ->
    {error, _} = gen_server:start(mongoose_rdbms, ?config(db_opts, Config), []),
    % We expect to have 2 at the beginning, then values up to 10 and 10 three times in total
    receive_backoffs(2, 10, 3).

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
    after 200 -> % Lower this
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

set_opts() ->
    mongoose_config:set_opts(opts()).

unset_opts() ->
    mongoose_config:erase_opts().

opts() ->
    #{instrumentation => config_parser_helper:default_config([instrumentation])}.

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
    meck:expect(epgsql, connect, fun(_) -> connection_error end);
meck_connection_error(mysql) ->
    meck:expect(mongoose_rdbms_mysql, connect, fun(_, _) -> {error, connection_error} end).


meck_error(mysql) ->
    meck:expect(mysql, query, fun(_Ref, _Query) -> {error, {123, "", <<"connection broken">>}} end);
meck_error(pgsql) ->
    meck:expect(epgsql, connect, fun(_) -> {ok, self()} end),
    meck:expect(epgsql, squery,
                fun(_Ref, _Query) -> {error, {error, 2, 3, 4, <<"connection broken">>, 5}} end).

meck_config_and_db_unload(DbType) ->
    do_meck_unload(DbType).

do_meck_unload(mysql) ->
    meck:unload(mongoose_rdbms_mysql),
    meck:unload(mysql);
do_meck_unload(pgsql) ->
    meck:unload(epgsql).

query_calls(Config) ->
    DbType = ?config(db_type, Config),
    {M, F} = mf(DbType),
    meck:num_calls(M, F, a(DbType)).

mf(mysql) ->
    {mysql, query};
mf(pgsql) ->
    {epgsql, squery}.

a(mysql) ->
    ['_', [?KEEPALIVE_QUERY]];
a(pgsql) ->
    ['_', [?KEEPALIVE_QUERY]].

server_opts(mysql) ->
    #{driver => mysql, host => "fake-host", port => 3306,
      database => "fake-db", username => "fake-user", password => "fake-pass"};
server_opts(pgsql) ->
    #{driver => pgsql, host => "fake-host", port => 5432,
      database => "fake-db", username => "fake-user", password => "fake-pass"}.
