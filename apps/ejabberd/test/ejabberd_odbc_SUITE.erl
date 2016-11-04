-module(ejabberd_odbc_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").

-compile([export_all]).

-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(ne(E, I), ?assert(E =/= I)).

-define(HOST, <<"localhost">>).
-define(KEEPALIVE_INTERVAL, 1).
-define(KEEPALIVE_QUERY, <<"SELECT 1;">>).

all() ->
    [{group, odbc},
     {group, mysql},
     {group, pgsql}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    meck:unload().

groups() ->
    [{odbc, [], tests()},
     {mysql, [], tests()},
     {pgsql, [], tests()}].

tests() ->
    [keepalive_interval,
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

init_per_testcase(_, Config) ->
    DbType = ?config(db_type, Config),
    meck_config(DbType, ?KEEPALIVE_INTERVAL),
    meck_db(DbType),
    Config.

end_per_testcase(_, Config) ->
    meck_unload(?config(db_type, Config)),
    Config.

%% Test cases
keepalive_interval(Config) ->
    {ok, Pid} = ejabberd_odbc:start_link(?HOST, 1000, true),
    true = erlang:unlink(Pid),
    timer:sleep(5500),
    ?eq(5, query_calls(Config)),
    true = erlang:exit(Pid, kill),
    ok.

keepalive_exit(Config) ->
    {ok, Pid} = ejabberd_odbc:start_link(?HOST, 1000, true),
    true = erlang:unlink(Pid),
    Monitor = erlang:monitor(process, Pid),
    timer:sleep(3500),
    ?eq(3, query_calls(Config)),
    meck_error(?config(db_type, Config)),
    timer:sleep(1500),
    receive
        {'DOWN', Monitor, process, Pid, {keepalive_failed, _}} ->
            ok
    after 0 ->
        ct:fail(no_down_message)
    end.

%% Mocks
meck_config(Server, KeepaliveInterval) ->
    meck:new(ejabberd_config, [no_link]),
    meck:expect(ejabberd_config, get_local_option,
                fun({odbc_keepalive_interval, _Host}) -> KeepaliveInterval;
                   (max_fsm_queue) -> 1024;
                   ({odbc_server, _}) -> server(Server);
                   (Arg) -> meck:passthrough([Arg])
                end).

meck_db(odbc) ->
    meck:new(odbc, [no_link]),
    meck:expect(odbc, connect, fun(_, _) -> {ok, self()} end),
    meck:expect(odbc, sql_query,
                fun(_Ref, _Query, _Timeout) ->
                        {selected, ["column"], ["row"]}
                end);
meck_db(mysql) ->
    meck:new(p1_mysql_conn, [no_link]),
    meck:expect(p1_mysql_conn, start, fun(_, _, _, _, _, _) -> {ok, self()} end),
    meck:expect(p1_mysql_conn, squery,
               fun(_Ref, _Query, _Pid, _Options) ->
                       {data, {p1_mysql_result, [], [], 0, ""}}
               end);
meck_db(pgsql) ->
    meck:new(pgsql, [no_link]),
    meck:expect(pgsql, connect, fun(_) -> {ok, self()} end),
    meck:expect(pgsql, squery,
                fun(_Ref, "SET" ++ _, _Timeout) ->
                        {ok, [<<"SET">>]};
                   (_Ref, [<<"SELECT", _/binary>>], _Timeout) ->
                        {ok, [{<<"SELECT">>, [], []}]}
                end).

meck_error(odbc) ->
    meck:expect(odbc, sql_query,
                fun(_Ref, _Query, _Timeout) ->
                        {error, "connection broken"}
                end);
meck_error(mysql) ->
    meck:expect(p1_mysql_conn, squery,
                fun(_Ref, _Query, _Pid, _Options) ->
                        {error, "connection broken"}
                end);
meck_error(pgsql) ->
    meck:expect(pgsql, squery,
                fun(_Ref, _Query, _Timeout) ->
                        {ok, [{error, "connection broken"}]}
                end).

meck_unload(DbType) ->
    meck:unload(ejabberd_config),
    do_meck_unload(DbType).

do_meck_unload(odbc) ->
    meck:unload(odbc);
do_meck_unload(mysql) ->
    meck:unload(p1_mysql_conn);
do_meck_unload(pgsql) ->
    meck:unload(pgsql).

query_calls(Config) ->
    DbType = ?config(db_type, Config),
    {M, F} = mf(DbType),
    meck:num_calls(M, F, a(DbType)).

mf(odbc) ->
    {odbc, sql_query};
mf(mysql) ->
    {p1_mysql_conn, squery};
mf(pgsql) ->
    {pgsql, squery}.

a(odbc) ->
    ['_', [?KEEPALIVE_QUERY], '_'];
a(mysql) ->
    ['_', [?KEEPALIVE_QUERY], '_', '_'];
a(pgsql) ->
    ['_', [?KEEPALIVE_QUERY], '_'].

server(odbc) ->
    "fake-connection-string";
server(mysql) ->
    {mysql, "fake-host", "fake-db", "fake-user", "fake-pass"};
server(pgsql) ->
    {pgsql, "fake-host", "fake-db", "fake-user", "fake-pass"}.
