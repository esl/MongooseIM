-module(gen_server_unexpected_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log_helper.hrl").

all() ->
    [{group, Name} || Name <- servers()].

groups() ->
    [{Name, [], cases()} || Name <- servers()].

servers() ->
    [mongoose_ets_heir].

cases() ->
    [unexpected_call,
     unexpected_cast,
     unexpected_info].

init_per_suite(Config) ->
    log_helper:set_up(),
    Config.

end_per_suite(_Config) ->
    log_helper:tear_down().

init_per_group(Name, Config) ->
    [{server, Name} | Config].

end_per_group(_Name, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    {ok, _Pid} = start_server(?config(server, Config)),
    log_helper:subscribe(),
    Config.

end_per_testcase(_Case, Config) ->
    log_helper:unsubscribe(),
    gen_server:stop(?config(server, Config)).

start_server(mongoose_ets_heir) ->
    mongoose_ets_heir:start_link().

%% Test cases

unexpected_call(Config) ->
    % GIVEN
    Name = ?config(server, Config),
    Pid = whereis(Name),

    % WHEN
    _ = gen_server:call(Name, bad_call),

    % THEN
    ?assertLog(warning, #{what := unexpected_call, msg := bad_call}, 1000),
    ?assertEqual(Pid, whereis(Name)).

unexpected_cast(Config) ->
    % GIVEN
    Name = ?config(server, Config),
    Pid = whereis(Name),

    % WHEN
    gen_server:cast(Name, bad_cast),

    % THEN
    ?assertLog(warning, #{what := unexpected_cast, msg := bad_cast}, 1000),
    ?assertEqual(Pid, whereis(Name)).

unexpected_info(Config) ->
    % GIVEN
    Name = ?config(server, Config),
    Pid = whereis(Name),

    % WHEN
    Name ! bad_info,

    % THEN
    ?assertLog(warning, #{what := unexpected_info, msg := bad_info}, 1000),
    ?assertEqual(Pid, whereis(Name)).
