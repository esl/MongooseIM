-module(mongoose_deprecations_SUITE).

-include_lib("exml/include/exml.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

-define(TEST_COOLDOWN, 400).    % ms
-define(COOLDOWN_EPS, 10).      % ms, must be much smaller than TEST_COOLDOWN
-define(LOG_MSG, "hey, deprecated!").

all() ->
    [
        check_too_fast_pace
        check_ok_pace,
        check_changing_pace,
        different_tags_get_logged_always
    ].

init_per_suite(C) ->
    C.
    
end_per_suite(C) ->
    meck:unload(),
    C.

init_per_testcase(_, C) ->
    mock_log_with_lvl(self()),
    mongoose_deprecations:start(),
    C.

end_per_testcase(_, C) ->
    mongoose_deprecations:stop(),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_too_fast_pace(Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    assert_n_logged(1),
    ok.

check_ok_pace(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    assert_n_logged(3).

check_changing_pace(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    assert_n_logged(3).

%% Even the same message can be logged with any frequency when tags
%% are different
different_tags_get_logged_always(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag1, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag2, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    assert_n_logged(3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assert_n_logged(0) ->
    receive
        {logged, {Msg, Lvl}} ->
            ct:log("LOGGED: ~p~n", [Lvl]),
            ct:fail(too_many_logged)
    after ?TEST_COOLDOWN * 3 ->
              ok
    end;
assert_n_logged(N) ->
    receive
        M ->
            ct:log("got: ~p~n", [M]),
            ok;
        {logged, {Msg, Lvl}} ->
            ct:log("LOGGED: ~p~n", [Lvl]),
            assert_n_logged(N - 1)
    after ?TEST_COOLDOWN * 3 + 2000 -> % delete 2000 afterwards
              ct:fail(too_little_logged)
    end.

% we need to send log lvl back - we will test its correctness as well
mock_log_with_lvl(Pid) ->
    meck:new(mongoose_deprecations, [passthrough]),
    ct:log("Preparing log mock... will send to ~p", [Pid]),
    meck:expect(mongoose_deprecations, log_with_lvl, fun(Msg, Lvl) ->
                                    ct:log("sending"),
                                    Pid ! {logged, {Msg, Lvl}},
                                    ok end).
