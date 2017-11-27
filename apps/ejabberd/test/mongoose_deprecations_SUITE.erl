-module(mongoose_deprecations_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

-define(TEST_COOLDOWN, 50).    % ms
-define(COOLDOWN_EPS, 3).      % ms, must be much smaller than TEST_COOLDOWN
-define(LOG_MSG, "hey, deprecated!").

all() ->
    [
        check_too_fast_pace,
        check_ok_pace,
        check_changing_pace,
        different_tags_get_logged_always,
        specified_lvl_logged_default_stays,
        default_lvl_is_error
    ].

init_per_suite(C) ->
    C.
    
end_per_suite(C) ->
    C.

init_per_testcase(_, C) ->
    mock_log_with_lvl(self()),
    mongoose_deprecations:start(),
    C.

end_per_testcase(_, C) ->
    mongoose_deprecations:stop(),
    meck:unload(),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_too_fast_pace(Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    {ok, _} = assert_n_logged(1),
    ok.

check_ok_pace(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    {ok, _} = assert_n_logged(3),
    ok.

check_changing_pace(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    {ok, _} = assert_n_logged(3),
    ok.

%% Even the same message can be logged with any frequency when tags
%% are different
different_tags_get_logged_always(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag1, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    mongoose_deprecations:log(some_tag2, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    {ok, _} = assert_n_logged(3),
    ok.

default_lvl_is_error(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    {ok, [error, error, error]} = assert_n_logged(3),
    ok.

% One can specify the level error on warning. It does not affect the default
% level.
specified_lvl_logged_default_stays(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG, 
                              [{cooldown, ?TEST_COOLDOWN},
                               {log_level, warning}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, 
                              [{cooldown, ?TEST_COOLDOWN},
                               {log_level, warning}]),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG, [{cooldown, ?TEST_COOLDOWN}]),
    {ok, [warning, error, warning, error]} = assert_n_logged(4),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Returns {ok, Lvls} where Lvls is the list
% of log levels used for each message. In order
% of appearing.
assert_n_logged(N) ->
    do_assert_n_logged(N, []).

do_assert_n_logged(0, LvlsAcc) ->
    receive
        {logged, {Msg, Lvl}} ->
            ct:fail(too_many_logged)
    after ?TEST_COOLDOWN * 2 ->
              {ok, lists:reverse(LvlsAcc)}
    end;
do_assert_n_logged(N, LvlsAcc) ->
    receive
        {logged, {Msg, Lvl}} ->
            do_assert_n_logged(N - 1, [Lvl | LvlsAcc])
    after ?TEST_COOLDOWN * 2 ->
              ct:fail(too_little_logged)
    end.

mock_log_with_lvl(Pid) ->
    meck:new(mongoose_deprecations, [passthrough]),
    meck:expect(mongoose_deprecations, log_with_lvl, fun(Msg, Lvl) ->
                                    Pid ! {logged, {Msg, Lvl}},
                                    ok end).
