-module(mongoose_deprecation_SUITE).

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
        check_too_fast_pace,
        check_ok_pace,
        check_changing_pace
    ].

init_per_suite(C) ->
    mongoose_deprecation:start(),
    meck:new(mongoose_depreacation, [passthrough]),
    meck:excpect(mongoose_depreacation, 
                 default_cooldown, 
                 fun() -> 6000 * 200 end),    % change 6h to 200 ms
    meck:new(lager, []),
    Self = self(),
    meck:excpect(lager, log, fun(lager_event, _, _, ?LOG_MSG, _) ->
                                     Self ! logged;
                                (_, _, _, _, _) -> ok end),
    C.
    
end_per_suite(C) ->
    meck:unload(),
    C.

init_per_test(C) ->
    mongoose_deprecations:start(),
    C.

end_per_testcase(C) ->
    mongoose_deprecations:stop(),
    C.

check_too_fast_pace(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    assert_n_logged(1).

check_ok_pace(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    assert_n_logged(3).

check_changing_pace(_Config) ->
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    timer:sleep(?TEST_COOLDOWN + ?COOLDOWN_EPS),
    mongoose_deprecations:log(some_tag, ?LOG_MSG),
    assert_n_logged(3).

assert_n_logged(0) ->
    receive
        logged ->
            ct:fail(too_many_logged)
    after ?TEST_COOLDOWN * 3 ->
              ok
    end;
assert_n_logged(N) ->
    receive
        logged ->
            assert_n_logged(N - 1)
    after ?TEST_COOLDOWN * 3 ->
              ct:fail(too_little_logged)
    end.

