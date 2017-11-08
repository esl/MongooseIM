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
%        check_ok_pace,
%        check_changing_pace
    ].

init_per_suite(C) ->
    meck:new(lager, []),
    Self = self(),
    meck:expect(lager, log, fun(lager_event, _, _, ?LOG_MSG, _) ->   %% TODO check if it makes it fail. Does it send ad all?
                                                                     %% it's not logging anything
                                    ct:log("sending"),
                                     Self ! logged;
                                (_, _, _, _, _) -> 
                                    ct:log("sending2"),
                                    ok end),
    C.
    
end_per_suite(C) ->
    meck:unload(),
    C.

init_per_testcase(_, C) ->
    mongoose_deprecations:start(),
    C.

end_per_testcase(_, C) ->
    mongoose_deprecations:stop(),
    C.

check_too_fast_pace(Config) ->
    ct:print("Tabs: ~p~n", [ets:all()]),
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

