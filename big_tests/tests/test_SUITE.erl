-module(test_SUITE).

-compile([export_all, nowarn_export_all]).

% -define(ERROR(E), (throw(E))).
-define(ERROR(E), (error(E))).
% -define(ERROR(E), (exit(E))).

all() ->
    [{group, test_group},
     {group, skipped_test_group},
     {group, failing_test_group_1},
     {group, failing_test_group_2},
     {group, nested_test_group},
     {group, deeply_nested_test_group},
     {group, failing_nested_test_group_1},
     {group, failing_nested_test_group_2},
     {group, failing_repeating_group},
     {group, passing_repeating_group},
     {group, passing_repeating_group_with_autoskip}
     | test_cases() ].

groups() ->
    [{test_group, [], test_cases()},
     {skipped_test_group, [], test_cases()},
     {failing_test_group_1, [], test_cases()},
     {failing_test_group_2, [], test_cases()},
     {nested_test_group, [], [{group, test_group},
                              {group, skipped_test_group},
                              {group, failing_test_group_1},
                              {group, failing_test_group_2}]},
     {deeply_nested_test_group, [], [{group, test_group},
                                     {group, skipped_test_group},
                                     {group, failing_test_group_1},
                                     {group, failing_test_group_2},
                                     {group, nested_test_group}]},
     {failing_nested_test_group_1, [], [{group, test_group}]},
     {failing_nested_test_group_2, [], [{group, test_group}]},
     {failing_repeating_group, [{repeat_until_all_ok, 3}], test_cases()},
     {passing_repeating_group, [{repeat_until_all_ok, 3}], [passing_tc, eventually_passing_tc]},
     {passing_repeating_group_with_autoskip, [{repeat_until_all_ok, 3}, sequence], [eventually_passing_tc,
                                                                                    passing_tc]}].

test_cases() ->
    [passing_tc,
     skipped_tc,
     failing_tc_1,
     failing_tc_2,
     failing_tc_3].

% init_per_suite(_Config) -> ?ERROR({error, init_per_suite});
init_per_suite(Config) ->
    %% atomics are automatically garbage collected
    %% when they are no longer referenced
    [ {counter, atomics:new(1, [{signed, false}])} | Config ].

% end_per_suite(_Config) -> ?ERROR({error, end_per_suite});
end_per_suite(Config) -> Config.

init_per_group(skipped_test_group, _Config) ->
    {skip, init_per_group};
init_per_group(failing_test_group_1, _Config) ->
    ?ERROR({error, init_per_group});
init_per_group(failing_nested_test_group_1, _Config) ->
    ?ERROR({error, init_per_group});
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(failing_test_group_2, _Config) ->
    ?ERROR({error, end_per_group});
end_per_group(failing_nested_test_group_2, _Config) ->
    ?ERROR({error, end_per_group});
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(skipped_tc, _Config) ->
    {skip, init_per_testcase};
init_per_testcase(failing_tc_2, _Config) ->
    ?ERROR({error, init_per_testcase});
init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(failing_tc_3, _Config) ->
    ?ERROR({error, end_per_testcase});
end_per_testcase(_CaseName, Config) ->
    Config.

passing_tc(_Config) -> ok.

skipped_tc(_Config) -> ok.

failing_tc_1(_Config) -> ?ERROR({error, testcase}).

failing_tc_2(_Config) -> ok.

failing_tc_3(_Config) -> ok.

eventually_passing_tc(Config) ->
    {counter, AtomicRef} = proplists:lookup(counter, Config),
    Counter = atomics:add_get(AtomicRef, 1, 1),
    if
        (Counter rem 3) =:= 0 ->
            ok;
        true ->
            ?ERROR({error, eventually_passing_testcase, Counter})
    end.
