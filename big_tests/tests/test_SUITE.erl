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
     {group, failing_nested_test_group_2}
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
     {failing_nested_test_group_2, [], [{group, test_group}]}].

test_cases() ->
    [passing_tc,
     skipped_tc,
     failing_tc_1,
     failing_tc_2,
     failing_tc_3].

% init_per_suite(_Config) -> ?ERROR({error, init_per_suite});
init_per_suite(Config) -> Config.

% end_per_suite(_Config) -> ?ERROR({error, end_per_suite});
end_per_suite(Config) -> Config.

init_per_group(skipped_test_group, Config) ->
    {skip, init_per_group};
init_per_group(failing_test_group_1, Config) ->
    ?ERROR({error, init_per_group});
init_per_group(failing_nested_test_group_1, Config) ->
    ?ERROR({error, init_per_group});
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(failing_test_group_2, Config) ->
    ?ERROR({error, end_per_group});
end_per_group(failing_nested_test_group_2, Config) ->
    ?ERROR({error, end_per_group});
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(skipped_tc, Config) ->
    {skip, init_per_testcase};
init_per_testcase(failing_tc_2, Config) ->
    ?ERROR({error, init_per_testcase});
init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(failing_tc_3, Config) ->
    ?ERROR({error, end_per_testcase});
end_per_testcase(_CaseName, Config) ->
    Config.

passing_tc(Config) -> ok.

skipped_tc(Config) -> ok.

failing_tc_1(Config) -> ?ERROR({error, testcase}).

failing_tc_2(Config) -> ok.

failing_tc_3(Config) -> ok.
