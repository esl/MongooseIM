%% @doc Test suite for log_error_helper and log_error_collector.
%% Verifies error log capturing and pattern matching functionality.
-module(log_error_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [rpc/4, mim/0, require_rpc_nodes/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [{group, basic},
     {group, pattern_matching},
     {group, parallel_tests}].

groups() ->
    [{basic, [], [captures_error_logs,
                  no_errors_passes]},
     {pattern_matching, [], [expect_by_what,
                             expect_by_module,
                             expect_by_mfa,
                             expect_by_substring,
                             expect_by_regex,
                             expect_by_custom_function,
                             multiple_expected_patterns]},
     {parallel_tests, [parallel], [parallel_test_1,
                                   parallel_test_2,
                                   parallel_test_3]}].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    %% Inject test helper module into MIM node
    mongoose_helper:inject_module(log_error_test_helper),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(parallel_tests, Config) ->
    %% For parallel tests, start once at group level
    log_error_helper:start(),
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(parallel_tests, _Config) ->
    %% For parallel tests, check once at group level
    log_error_helper:check(),
    ok;
end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(TestCase, Config) when TestCase =:= parallel_test_1;
                                         TestCase =:= parallel_test_2;
                                         TestCase =:= parallel_test_3 ->
    %% Parallel tests use group-level start/stop
    Config;
init_per_testcase(_TestCase, Config) ->
    log_error_helper:start(),
    Config.

end_per_testcase(TestCase, _Config) when TestCase =:= parallel_test_1;
                                         TestCase =:= parallel_test_2;
                                         TestCase =:= parallel_test_3 ->
    %% Parallel tests use group-level start/stop
    ok;
end_per_testcase(_TestCase, _Config) ->
    %% Clean up - stop without checking to avoid test interference
    catch log_error_helper:stop(),
    ok.

%%--------------------------------------------------------------------
%% Basic tests
%%--------------------------------------------------------------------

%% Verify that error logs are captured
captures_error_logs(_Config) ->
    %% Trigger an error on MIM node
    rpc(mim(), log_error_test_helper, log_error, [test_error_capture, "Test error message"]),

    %% Get captured errors (without checking, to inspect them)
    Result = log_error_helper:stop(),

    %% Should have unexpected errors
    ?assertMatch({error, unexpected, [_|_]}, Result),
    {error, unexpected, Errors} = Result,

    %% Verify the error was captured with correct structure
    ?assertEqual(1, length(Errors)),
    [{_Timestamp, error, Msg, Meta}] = Errors,

    %% Verify msg structure
    ?assertMatch({report, #{what := test_error_capture}}, Msg),

    %% Verify metadata contains MFA (arity 3 because logger:error is called from log_error/3)
    ?assertMatch(#{mfa := {log_error_test_helper, log_error, 3}}, Meta).

%% Verify that no errors means test passes
no_errors_passes(_Config) ->
    %% Don't trigger any errors
    Result = log_error_helper:stop(),
    ?assertEqual(ok, Result).

%%--------------------------------------------------------------------
%% Pattern matching tests
%%--------------------------------------------------------------------

%% Test expect by 'what' atom - most common pattern
expect_by_what(_Config) ->
    %% Declare expected error
    log_error_helper:expect({what, expected_what_error}),

    %% Trigger the error
    rpc(mim(), log_error_test_helper, log_error, [expected_what_error, "Expected error"]),

    %% Should pass - error was expected
    Result = log_error_helper:stop(),
    ?assertEqual(ok, Result).

%% Test expect by source module
expect_by_module(_Config) ->
    %% Expect any error from log_error_test_helper module
    log_error_helper:expect({module, log_error_test_helper}),

    %% Trigger error
    rpc(mim(), log_error_test_helper, log_error, [some_error, "Module test"]),

    Result = log_error_helper:stop(),
    ?assertEqual(ok, Result).

%% Test expect by MFA with wildcards
expect_by_mfa(_Config) ->
    %% Expect error from specific function, any arity
    log_error_helper:expect({mfa, {log_error_test_helper, log_error, '_'}}),

    %% Trigger error
    rpc(mim(), log_error_test_helper, log_error, [mfa_test, "MFA test"]),

    Result = log_error_helper:stop(),
    ?assertEqual(ok, Result).

%% Test expect by substring in formatted message
expect_by_substring(_Config) ->
    %% Expect error containing specific substring
    log_error_helper:expect(<<"substring_marker">>),

    %% Trigger error with that substring in the what atom
    rpc(mim(), log_error_test_helper, log_error, [substring_marker, "Test"]),

    Result = log_error_helper:stop(),
    ?assertEqual(ok, Result).

%% Test expect by regex pattern
expect_by_regex(_Config) ->
    %% Expect error matching regex (use [0-9] instead of \d for clarity)
    log_error_helper:expect({regex, <<"regex_test_[0-9]+">>}),

    %% Trigger error that matches the regex
    rpc(mim(), log_error_test_helper, log_error, [regex_test_123, "Regex test"]),

    Result = log_error_helper:stop(),
    ?assertEqual(ok, Result).

%% Test expect with custom filter function
expect_by_custom_function(_Config) ->
    %% Custom function that checks both msg and meta
    Filter = fun({report, #{what := What}}, #{mfa := {Mod, _, _}}) ->
                     What =:= custom_func_test andalso Mod =:= log_error_test_helper;
                (_, _) ->
                     false
             end,
    log_error_helper:expect(Filter),

    %% Trigger matching error
    rpc(mim(), log_error_test_helper, log_error, [custom_func_test, "Custom function test"]),

    Result = log_error_helper:stop(),
    ?assertEqual(ok, Result).

%% Test multiple expected patterns
multiple_expected_patterns(_Config) ->
    %% Expect multiple different errors
    log_error_helper:expect({what, first_error}),
    log_error_helper:expect({what, second_error}),
    log_error_helper:expect({what, third_error}),

    %% Trigger all expected errors
    rpc(mim(), log_error_test_helper, log_error, [first_error, "First"]),
    rpc(mim(), log_error_test_helper, log_error, [second_error, "Second"]),
    rpc(mim(), log_error_test_helper, log_error, [third_error, "Third"]),

    Result = log_error_helper:stop(),
    ?assertEqual(ok, Result).

%%--------------------------------------------------------------------
%% Parallel tests
%% These tests run in parallel and share error collection.
%% Each test declares its expected errors, triggers them, and the
%% group-level check/0 verifies all errors were expected.
%%--------------------------------------------------------------------

parallel_test_1(_Config) ->
    %% Declare expected error for this test
    log_error_helper:expect({what, parallel_error_1}),

    %% Trigger the error
    rpc(mim(), log_error_test_helper, log_error, [parallel_error_1, "From parallel test 1"]),

    %% No stop() here - done at group level
    ok.

parallel_test_2(_Config) ->
    %% Declare expected error for this test
    log_error_helper:expect({what, parallel_error_2}),

    %% Trigger the error
    rpc(mim(), log_error_test_helper, log_error, [parallel_error_2, "From parallel test 2"]),

    %% No stop() here - done at group level
    ok.

parallel_test_3(_Config) ->
    %% Declare expected error for this test
    log_error_helper:expect({what, parallel_error_3}),

    %% Trigger the error
    rpc(mim(), log_error_test_helper, log_error, [parallel_error_3, "From parallel test 3"]),

    %% No stop() here - done at group level
    ok.
