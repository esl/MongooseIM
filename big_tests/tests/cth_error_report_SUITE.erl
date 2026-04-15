%% @doc Test suite for cth_error_report CT hook.
%% Verifies expected error declarations, pattern matching,
%% counted allowances, and report generation.
%%
%% After the run, inspect the reports:
%%   ct_report/ct_run.*/logged_errors/cth_error_report_SUITE.html
%%   ct_report/ct_run.*/logged_errors/cth_error_report_SUITE.log
%%   ct_report/ct_run.*/logged_errors/summary.html
-module(cth_error_report_SUITE).
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
    [{group, unlimited_patterns},
     {group, counted_patterns},
     {group, mixed_patterns},
     {group, report_verification}].

groups() ->
    [{unlimited_patterns, [],
      [expect_by_what,
       expect_by_module,
       expect_by_function,
       expect_by_mfa_wildcard,
       expect_by_reason,
       expect_by_generic_key,
       expect_by_substring,
       expect_by_regex,
       expect_by_custom_function,
       multiple_unlimited_patterns,
       unlimited_allows_any_count]},
     {counted_patterns, [],
      [expect_counted_exact,
       expect_counted_accumulate,
       expect_counted_overflow_is_unexpected,
       expect_counted_across_testcases]},
     {mixed_patterns, [],
      [unlimited_overrides_counted,
       some_expected_some_not,
       no_patterns_all_unexpected]},
     {report_verification, [],
      [verify_report_files_created,
       verify_unexpected_prefix_in_log]}].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    mongoose_helper:inject_module(log_error_test_helper),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Unlimited pattern tests
%%--------------------------------------------------------------------

expect_by_what(_Config) ->
    cth_error_report:expect({what, err_by_what}),
    trigger_error(err_by_what, "by what").

expect_by_module(_Config) ->
    cth_error_report:expect({module, log_error_test_helper}),
    trigger_error(err_by_module, "by module").

expect_by_function(_Config) ->
    cth_error_report:expect({function, log_error}),
    trigger_error(err_by_function, "by function").

expect_by_mfa_wildcard(_Config) ->
    cth_error_report:expect(
        {mfa, {log_error_test_helper, '_', '_'}}),
    trigger_error(err_by_mfa, "by mfa wildcard").

expect_by_reason(_Config) ->
    cth_error_report:expect({reason, "by reason value"}),
    trigger_error(err_by_reason, "by reason value").

expect_by_generic_key(_Config) ->
    cth_error_report:expect({custom_key, custom_value}),
    trigger_error_extra(err_by_key, "by key",
                        #{custom_key => custom_value}).

expect_by_substring(_Config) ->
    cth_error_report:expect(<<"substr_marker_123">>),
    trigger_error(substr_marker_123, "by substring").

expect_by_regex(_Config) ->
    cth_error_report:expect({regex, <<"regex_test_[0-9]+">>}),
    trigger_error(regex_test_42, "by regex").

expect_by_custom_function(_Config) ->
    Filter = fun({report, #{what := err_custom_fn}}, _Meta) ->
                     true;
                (_, _) ->
                     false
             end,
    cth_error_report:expect(Filter),
    trigger_error(err_custom_fn, "by custom function").

multiple_unlimited_patterns(_Config) ->
    cth_error_report:expect({what, multi_err_1}),
    cth_error_report:expect({what, multi_err_2}),
    cth_error_report:expect({what, multi_err_3}),
    trigger_error(multi_err_1, "first"),
    trigger_error(multi_err_2, "second"),
    trigger_error(multi_err_3, "third").

unlimited_allows_any_count(_Config) ->
    cth_error_report:expect({what, unlimited_repeat}),
    trigger_error(unlimited_repeat, "one"),
    trigger_error(unlimited_repeat, "two"),
    trigger_error(unlimited_repeat, "three"),
    trigger_error(unlimited_repeat, "four"),
    trigger_error(unlimited_repeat, "five").

%%--------------------------------------------------------------------
%% Counted pattern tests
%%--------------------------------------------------------------------

expect_counted_exact(_Config) ->
    %% Expect exactly 3 -- trigger exactly 3
    cth_error_report:expect({what, counted_exact}, 3),
    trigger_error(counted_exact, "one"),
    trigger_error(counted_exact, "two"),
    trigger_error(counted_exact, "three").

expect_counted_accumulate(_Config) ->
    %% Two declarations for the same pattern should add up
    cth_error_report:expect({what, counted_accum}, 2),
    cth_error_report:expect({what, counted_accum}, 3),
    %% Total allowed: 5
    trigger_error(counted_accum, "one"),
    trigger_error(counted_accum, "two"),
    trigger_error(counted_accum, "three"),
    trigger_error(counted_accum, "four"),
    trigger_error(counted_accum, "five").

expect_counted_overflow_is_unexpected(_Config) ->
    %% Expect 2, trigger 4 -- 2 should be expected, 2 unexpected
    cth_error_report:expect({what, counted_overflow}, 2),
    trigger_error(counted_overflow, "expected one"),
    trigger_error(counted_overflow, "expected two"),
    trigger_error(counted_overflow, "unexpected three"),
    trigger_error(counted_overflow, "unexpected four").

expect_counted_across_testcases(_Config) ->
    %% This test runs after expect_counted_exact and
    %% expect_counted_accumulate in the same group.
    %% Verify that their allowances don't leak here:
    %% trigger an error with a fresh pattern, no expect.
    trigger_error(counted_no_expect, "should be unexpected").

%%--------------------------------------------------------------------
%% Mixed pattern tests
%%--------------------------------------------------------------------

unlimited_overrides_counted(_Config) ->
    %% Declare counted first, then unlimited -- unlimited wins
    cth_error_report:expect({what, override_test}, 2),
    cth_error_report:expect({what, override_test}),
    %% All should be expected regardless of count
    trigger_error(override_test, "one"),
    trigger_error(override_test, "two"),
    trigger_error(override_test, "three"),
    trigger_error(override_test, "four").

some_expected_some_not(_Config) ->
    %% Only one pattern is declared
    cth_error_report:expect({what, partially_expected}),
    trigger_error(partially_expected, "this is expected"),
    trigger_error(totally_unexpected, "this is not expected").

no_patterns_all_unexpected(_Config) ->
    %% No expect calls -- everything is unexpected
    trigger_error(no_pattern_err, "all unexpected").

%%--------------------------------------------------------------------
%% Report verification tests
%%--------------------------------------------------------------------

verify_report_files_created(_Config) ->
    %% Just trigger an error so the report has content
    cth_error_report:expect({what, report_verify_err}),
    trigger_error(report_verify_err, "for report verification").

verify_unexpected_prefix_in_log(_Config) ->
    %% Trigger an unexpected error -- it should appear
    %% with *** UNEXPECTED *** prefix in the .log file
    trigger_error(prefix_test_err, "check prefix in log").

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

trigger_error(What, Reason) ->
    rpc(mim(), log_error_test_helper, log_error,
        [What, Reason]).

trigger_error_extra(What, Reason, Extra) ->
    rpc(mim(), log_error_test_helper, log_error,
        [What, Reason, Extra]).
