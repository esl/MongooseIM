# Error Log Tracking in Big Tests

The `cth_error_report` CT hook captures error-level logs from MongooseIM nodes
during big test execution. It produces per-suite reports showing which errors
were logged, broken down by group and testcase.

## How it works

The hook is enabled via test spec files (e.g., `default.spec`):

```erlang
{ct_hooks, [..., cth_error_report]}.
```

During test execution it:

1. Injects a logger handler into the MIM node to capture error-level log events
2. Collects errors at each testcase/group boundary
3. Writes reports to `ct_report/ct_run.*/logged_errors/`

## Reports

After a test run, the `logged_errors/` directory contains:

- `summary.html` -- overview table of all suites, with error counts
- `summary.log` -- plain text version of the summary
- `SuiteName.html` -- per-suite report with color-coded errors
- `SuiteName.log` -- plain text version

A link to `summary.html` appears as a "LOGGED ERRORS" button on the CT
`index.html` page.

## Declaring expected errors

Tests can declare that certain errors are expected using `cth_error_report:expect/1`.
Expected errors appear in green in the HTML report and are not prefixed with
`*** UNEXPECTED ***` in the log report. They do not count toward the
unexpected error total in the summary.

### Pattern types

The `expect/1` function accepts the same pattern types as `log_error_helper:expect/1`:

| Pattern | Matches |
|---------|---------|
| `{what, Atom}` | Report map with `#{what := Atom}` -- most common |
| `{module, Atom}` | Error logged from the given module (from MFA metadata) |
| `{function, Atom}` | Error logged from the given function |
| `{mfa, {M, F, A}}` | Exact MFA match, use `'_'` as wildcard for any element |
| `{reason, Term}` | Report map with `#{reason := Term}` |
| `{Key, Value}` | Any key-value pair in the report map |
| `<<"substring">>` | Binary substring match in the formatted message |
| `{regex, <<"pattern">>}` | Regex match on the formatted message |
| `fun((Msg, Meta) -> boolean())` | Custom filter function |

### Where to call expect/1

Call `cth_error_report:expect/1` from `init_per_suite`, `init_per_group`,
or `init_per_testcase`. Patterns accumulate for the duration of the suite --
once declared, they apply to all subsequent error classification in that suite.

### Examples

```erlang
init_per_suite(Config) ->
    %% This module always logs an error on startup, it's expected
    cth_error_report:expect({what, push_send_failed}),

    %% Expect any error from a specific module
    cth_error_report:expect({module, mod_push_service_mongoosepush}),

    %% Expect errors matching a substring
    cth_error_report:expect(<<"connection refused">>),

    %% Expect errors matching a regex
    cth_error_report:expect({regex, <<"timeout after [0-9]+ ms">>}),

    %% Expect errors from a specific function (any arity)
    cth_error_report:expect({mfa, {mod_mam_rdbms_arch, retract_message, '_'}}),

    Config.
```

Patterns can also be declared per-group or per-testcase:

```erlang
init_per_group(reconnect_tests, Config) ->
    cth_error_report:expect({what, session_replaced}),
    Config;
init_per_group(_, Config) ->
    Config.

init_per_testcase(test_connection_timeout, Config) ->
    cth_error_report:expect(<<"connection closed">>),
    Config;
init_per_testcase(_, Config) ->
    Config.
```

## Asserting a maximum number of unexpected errors

Use `cth_error_report:max_unexpected_errors_logged/1` to set a limit on
unexpected errors for a suite. If the limit is exceeded, the test run
fails with a non-zero exit code.

```erlang
init_per_suite(Config) ->
    %% Allow at most 5 unexpected errors in this suite
    cth_error_report:max_unexpected_errors_logged(5),

    %% Declare known expected errors
    cth_error_report:expect({what, known_flaky_error}),

    Config.
```

The check runs after all testcases complete. If the unexpected error count
exceeds the limit, the test run exits with code 1 and prints:

```
**** Failing due to unexpected error log limit exceeded:
my_SUITE: 8 unexpected errors logged, max allowed: 5
```

Suites without `max_unexpected_errors_logged` are not checked.

## Parallel groups

For parallel test groups, per-testcase error tracking is not possible because
CT forks the hook state for each parallel testcase. Errors from parallel
testcases are collected at the group level instead and attributed to the
group's `end_per_group` section. Parallel groups are marked with `[parallel]`
in the reports.
