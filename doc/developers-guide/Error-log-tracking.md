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

Tests can declare that certain errors are expected using
`cth_error_report:expect/1,2`. Expected errors appear in green in the HTML
report and are not prefixed with `*** UNEXPECTED ***` in the log report.
They do not count toward the unexpected error total in the summary.

### Pattern types

The `expect/1,2` functions accept the following pattern types:

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

### Unlimited vs counted expectations

`expect/1` declares a pattern with unlimited matches -- all matching errors
are classified as expected, regardless of how many occur:

```erlang
cth_error_report:expect({what, push_send_failed}).
```

`expect/2` declares a pattern with a specific expected count. Only that many
matches are classified as expected; additional matches become unexpected:

```erlang
%% Expect exactly 3 occurrences of this error
cth_error_report:expect({what, push_send_failed}, 3).
```

### Accumulating counts

Multiple `expect/2` calls for the same pattern accumulate their counts.
This is useful when different groups or testcases each contribute a known
number of expected errors:

```erlang
init_per_group(group_a, Config) ->
    %% This group triggers 3 push errors
    cth_error_report:expect({what, push_send_failed}, 3),
    Config;
init_per_group(group_b, Config) ->
    %% This group triggers 2 more
    cth_error_report:expect({what, push_send_failed}, 2),
    Config.
%% Total: 5 errors expected for this pattern across the suite
```

An unlimited `expect/1` call for the same pattern overrides any counted
declarations -- all matches become expected:

```erlang
%% These 3 are expected
cth_error_report:expect({what, some_error}, 3),
%% Now all matches are expected, the count of 3 is ignored
cth_error_report:expect({what, some_error}).
```

### Where to call expect/1,2

Call `cth_error_report:expect/1,2` from `init_per_suite`, `init_per_group`,
or `init_per_testcase`. Patterns accumulate for the duration of the suite --
once declared, they apply to all subsequent error classification in that
suite. Counted allowances are consumed as errors are matched, tracking
across all testcases within the suite.

### Examples

```erlang
init_per_suite(Config) ->
    %% This module always logs errors on startup, expect all of them
    cth_error_report:expect({what, push_send_failed}),

    %% Expect any error from a specific module
    cth_error_report:expect({module, mod_push_service_mongoosepush}),

    %% Expect errors matching a substring
    cth_error_report:expect(<<"connection refused">>),

    %% Expect errors matching a regex
    cth_error_report:expect({regex, <<"timeout after [0-9]+ ms">>}),

    %% Expect exactly 2 errors from a specific function
    cth_error_report:expect(
        {mfa, {mod_mam_rdbms_arch, retract_message, '_'}}, 2),

    Config.
```

Patterns can also be declared per-group or per-testcase:

```erlang
init_per_group(reconnect_tests, Config) ->
    cth_error_report:expect({what, session_replaced}, 5),
    Config;
init_per_group(_, Config) ->
    Config.

init_per_testcase(test_connection_timeout, Config) ->
    cth_error_report:expect(<<"connection closed">>, 1),
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

## Multi-node collection

The hook collects errors from all reachable MongooseIM nodes
(`mim`, `mim2`, `mim3`, `fed`, `reg`) using a push-mode design:

- A single **sink** (`cth_error_report_sink`, a gen_server) runs on
  the test runner. It owns one ETS table that holds every error
  entry from every node, keyed by a global monotonic sequence.
- A **logger handler** (`log_error_collector`) is injected on each
  reachable MIM node. For every error-level event it sends a
  `{log_entry, Node, Level, Msg, Meta}` Erlang message to the sink
  using `[noconnect, nosuspend]` (a slow or disconnected runner
  cannot back-pressure logging).
- After a MIM node restart, the restart caller is responsible for
  re-injecting the handler. `distributed_helper:start_node/2` does
  this automatically by calling `cth_error_report_sink:inject/1`.
  Accumulated entries on the runner-side sink are unaffected by
  node restarts -- only entries logged in the brief window between
  the node coming up and `inject/1` returning are missed.

Each error entry includes the originating node (in the `meta` map),
so reports show which node logged each error. Expected error
patterns apply to all nodes -- a single `expect` declaration matches
errors regardless of which node produced them.

**Limitation:** if a MIM node is restarted by a path that does not go
through `distributed_helper:start_node/2` (e.g. an external script,
or a future custom helper), error collection on that node will not
resume until something calls `cth_error_report_sink:inject/1`
explicitly. Plan-fully restarted nodes are fine.

## Parallel groups

For parallel test groups, per-testcase error tracking is not possible because
CT forks the hook state for each parallel testcase. Errors from parallel
testcases are collected at the group level instead and attributed to the
group's `end_per_group` section. Parallel groups are marked with `[parallel]`
in the reports.
