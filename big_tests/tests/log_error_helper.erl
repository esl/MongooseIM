%% @doc A helper module for checking error logs in tests.
%% Captures error logs from MIM nodes and fails tests if unexpected errors are found.
%% Tests can declare expected errors using expect/1,2 to avoid false failures.
%%
%% Usage:
%% - Call start/0 in init_per_group/suite (idempotent, safe to call multiple times)
%% - Call expect/1,2 before code that is expected to produce errors
%% - Call check/0 in end_per_group/suite (fails if unexpected errors)
%%
%% Parallel test support:
%% - start/0 and stop/0 are idempotent and handle concurrent calls safely
%% - For parallel test groups, call start/0 in init_per_group and check/0 in end_per_group
%% - All tests in the parallel group share error collection
%% - Expected patterns from all tests are combined
%% - Any unexpected error fails the whole group
%%
%% Pattern types for expect/1:
%% - {what, atom()} - Match #{what := Atom} in report (most common)
%% - {module, atom()} - Match by source module
%% - {function, atom()} - Match by source function
%% - {mfa, {M, F, A}} - Match exact MFA (use '_' as wildcard)
%% - {reason, term()} - Match #{reason := Term} in report
%% - {Key, Value} - Match any key in the report map
%% - binary() - Substring match in formatted message (fallback)
%% - {regex, binary()} - Regex match in formatted message
%% - fun((Msg, Meta) -> boolean()) - Custom filter function
%%
%% Count types for expect/2:
%% - any - At least one matching error (default)
%% - N (integer) - Exactly N matching errors
%% - {at_least, N} - At least N matching errors
%% - {at_most, N} - At most N matching errors (0 is valid)

-module(log_error_helper).

%% Setup/teardown API
-export([start/0, start/1, stop/0, check/0]).

%% API for expecting errors
-export([expect/1, expect/2]).

%% Exported for testing
-export([matches_pattern/3, format_msg/1]).

-import(distributed_helper, [rpc/4, mim/0]).

-define(STATUS_TABLE, log_error_status_table).
-define(COLLECTOR, log_error_collector).

-type msg() :: log_error_collector:msg().
-type meta() :: log_error_collector:meta().
-type log_entry() :: {integer(), atom(), msg(), meta()}.

-type pattern() :: {what, atom()}
                 | {module, atom()}
                 | {function, atom()}
                 | {mfa, {atom() | '_', atom() | '_', non_neg_integer() | '_'}}
                 | {reason, term()}
                 | {atom(), term()}  % Generic report key match
                 | binary()          % Substring match (fallback)
                 | {regex, binary()} % Regex match (fallback)
                 | fun((msg(), meta()) -> boolean()).
-type count() :: any
              | pos_integer()
              | {at_least, pos_integer()}
              | {at_most, non_neg_integer()}.

%% API

%% @doc Start capturing errors on mim() node. Idempotent - safe to call if already started.
-spec start() -> ok | {error, term()}.
start() ->
    start(#{}).

-spec start(#{levels => [atom()]}) -> ok | {error, term()}.
start(Opts) ->
    case ets:whereis(?STATUS_TABLE) of
        undefined ->
            Levels = maps:get(levels, Opts, [error]),
            mongoose_helper:inject_module(?COLLECTOR),
            ets_helper:new(?STATUS_TABLE, [bag]),
            rpc(mim(), ?COLLECTOR, start, [Levels]);
        _Tid ->
            %% Already started
            ok
    end.

%% @doc Stop capturing and return classification result. Idempotent - safe to call if already stopped.
-spec stop() -> ok | {error, term()}.
stop() ->
    case ets:whereis(?STATUS_TABLE) of
        undefined ->
            %% Already stopped
            ok;
        _Tid ->
            Errors = rpc(mim(), ?COLLECTOR, get_errors, []),
            rpc(mim(), ?COLLECTOR, stop, []),
            Expected = ets:tab2list(?STATUS_TABLE),
            ets_helper:delete(?STATUS_TABLE),
            {Unexpected, Unmatched} = classify_errors(Errors, Expected),
            maybe_report_errors(Unexpected, Unmatched)
    end.

-spec maybe_report_errors([log_entry()], [mismatch_info()]) -> ok | {error, term()}.
maybe_report_errors([], []) ->
    ok;
maybe_report_errors(Unexpected, []) ->
    ct:log("Unexpected error logs:~n~s", [format_errors_for_report(Unexpected)]),
    {error, {unexpected_errors, Unexpected}};
maybe_report_errors([], Unmatched) ->
    ct:log("Expected error count mismatch:~n~s", [format_unmatched(Unmatched)]),
    {error, {count_mismatch, Unmatched}};
maybe_report_errors(Unexpected, Unmatched) ->
    ct:log("Unexpected error logs:~n~s", [format_errors_for_report(Unexpected)]),
    ct:log("Expected error count mismatch:~n~s", [format_unmatched(Unmatched)]),
    {error, {unexpected_errors, Unexpected, count_mismatch, Unmatched}}.

%% @doc Stop and fail test if there are unexpected errors or count mismatches
-spec check() -> ok.
check() ->
    case stop() of
        ok ->
            ok;
        {error, {unexpected_errors, Errors}} ->
            ct:fail("Unexpected error logs:~n~s", [format_errors_for_report(Errors)]);
        {error, {count_mismatch, Unmatched}} ->
            ct:fail("Expected error count mismatch:~n~s", [format_unmatched(Unmatched)]);
        {error, {unexpected_errors, Errors, count_mismatch, Unmatched}} ->
            ct:fail("Unexpected error logs:~n~s~nExpected error count mismatch:~n~s",
                    [format_errors_for_report(Errors), format_unmatched(Unmatched)])
    end.

%% @doc Declare an expected error pattern. Any number of matching errors is allowed.
-spec expect(pattern()) -> true.
expect(Pattern) ->
    expect(Pattern, any).

%% @doc Declare an expected error pattern with expected count.
%% Count can be 'any' or a positive integer.
-spec expect(pattern(), count()) -> true.
expect(Pattern, Count) ->
    ets:insert(?STATUS_TABLE, {Pattern, Count}).

%% Internal functions

-spec classify_errors([log_entry()], [{pattern(), count()}]) ->
    {Unexpected :: [log_entry()],
     Unmatched :: [{pattern(), count()}]}.
classify_errors(Errors, Expected) ->
    %% For each error, try to match against expected patterns
    %% Track which patterns were used and how many times
    {RemainingErrors, UsedPatterns} = lists:foldl(
        fun(Error, {AccErrors, AccUsed}) ->
            case find_matching_pattern(Error, Expected) of
                {ok, Pattern} ->
                    %% Error matched a pattern, don't include in unexpected
                    {AccErrors, increment_pattern_usage(Pattern, AccUsed)};
                none ->
                    %% No pattern matched, this is unexpected
                    {[Error | AccErrors], AccUsed}
            end
        end,
        {[], #{}},
        Errors),

    %% Find patterns that were expected but never matched
    Unmatched = find_unmatched_patterns(Expected, UsedPatterns),

    {lists:reverse(RemainingErrors), Unmatched}.

-spec find_matching_pattern(log_entry(), [{pattern(), count()}]) ->
    {ok, pattern()} | none.
find_matching_pattern({_Timestamp, _Level, Msg, Meta}, Expected) ->
    find_matching_pattern_impl(Msg, Meta, Expected).

find_matching_pattern_impl(_Msg, _Meta, []) ->
    none;
find_matching_pattern_impl(Msg, Meta, [{Pattern, _Count} | Rest]) ->
    case matches_pattern(Msg, Meta, Pattern) of
        true -> {ok, Pattern};
        false -> find_matching_pattern_impl(Msg, Meta, Rest)
    end.

%% Pattern matching on structured log data
-spec matches_pattern(msg(), meta(), pattern()) -> boolean().

%% Match on 'what' key in report - most common pattern
matches_pattern({report, Report}, _Meta, {what, What}) when is_map(Report) ->
    maps:get(what, Report, undefined) =:= What;

%% Match on source module from metadata
matches_pattern(_Msg, #{mfa := {Module, _, _}}, {module, Module}) ->
    true;
matches_pattern(_Msg, _Meta, {module, _}) ->
    false;

%% Match on source function from metadata
matches_pattern(_Msg, #{mfa := {_, Function, _}}, {function, Function}) ->
    true;
matches_pattern(_Msg, _Meta, {function, _}) ->
    false;

%% Match on full MFA with wildcards
matches_pattern(_Msg, #{mfa := {M, F, A}}, {mfa, {PM, PF, PA}}) ->
    (PM =:= '_' orelse PM =:= M) andalso
    (PF =:= '_' orelse PF =:= F) andalso
    (PA =:= '_' orelse PA =:= A);
matches_pattern(_Msg, _Meta, {mfa, _}) ->
    false;

%% Match on 'reason' key in report
matches_pattern({report, Report}, _Meta, {reason, Reason}) when is_map(Report) ->
    maps:get(reason, Report, undefined) =:= Reason;

%% Regex match - format message first (must be before generic key match)
matches_pattern(Msg, _Meta, {regex, Regex}) ->
    FormattedMsg = format_msg(Msg),
    case re:run(FormattedMsg, Regex) of
        {match, _} -> true;
        nomatch -> false
    end;

%% Generic key match in report
matches_pattern({report, Report}, _Meta, {Key, Value}) when is_atom(Key), is_map(Report) ->
    maps:get(Key, Report, undefined) =:= Value;

%% Binary substring match - format message first
matches_pattern(Msg, _Meta, Pattern) when is_binary(Pattern) ->
    FormattedMsg = format_msg(Msg),
    binary:match(FormattedMsg, Pattern) =/= nomatch;

%% Custom function - receives both msg and meta
matches_pattern(Msg, Meta, Fun) when is_function(Fun, 2) ->
    try Fun(Msg, Meta)
    catch _:_ -> false
    end;

%% Legacy: function with single argument (msg only)
matches_pattern(Msg, _Meta, Fun) when is_function(Fun, 1) ->
    try Fun(Msg)
    catch _:_ -> false
    end;

%% Fallback
matches_pattern(_Msg, _Meta, _Pattern) ->
    false.

%% Format msg to binary for string-based matching
-spec format_msg(msg()) -> binary().
format_msg({string, String}) when is_list(String) ->
    unicode:characters_to_binary(String);
format_msg({string, Binary}) when is_binary(Binary) ->
    Binary;
format_msg({report, Report}) when is_map(Report) ->
    unicode:characters_to_binary(io_lib:format("~0p", [Report]));
format_msg({Format, Args}) when is_list(Format), is_list(Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args));
format_msg(Other) ->
    unicode:characters_to_binary(io_lib:format("~0p", [Other])).

-spec increment_pattern_usage(pattern(), #{pattern() => pos_integer()}) ->
    #{pattern() => pos_integer()}.
increment_pattern_usage(Pattern, Used) ->
    maps:update_with(Pattern, fun(N) -> N + 1 end, 1, Used).

-type mismatch_info() :: {pattern(), count(), Actual :: non_neg_integer()}.

-spec find_unmatched_patterns([{pattern(), count()}], #{pattern() => pos_integer()}) ->
    [mismatch_info()].
find_unmatched_patterns(Expected, UsedPatterns) ->
    lists:filtermap(
        fun({Pattern, Count}) ->
            Actual = maps:get(Pattern, UsedPatterns, 0),
            case is_count_satisfied(Count, Actual) of
                true -> false;
                false -> {true, {Pattern, Count, Actual}}
            end
        end,
        Expected).

-spec is_count_satisfied(count(), non_neg_integer()) -> boolean().
is_count_satisfied(any, Actual) -> Actual > 0;
is_count_satisfied(N, Actual) when is_integer(N) -> Actual =:= N;
is_count_satisfied({at_least, N}, Actual) -> Actual >= N;
is_count_satisfied({at_most, N}, Actual) -> Actual =< N.

-spec format_unmatched([mismatch_info()]) -> iolist().
format_unmatched(Unmatched) ->
    lists:map(fun format_single_unmatched/1, Unmatched).

-spec format_single_unmatched(mismatch_info()) -> iolist().
format_single_unmatched({Pattern, ExpectedCount, ActualCount}) ->
    io_lib:format("  Pattern: ~p~n    expected: ~s, actual: ~w~n",
                  [Pattern, format_expected_count(ExpectedCount), ActualCount]).

-spec format_expected_count(count()) -> string().
format_expected_count(any) -> "any (at least 1)";
format_expected_count(N) when is_integer(N) -> integer_to_list(N);
format_expected_count({at_least, N}) -> "at least " ++ integer_to_list(N);
format_expected_count({at_most, N}) -> "at most " ++ integer_to_list(N).

-spec format_errors_for_report([log_entry()]) -> iolist().
format_errors_for_report(Errors) ->
    lists:map(fun format_single_error/1, Errors).

-spec format_single_error(log_entry()) -> iolist().
format_single_error({_Timestamp, Level, Msg, Meta}) ->
    MsgFormatted = format_msg(Msg),
    Location = format_location(Meta),
    What = extract_what(Msg),
    case What of
        undefined ->
            io_lib:format("  [~p]~s ~s~n", [Level, Location, MsgFormatted]);
        WhatAtom ->
            io_lib:format("  [~p]~s what=~p~n    ~s~n", [Level, Location, WhatAtom, MsgFormatted])
    end.

-spec format_location(meta()) -> iolist().
format_location(#{mfa := {M, F, A}}) ->
    io_lib:format(" ~p:~p/~p", [M, F, A]);
format_location(_) ->
    "".

-spec extract_what(msg()) -> atom() | undefined.
extract_what({report, #{what := What}}) -> What;
extract_what(_) -> undefined.
