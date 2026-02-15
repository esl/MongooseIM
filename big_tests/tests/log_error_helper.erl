%% @doc A helper module for checking error logs in tests.
%% Captures error logs from MIM nodes and fails tests if unexpected errors are found.
%% Tests can declare expected errors using expect/1,2 to avoid false failures.
%%
%% Usage:
%% - Call start/0 in init_per_group/suite
%% - Call expect/1,2 before code that is expected to produce errors
%% - Call check/0 in end_per_group/suite (fails if unexpected errors)
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
-type count() :: any | pos_integer().

%% API

%% @doc Start capturing errors on mim() node
-spec start() -> ok | {error, term()}.
start() ->
    start(#{}).

-spec start(#{levels => [atom()]}) -> ok | {error, term()}.
start(Opts) ->
    Levels = maps:get(levels, Opts, [error]),
    mongoose_helper:inject_module(?COLLECTOR),
    ets_helper:new(?STATUS_TABLE, [bag]),
    rpc(mim(), ?COLLECTOR, start, [Levels]).

%% @doc Stop capturing and return classification result
-spec stop() -> ok | {error, unexpected, [log_entry()]}.
stop() ->
    Errors = rpc(mim(), ?COLLECTOR, get_errors, []),
    rpc(mim(), ?COLLECTOR, stop, []),
    Expected = ets:tab2list(?STATUS_TABLE),
    ets_helper:delete(?STATUS_TABLE),
    {Unexpected, Unmatched} = classify_errors(Errors, Expected),
    report_unmatched(Unmatched),
    report_unexpected(Unexpected).

%% @doc Stop and fail test if there are unexpected errors
-spec check() -> ok.
check() ->
    case stop() of
        ok ->
            ok;
        {error, unexpected, Errors} ->
            ct:fail("Unexpected error logs:~n~s", [format_errors_for_report(Errors)])
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

%% Generic key match in report
matches_pattern({report, Report}, _Meta, {Key, Value}) when is_atom(Key), is_map(Report) ->
    maps:get(Key, Report, undefined) =:= Value;

%% Binary substring match - format message first
matches_pattern(Msg, _Meta, Pattern) when is_binary(Pattern) ->
    FormattedMsg = format_msg(Msg),
    binary:match(FormattedMsg, Pattern) =/= nomatch;

%% Regex match - format message first
matches_pattern(Msg, _Meta, {regex, Regex}) ->
    FormattedMsg = format_msg(Msg),
    case re:run(FormattedMsg, Regex) of
        {match, _} -> true;
        nomatch -> false
    end;

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

-spec find_unmatched_patterns([{pattern(), count()}], #{pattern() => pos_integer()}) ->
    [{pattern(), count()}].
find_unmatched_patterns(Expected, UsedPatterns) ->
    lists:filter(
        fun({Pattern, Count}) ->
            Actual = maps:get(Pattern, UsedPatterns, 0),
            case Count of
                any -> Actual =:= 0;
                N when is_integer(N) -> Actual =/= N
            end
        end,
        Expected).

-spec report_unmatched([{pattern(), count()}]) -> ok.
report_unmatched([]) ->
    ok;
report_unmatched(Unmatched) ->
    ct:log("Warning: Expected error patterns that were not matched:~n~p", [Unmatched]).

-spec report_unexpected([log_entry()]) ->
    ok | {error, unexpected, [log_entry()]}.
report_unexpected([]) ->
    ok;
report_unexpected(Errors) ->
    ct:log("Unexpected error logs:~n~s", [format_errors_for_report(Errors)]),
    {error, unexpected, Errors}.

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
