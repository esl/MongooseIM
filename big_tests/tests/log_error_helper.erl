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
%% - binary() - Substring match in error message
%% - {regex, binary()} - Regex match
%% - {what, atom()} - Match #{what := Atom} in structured logs
%% - fun((Msg) -> boolean()) - Custom filter

-module(log_error_helper).

%% Setup/teardown API
-export([start/0, start/1, stop/0, check/0]).

%% API for expecting errors
-export([expect/1, expect/2]).

-import(distributed_helper, [rpc/4, mim/0]).

-define(STATUS_TABLE, log_error_status_table).
-define(COLLECTOR, log_error_collector).

-type pattern() :: binary()
                 | {regex, binary()}
                 | {what, atom()}
                 | fun((binary()) -> boolean()).
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
-spec stop() -> ok | {error, unexpected, [{integer(), atom(), binary()}]}.
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

-spec classify_errors([{integer(), atom(), binary()}], [{pattern(), count()}]) ->
    {Unexpected :: [{integer(), atom(), binary()}],
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

-spec find_matching_pattern({integer(), atom(), binary()}, [{pattern(), count()}]) ->
    {ok, pattern()} | none.
find_matching_pattern({_Timestamp, _Level, Msg}, Expected) ->
    find_matching_pattern_msg(Msg, Expected).

find_matching_pattern_msg(_Msg, []) ->
    none;
find_matching_pattern_msg(Msg, [{Pattern, _Count} | Rest]) ->
    case matches_pattern(Msg, Pattern) of
        true -> {ok, Pattern};
        false -> find_matching_pattern_msg(Msg, Rest)
    end.

-spec matches_pattern(binary(), pattern()) -> boolean().
matches_pattern(Msg, Pattern) when is_binary(Pattern) ->
    %% Substring match
    binary:match(Msg, Pattern) =/= nomatch;
matches_pattern(Msg, {regex, Regex}) ->
    case re:run(Msg, Regex) of
        {match, _} -> true;
        nomatch -> false
    end;
matches_pattern(Msg, {what, What}) when is_atom(What) ->
    %% Match #{what := Atom} in structured logs
    WhatBin = atom_to_binary(What),
    %% Look for patterns like "what => atom" or "what := atom"
    Pattern = <<"what => ", WhatBin/binary>>,
    Pattern2 = <<"what := ", WhatBin/binary>>,
    binary:match(Msg, Pattern) =/= nomatch orelse
    binary:match(Msg, Pattern2) =/= nomatch;
matches_pattern(Msg, Fun) when is_function(Fun, 1) ->
    try Fun(Msg)
    catch _:_ -> false
    end.

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

-spec report_unexpected([{integer(), atom(), binary()}]) ->
    ok | {error, unexpected, [{integer(), atom(), binary()}]}.
report_unexpected([]) ->
    ok;
report_unexpected(Errors) ->
    ct:log("Unexpected error logs:~n~s", [format_errors_for_report(Errors)]),
    {error, unexpected, Errors}.

-spec format_errors_for_report([{integer(), atom(), binary()}]) -> iolist().
format_errors_for_report(Errors) ->
    lists:map(
        fun({_Timestamp, Level, Msg}) ->
            io_lib:format("  [~p] ~s~n", [Level, Msg])
        end,
        Errors).
