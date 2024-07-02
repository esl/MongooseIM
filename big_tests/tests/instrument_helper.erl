%% @doc A helper module for checking instrumentation events in tests.
%% For now it always uses mim(). We can extend it to other nodes when needed.

-module(instrument_helper).

%% API for setup and teardown in test suites
-export([declared_events/1, declared_events/2, start/1, start/2, stop/0]).

%% API for assertions in test cases
-export([assert/3, assert_one/3, assert_not_emitted/3, assert_not_emitted/2, assert_not_emitted/1,
         wait_and_assert/3, wait_and_assert_new/3, assert/4]).

-import(distributed_helper, [rpc/4, mim/0]).

-include_lib("eunit/include/eunit.hrl").

-define(STATUS_TABLE, instrument_event_status_table).
-define(HANDLER_MODULE, mongoose_instrument_event_table).

-type event_name() :: atom().
-type labels() :: #{atom() => term()}.
-type measurements() :: #{atom() => term()}.
-type check_fun() :: fun((measurements()) -> boolean()).
-type event_count() :: non_neg_integer() | positive.
-type opts() :: #{timestamp => integer(),
                  retries := non_neg_integer(),
                  delay := non_neg_integer(),
                  expected_count := event_count()}.

%% API for setup and teardown in test suites

%% @doc Helper to get `DeclaredEvents' needed by `start/1'
declared_events(Modules) ->
    declared_events(Modules, [domain_helper:host_type()]).

-spec declared_events([module()] | module(), Args :: list()) -> [{event_name(), labels()}].
declared_events(Modules, Args) when is_list(Modules) ->
    lists:flatmap(fun(Module) -> declared_events(Module, Args) end, Modules);
declared_events(Module, Args) ->
    Specs = rpc(mim(), Module, instrumentation, Args),
    [{Event, Labels} || {Event, Labels, _Config} <- Specs].

-spec start([{event_name(), labels()}]) -> ok.
start(DeclaredEvents) ->
    start(DeclaredEvents, []).

%% @doc Only `DeclaredEvents' will be logged, and can be tested with `assert/3'
-spec start([{event_name(), labels()}], [{event_name(), labels()}]) -> ok.
start(DeclaredEvents, NegativeEvents) ->
    mongoose_helper:inject_module(ets_helper),
    mongoose_helper:inject_module(?HANDLER_MODULE),
    ets_helper:new(?STATUS_TABLE),
    [ets:insert(?STATUS_TABLE, {Event, negative}) || Event <- NegativeEvents],
    [ets:insert(?STATUS_TABLE, {Event, untested}) || Event <- DeclaredEvents],
    ok = rpc(mim(), mongoose_instrument, add_handler,
        [event_table, #{declared_events => DeclaredEvents ++ NegativeEvents}]).

-spec stop() -> ok.
stop() ->
    #{tested := Tested, untested := Untested, negative := Negative} = classify_events(),
    ets_helper:delete(?STATUS_TABLE),
    Logged = rpc(mim(), ?HANDLER_MODULE, all_keys, []),
    rpc(mim(), mongoose_instrument, remove_handler, [event_table]),
    ct:log("Tested instrumentation events:~n~p", [lists:sort(Tested)]),
    verify_unlogged((Untested -- Logged) -- Negative),
    verify_logged_but_untested((Logged -- Tested) -- Negative).

%% API for assertions in test cases

%% @doc Checks that there is at least one event with `EventName', `Labels'
%%      and matching measurements.
 -spec assert(event_name(), labels(), check_fun()) -> ok.
assert(EventName, Labels, CheckF) ->
    assert(EventName, Labels, CheckF, #{}).

%% @doc Checks that there is exactly one event with `EventName', `Labels'
%%      and matching measurements.
-spec assert_one(event_name(), labels(), check_fun()) -> ok.
assert_one(EventName, Labels, CheckF) ->
    assert(EventName, Labels, CheckF, #{expected_count => 1}).

%% @doc Checks that there is no event with `EventName', `Labels' and matching measurements.
-spec assert_not_emitted(event_name(), labels(), check_fun()) -> ok.
assert_not_emitted(EventName, Labels, CheckF) ->
    assert(EventName, Labels, CheckF, #{expected_count => 0}).

%% @doc Checks that there is no event with `EventName' and `Labels'.
-spec assert_not_emitted(event_name(), labels()) -> ok.
assert_not_emitted(EventName, Labels) ->
    assert_not_emitted(EventName, Labels, fun(_) -> true end).

%% @doc Checks that there is no event for any of the `{EventName, Labels}' tuples.
-spec assert_not_emitted([{event_name(), labels()}]) -> ok.
assert_not_emitted(Events) ->
    [assert_not_emitted(Event, Label) || {Event, Label} <- Events],
    ok.

%% @doc Waits for a matching event.
-spec wait_and_assert(event_name(), labels(), check_fun()) -> ok.
wait_and_assert(EventName, Labels, CheckF) ->
    assert(EventName, Labels, CheckF, #{retries => 50, delay => 100}).

%% @doc Waits for a matching event, ignoring past events.
-spec wait_and_assert_new(event_name(), labels(), check_fun()) -> ok.
wait_and_assert_new(EventName, Labels, CheckF) ->
    assert(EventName, Labels, CheckF, #{timestamp => timestamp(), retries => 50, delay => 100}).

%% @doc Assert that an expected number of events with `EventName' and `Labels' are present.
%% Events are filtered by applying `CheckF' to the map of measurements.
%% `CheckF' can return a boolean or fail with `function_clause', which means `false'.
%% This is for convenience - you only have to code one clause.
-spec assert(event_name(), labels(), check_fun(), opts()) -> ok.
assert(EventName, Labels, CheckF, Opts) ->
    FullOpts = maps:merge(default_opts(), Opts),
    assert_loop(EventName, Labels, CheckF, FullOpts).

%% Low-level API

-spec filter(fun((measurements()) -> boolean()), [measurements()]) -> [measurements()].
filter(CheckF, MeasurementsList) ->
    lists:filter(fun(Measurements) ->
                         try CheckF(Measurements) catch error:function_clause -> false end
                 end, MeasurementsList).

-spec select(event_name(), labels()) -> [measurements()].
select(EventName, Labels) ->
    rpc(mim(), ?HANDLER_MODULE, select, [EventName, Labels]).

-spec select_new(event_name(), labels(), integer()) -> [measurements()].
select_new(EventName, Labels, Timestamp) ->
    rpc(mim(), ?HANDLER_MODULE, select_new, [EventName, Labels, Timestamp]).

-spec timestamp() -> integer().
timestamp() ->
    rpc(mim(), ?HANDLER_MODULE, timestamp, []).

%% Internal functions

-spec assert_loop(event_name(), labels(), check_fun(), opts()) -> ok.
assert_loop(EventName, Labels, CheckF, Opts) ->
    #{retries := Retries, expected_count := ExpectedCount, delay := Delay} = Opts,
    All = case Opts of
              #{timestamp := Timestamp} ->
                  select_new(EventName, Labels, Timestamp);
              #{} ->
                  select(EventName, Labels)
          end,
    Filtered = filter(CheckF, All),
    case check(Filtered, ExpectedCount) of
        false when Retries > 0 ->
            timer:sleep(Delay),
            assert_loop(EventName, Labels, CheckF, Opts#{retries := Retries - 1});
        CheckResult ->
            assert_check_result(EventName, Labels, All, Filtered, CheckResult, ExpectedCount)
    end.

-spec default_opts() -> opts().
default_opts() ->
    #{retries => 0, delay => timer:seconds(1), expected_count => positive}.

-spec check([measurements()], event_count()) -> boolean().
check(Filtered, positive) ->
    length(Filtered) > 0;
check(Filtered, ExpectedCount) ->
    length(Filtered) =:= ExpectedCount.

-spec assert_check_result(event_name(), labels(), All :: [measurements()],
                          Filtered :: [measurements()], CheckResult :: boolean(),
                          event_count()) -> ok.
assert_check_result(_EventName, _Labels, _All, [], true, _ExpectedCount) ->
    ok; % don't mark events as tested
assert_check_result(EventName, Labels, _All, Filtered, true, _ExpectedCount) ->
    ct:log("Matching measurements for event ~p with labels ~p:~n~p", [EventName, Labels, Filtered]),
    event_tested(EventName, Labels);
assert_check_result(EventName, Labels, All, Filtered, false, ExpectedCount) ->
    ct:log("All measurements for event ~p with labels ~p:~n~p", [EventName, Labels, All]),
    ct:fail("Incorrect number of instrumentation events - matched: ~p, expected: ~p",
            [length(Filtered), ExpectedCount]).

%% Don't fail if some events are unlogged, because we don't have full test coverage (yet)
verify_unlogged([]) -> ok;
verify_unlogged(Events) ->
    ct:log("Instrumentation events that were not logged - functionality not covered by tests:~n~p",
           [lists:sort(Events)]).

%% Fail if any events were logged, but there were no assertions for them
verify_logged_but_untested([]) -> ok;
verify_logged_but_untested(Events) ->
    ct:fail("Instrumentation events that were logged, but not tested:~n~p~n"
            "You need to test them with ~p:assert/3",
            [lists:sort(Events), ?MODULE]).

event_tested(EventName, Labels) ->
    ets:insert(?STATUS_TABLE, {{EventName, Labels}, tested}),
    ok.

classify_events() ->
    ets:foldl(fun classify_event/2, #{tested => [], untested => [], negative => []}, ?STATUS_TABLE).

classify_event({Event, Status}, M) ->
    M#{Status => [Event | maps:get(Status, M)]}.
