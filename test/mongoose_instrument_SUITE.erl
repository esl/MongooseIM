-module(mongoose_instrument_SUITE).
-compile([export_all, nowarn_export_all]).

-include("log_helper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(HANDLER, mongoose_instrument_test_handler).
-define(INACTIVE_HANDLER, mongoose_instrument_inactive_handler).
-define(ADDED_HANDLER, mongoose_instrument_added_handler).
-define(FAILING_HANDLER, mongoose_instrument_failing_handler).
-define(LABELS, #{key => value}).
-define(CFG, #{metrics => #{time => histogram}}).
-define(MEASUREMENTS, #{count => 1}).

%% Setup and teardown

all() ->
    [{group, persistent},
     {group, not_persistent}
    ].

groups() ->
    [{persistent, [parallel], api_test_cases()},
     {not_persistent, [parallel], api_test_cases()}].

api_test_cases() ->
    [set_up_and_execute,
     set_up_multiple_and_execute_one,
     set_up_fails_when_already_registered,
     set_up_fails_for_inconsistent_labels,
     set_up_and_tear_down,
     set_up_and_tear_down_multiple,
     execute_fails_when_not_set_up,
     set_up_and_span,
     set_up_and_span_with_arg,
     set_up_and_span_with_error,
     span_fails_when_not_set_up,
     set_up_probe,
     set_up_probe_with_incorrect_metric_type,
     set_up_failing_probe,
     set_up_and_tear_down_probe,
     unexpected_events,
     add_and_remove_handler,
     cannot_add_existing_handler,
     cannot_remove_non_existing_handler].

init_per_suite(Config) ->
    log_helper:set_up(),
    mongoose_config:set_opts(opts()),
    maps:map(fun mock_handler/2, mocked_handlers()),
    Config.

mock_handler(Module, {SetUpResult, HandleEventF}) ->
    meck:new(Module, [non_strict, no_link]),
    meck:expect(Module, set_up, fun(_Event, #{}, #{}) -> SetUpResult end),
    meck:expect(Module, handle_event, fun(_Event, #{}, #{}, #{}) -> HandleEventF() end).

end_per_suite(_Config) ->
    lists:foreach(fun meck:unload/1, maps:keys(mocked_handlers())),
    mongoose_config:erase_opts(),
    log_helper:tear_down().

init_per_group(Group, Config) ->
    lists:foreach(fun meck:reset/1, maps:keys(mocked_handlers())),
    Config1 = async_helper:start(Config, mongoose_instrument, start_link, []),
    set_up_persistence(Group),
    Config1.

init_per_testcase(Case, Config) ->
    log_helper:subscribe(),
    [{event, event_name(Case)} | Config].

end_per_testcase(_Case, _Config) ->
    log_helper:unsubscribe().

mocked_handlers() ->
    #{?HANDLER => {true, fun() -> ok end},
      ?INACTIVE_HANDLER => {false, fun() -> ok end},
      ?ADDED_HANDLER => {true, fun() -> ok end},
      ?FAILING_HANDLER => {true, fun() -> error(failed) end}}.

set_up_persistence(persistent) ->
    mongoose_instrument:persist(),
    ?assertEqual(#{}, persistent_term:get(mongoose_instrument));
set_up_persistence(not_persistent) ->
    ?assertError(badarg, persistent_term:get(mongoose_instrument)).

end_per_group(_Group, Config) ->
    async_helper:stop_all(Config),
    wait_helper:wait_until(fun() -> whereis(mongoose_instrument) end, undefined),
    ?assertError(badarg, persistent_term:get(mongoose_instrument)).

opts() ->
    #{instrumentation => #{test_handler => #{}, inactive_handler => #{}, failing_handler => #{}}}.

event_name(Case) ->
    list_to_atom(atom_to_list(Case) ++ "_event").

%% Test cases

set_up_and_execute(Config) ->
    Event = ?config(event, Config),
    Measurements = ?MEASUREMENTS,
    Labels = ?LABELS,
    ok = mongoose_instrument:set_up(Event, ?LABELS, ?CFG),
    ?assertEqual([{[Event, Labels, ?CFG], true}], history(?HANDLER, set_up, Event)),
    ?assertEqual([{[Event, Labels, ?CFG], false}], history(?INACTIVE_HANDLER, set_up, Event)),
    ?assertEqual([{[Event, Labels, ?CFG], true}], history(?FAILING_HANDLER, set_up, Event)),

    ok = mongoose_instrument:execute(Event, Labels, Measurements),
    ?assertEqual([{[Event, Labels, ?CFG, Measurements], ok}],
                 history(?HANDLER, handle_event, Event)),
    ?assertEqual([], history(?INACTIVE_HANDLER, handle_event, Event)),
    HandlerFun = fun ?FAILING_HANDLER:handle_event/4,
    ?assertLog(error, #{what := event_handler_failed,
                        class := error, reason := failed, stacktrace := _,
                        event_name := Event, labels := Labels, measurements := Measurements,
                        handler_fun := HandlerFun}).

set_up_multiple_and_execute_one(Config) ->
    Event = ?config(event, Config),
    Specs = [{Event, Labels1 = #{key => value1}, ?CFG},
             {Event, Labels2 = #{key => value2}, ?CFG}],
    ok = mongoose_instrument:set_up(Specs),
    ?assertEqual([{[Event, Labels1, ?CFG], true}, {[Event, Labels2, ?CFG], true}],
                 history(?HANDLER, set_up, Event)),
    ok = mongoose_instrument:execute(Event, Labels1, ?MEASUREMENTS),
    ?assertEqual([{[Event, Labels1, ?CFG, ?MEASUREMENTS], ok}],
                 history(?HANDLER, handle_event, Event)).

set_up_fails_when_already_registered(Config) ->
    Event = ?config(event, Config),
    ok = mongoose_instrument:set_up(Event, ?LABELS, ?CFG),
    ?assertError(#{what := event_already_registered},
                 mongoose_instrument:set_up(Event, ?LABELS, ?CFG)),
    ?assertError(#{what := event_already_registered},
                 mongoose_instrument:set_up(Event, ?LABELS, #{})).

set_up_fails_for_inconsistent_labels(Config) ->
    Event = ?config(event, Config),
    Labels = ?LABELS,
    ok = mongoose_instrument:set_up(Event, Labels, ?CFG),
    ?assertError(#{what := inconsistent_labels},
                 mongoose_instrument:set_up(Event, #{}, ?CFG)),
    ?assertError(#{what := inconsistent_labels},
                 mongoose_instrument:set_up(Event, Labels#{additional_key => value}, ?CFG)),
    ?assertError(#{what := inconsistent_labels},
                 mongoose_instrument:set_up(Event, #{different_key => value}, ?CFG)).

set_up_and_tear_down(Config) ->
    Event = ?config(event, Config),
    ok = mongoose_instrument:set_up(Event, ?LABELS, ?CFG),
    ok = mongoose_instrument:tear_down(Event, ?LABELS),
    ?assertError(#{what := event_not_registered},
                 mongoose_instrument:execute(Event, ?LABELS, ?MEASUREMENTS)),
    [] = history(?HANDLER, handle_event, Event),
    ok = mongoose_instrument:tear_down(Event, ?LABELS). % idempotent

set_up_and_tear_down_multiple(Config) ->
    Event = ?config(event, Config),
    Specs = [{Event, Labels1 = #{key => value1}, ?CFG},
             {Event, Labels2 = #{key => value2}, ?CFG}],
    ok = mongoose_instrument:set_up(Specs),
    ok = mongoose_instrument:tear_down(Specs),
    ?assertError(#{what := event_not_registered},
                 mongoose_instrument:execute(Event, Labels1, ?MEASUREMENTS)),
    ?assertError(#{what := event_not_registered},
                 mongoose_instrument:execute(Event, Labels2, ?MEASUREMENTS)),
    [] = history(?HANDLER, handle_event, Event).

execute_fails_when_not_set_up(Config) ->
    Event = ?config(event, Config),
    ?assertError(#{what := event_not_registered},
                 mongoose_instrument:execute(Event, ?LABELS, ?MEASUREMENTS)),
    [] = history(?HANDLER, handle_event, Event).

set_up_and_span(Config) ->
    {Event, Labels, InstrConfig} = {?config(event, Config), ?LABELS, ?CFG},
    ok = mongoose_instrument:set_up(Event, Labels, InstrConfig),
    ok = mongoose_instrument:span(Event, Labels, fun test_op/0, fun measure_test_op/2),
    [{[Event, Labels, InstrConfig, #{time := Time, result := ok}], ok}] =
        history(?HANDLER, handle_event, Event),
    ?assert(Time >= 1000).

set_up_and_span_with_arg(Config) ->
    {Event, Labels, InstrConfig} = {?config(event, Config), ?LABELS, ?CFG},
    ok = mongoose_instrument:set_up(Event, Labels, InstrConfig),
    ok = mongoose_instrument:span(Event, Labels, fun test_op/1, [2], fun measure_test_op/2),
    [{[Event, Labels, InstrConfig, #{time := Time, result := ok}], ok}] =
        history(?HANDLER, handle_event, Event),
    ?assert(Time >= 2000).

set_up_and_span_with_error(Config) ->
    Event = ?config(event, Config),
    ok = mongoose_instrument:set_up(Event, ?LABELS, ?CFG),
    ?assertError(simulated_error,
                 mongoose_instrument:span(Event, ?LABELS, fun crashing_op/0, fun measure_test_op/2)),
    [] = history(?HANDLER, handle_event, Event).

span_fails_when_not_set_up(Config) ->
    Event = ?config(event, Config),
    Labels = #{key => value},
    ?assertError(#{what := event_not_registered},
                 mongoose_instrument:span(Event, Labels, fun test_op/0, fun measure_test_op/2)),
    %% Also checks that the function is not executed - otherwise 'simulated_error' would be raised
    ?assertError(#{what := event_not_registered},
                 mongoose_instrument:span(Event, Labels, fun crashing_op/0, fun measure_test_op/2)),
    [] = history(?HANDLER, handle_event, Event).

set_up_probe(Config) ->
    Event = ?config(event, Config),
    Cfg = #{probe => #{module => ?MODULE, interval => 1}},
    ok = mongoose_instrument:set_up(Event, ?LABELS, Cfg),
    ExpectedMeasurements = #{event => Event, count => 1},

    %% Wait until the probe is called
    {ok, History1} = wait_helper:wait_until(fun() -> history(?HANDLER, handle_event, Event) end,
                                            [], #{validator => fun(L) -> length(L) > 0 end}),
    ExpectedEvent = {[Event, ?LABELS, Cfg, ExpectedMeasurements], ok},
    ?assertEqual([ExpectedEvent], History1),

    %% Wait until it's called again to ensure that it is done periodically
    {ok, History2} = wait_helper:wait_until(fun() -> history(?HANDLER, handle_event, Event) end,
                                            [], #{validator => fun(L) -> length(L) > 1 end}),
    ?assertEqual([ExpectedEvent, ExpectedEvent], History2).

set_up_probe_with_incorrect_metric_type(Config) ->
    Event = ?config(event, Config),
    Cfg = #{metrics => #{test_metric => gauge},
            probe => #{module => ?MODULE, interval => 1}},
    Cfg1 = #{metrics => ImproperMetrics = #{test_metric => spiral},
             probe => #{module => ?MODULE, interval => 1}},
    Specs = [{Event, #{key => value1}, Cfg},
             {Event, #{key => value2}, Cfg1}],
    ?assertError(#{what := non_gauge_metrics_in_probe, improper_metrics := ImproperMetrics},
                 mongoose_instrument:set_up(Event, ?LABELS, Cfg1)),
    ?assertError(#{what := non_gauge_metrics_in_probe, improper_metrics := ImproperMetrics},
                 mongoose_instrument:set_up(Specs)).

set_up_failing_probe(Config) ->
    Event = ?config(event, Config),
    Labels = ?LABELS,
    Cfg = #{probe => #{module => ?MODULE, interval => 1}},
    ok = mongoose_instrument:set_up(Event, Labels, Cfg),

    %% Wait until the error appears in logs
    ?assertLog(error, #{what := probe_failed,
                        class := error, reason := simulated_error, stacktrace := _,
                        event_name := Event, labels := Labels, probe_mod := ?MODULE}, 5000),

    %% Make sure that the failing probe is not disabled
    ?assertLog(error, #{what := probe_failed,
                        class := error, reason := simulated_error, stacktrace := _,
                        event_name := Event, labels := Labels, probe_mod := ?MODULE}, 5000).

set_up_and_tear_down_probe(Config) ->
    Event = ?config(event, Config),
    Cfg = #{probe => #{module => ?MODULE, interval => 1}},
    ok = mongoose_instrument:set_up(Event, ?LABELS, Cfg),
    mongoose_instrument:tear_down(Event, ?LABELS),

    %% The probe shouldn't be called anymore
    ?assertError(_,
                 wait_helper:wait_until(fun() -> history(?HANDLER, handle_event, Event) end,
                                        [], #{validator => fun(L) -> length(L) > 0 end,
                                              time_left => timer:seconds(2)})).

unexpected_events(_Config) ->
    Pid = whereis(mongoose_instrument),
    {error, #{what := unexpected_call}} = gen_server:call(mongoose_instrument, bad_call),
    gen_server:cast(mongoose_instrument, bad_cast),
    mongoose_instrument ! bad_info,
    ?assertEqual(Pid, whereis(mongoose_instrument)). %% It should be still working

add_and_remove_handler(Config) ->
    Event = ?config(event, Config),
    ok = mongoose_instrument:set_up(Event, Labels1 = #{key => value1}, ?CFG),

    %% Add handler
    ok = mongoose_instrument:add_handler(added_handler, #{cfg_key => cfg_val}),
    ?assertEqual(#{cfg_key => cfg_val}, mongoose_config:get_opt([instrumentation, added_handler])),
    ?assertEqual([{[Event, Labels1, ?CFG], true}], history(?ADDED_HANDLER, set_up, Event)),
    %% Make sure it's still possible to set up new events
    ok = mongoose_instrument:set_up(Event, Labels2 = #{key => value2}, ?CFG),

    ok = mongoose_instrument:execute(Event, Labels1, ?MEASUREMENTS),
    ok = mongoose_instrument:execute(Event, Labels2, ?MEASUREMENTS),
    History = history(?ADDED_HANDLER, handle_event, Event),
    ?assertEqual([{[Event, Labels1, ?CFG, ?MEASUREMENTS], ok},
                  {[Event, Labels2, ?CFG, ?MEASUREMENTS], ok}], History),

    %% Remove handler
    ok = mongoose_instrument:remove_handler(added_handler),
    ?assertEqual({error, not_found}, mongoose_config:lookup_opt([instrumentation, added_handler])),
    ok = mongoose_instrument:execute(Event, Labels1, ?MEASUREMENTS),
    ok = mongoose_instrument:execute(Event, Labels2, ?MEASUREMENTS),
    ?assertEqual(History, history(?ADDED_HANDLER, handle_event, Event)). % no new events

cannot_add_existing_handler(_Config) ->
    ?assertError(#{what := handler_already_configured},
                 mongoose_instrument:add_handler(test_handler, #{})).

cannot_remove_non_existing_handler(_Config) ->
    ?assertError(#{what := handler_not_configured},
                 mongoose_instrument:remove_handler(unknown_handler)).

%% Helpers

history(HandlerMod, WantedF, WantedEvent) ->
    [{Args, Result} || {_Pid, {_M, CalledF, [Event|_] = Args}, Result} <- meck:history(HandlerMod),
                       WantedF =:= CalledF andalso WantedEvent =:= Event].

test_op(Delay) ->
    timer:sleep(Delay).

test_op() ->
    test_op(1).

crashing_op() ->
    error(simulated_error).

measure_test_op(Time, Result) ->
    #{time => Time, result => Result}.

probe(set_up_failing_probe_event, _Labels) ->
    error(simulated_error);
probe(Event, Labels) ->
    ?assertEqual(?LABELS, Labels),
    #{event => Event, count => 1}.
