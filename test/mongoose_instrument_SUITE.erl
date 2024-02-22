-module(mongoose_instrument_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(HANDLER, mongoose_instrument_test_handler).
-define(INACTIVE_HANDLER, mongoose_instrument_inactive_handler).
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
     unexpected_events].

init_per_suite(Config) ->
    mongoose_config:set_opts(opts()),
    mock_handler(?HANDLER, true),
    mock_handler(?INACTIVE_HANDLER, false),
    Config.

mock_handler(Module, SetUpResult) ->
    meck:new(Module, [non_strict, no_link]),
    meck:expect(Module, set_up, fun(_Event, #{}, #{}) -> SetUpResult end),
    meck:expect(Module, handle_event, fun(_Event, #{}, #{}, #{}) -> ok end).

end_per_suite(_Config) ->
    meck:unload(?HANDLER),
    meck:unload(?INACTIVE_HANDLER),
    mongoose_config:erase_opts().

init_per_group(Group, Config) ->
    meck:reset(?HANDLER),
    meck:reset(?INACTIVE_HANDLER),
    Config1 = async_helper:start(Config, mongoose_instrument, start_link, []),
    set_up_persistence(Group),
    Config1.

set_up_persistence(persistent) ->
    mongoose_instrument:persist(),
    ?assertEqual(#{}, persistent_term:get(mongoose_instrument));
set_up_persistence(not_persistent) ->
    ?assertError(badarg, persistent_term:get(mongoose_instrument)).

end_per_group(_Group, Config) ->
    async_helper:stop_all(Config),
    mongoose_helper:wait_until(fun() -> whereis(mongoose_instrument) end, undefined),
    ?assertError(badarg, persistent_term:get(mongoose_instrument)).

init_per_testcase(Case, Config) ->
    [{event, event_name(Case)} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

opts() ->
    #{instrumentation => #{test_handler => #{}, inactive_handler => #{}}}.

event_name(Case) ->
    list_to_atom(atom_to_list(Case) ++ "_event").

%% Test cases

set_up_and_execute(Config) ->
    Event = ?config(event, Config),
    ok = mongoose_instrument:set_up(Event, ?LABELS, ?CFG),
    ?assertEqual([{[Event, ?LABELS, ?CFG], true}], history(?HANDLER, set_up, Event)),
    ?assertEqual([{[Event, ?LABELS, ?CFG], false}], history(?INACTIVE_HANDLER, set_up, Event)),
    ok = mongoose_instrument:execute(Event, ?LABELS, ?MEASUREMENTS),
    ?assertEqual([{[Event, ?LABELS, ?CFG, ?MEASUREMENTS], ok}],
                 history(?HANDLER, handle_event, Event)),
    ?assertEqual([], history(?INACTIVE_HANDLER, handle_event, Event)).

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

unexpected_events(_Config) ->
    Pid = whereis(mongoose_instrument),
    {error, #{what := unexpected_call}} = gen_server:call(mongoose_instrument, bad_call),
    gen_server:cast(mongoose_instrument, bad_cast),
    mongoose_instrument ! bad_info,
    ?assertEqual(Pid, whereis(mongoose_instrument)). %% It should be still working

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
