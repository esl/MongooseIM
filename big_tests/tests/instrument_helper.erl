%% @doc A helper module for checking instrumentation events in tests.
%% For now it always uses mim(). We can extend it to other nodes when needed.

-module(instrument_helper).

-export([declared_events/1, declared_events/2,
         start/1, stop/0,
         assert/3, assert/4,
         assert_not_emitted/2, assert_not_emitted/3,
         wait_for/2, wait_for_new/2,
         lookup/2, take/2]).

-import(distributed_helper, [rpc/4, mim/0]).

-include_lib("eunit/include/eunit.hrl").

-define(STATUS_TABLE, instrument_event_status_table).
-define(HANDLER_MODULE, mongoose_instrument_event_table).

-type event_name() :: atom().
-type labels() :: #{atom() => term()}.
-type measurements() :: #{atom() => term()}.

%% API

%% @doc Helper to get `DeclaredEvents' needed by `start/1'
declared_events(Modules) ->
    declared_events(Modules, [domain_helper:host_type()]).

-spec declared_events([module()] | module(), Args :: list()) -> [{event_name(), labels()}].
declared_events(Modules, Args) when is_list(Modules) ->
    lists:flatmap(fun(Module) -> declared_events(Module, Args) end, Modules);
declared_events(Module, Args) ->
    Specs = rpc(mim(), Module, instrumentation, Args),
    [{Event, Labels} || {Event, Labels, _Config} <- Specs].

%% @doc Only `DeclaredEvents' will be logged, and can be tested with `assert/3'
-spec start([{event_name(), labels()} | module()]) -> ok.
start(DeclaredEvents) ->
    mongoose_helper:inject_module(ets_helper),
    mongoose_helper:inject_module(?HANDLER_MODULE),
    ets_helper:new(?STATUS_TABLE),
    [ets:insert(?STATUS_TABLE, {Event, untested}) || Event <- DeclaredEvents],
    rpc(mim(), mongoose_instrument, add_handler,
        [event_table, #{declared_events => DeclaredEvents}]).

-spec stop() -> ok.
stop() ->
    #{tested := Tested, untested := Untested} = classify_events(),
    ets_helper:delete(?STATUS_TABLE),
    Logged = rpc(mim(), ?HANDLER_MODULE, all_keys, []),
    rpc(mim(), mongoose_instrument, remove_handler, [event_table]),
    ct:log("Tested instrumentation events:~n~p", [lists:sort(Tested)]),
    verify_unlogged(Untested -- Logged),
    verify_logged_but_untested(Logged -- Tested).

-spec assert(event_name(), labels(), fun((measurements()) -> boolean())) -> ok.
assert(EventName, Labels, CheckF) ->
    assert(EventName, Labels, lookup(EventName, Labels), CheckF).

%% @doc `CheckF' can return a boolean or fail with `function_clause', which means `false'.
%% This is for convenience - you only have to code one clause.
-spec assert(event_name(), labels(), [measurements()], fun((measurements()) -> boolean())) -> ok.
assert(EventName, Labels, MeasurementsList, CheckF) ->
    case lists:filter(fun(Measurements) ->
                              try CheckF(Measurements) catch error:function_clause -> false end
                      end, MeasurementsList) of
        [] ->
            ct:log("All measurements for event ~p with labels ~p:~n~p",
                   [EventName, Labels, MeasurementsList]),
            ct:fail("No instrumentation events matched");
        Filtered ->
            ct:log("Matching measurements for event ~p with labels ~p:~n~p",
                   [EventName, Labels, Filtered]),
            event_tested(EventName, Labels)
    end.

assert_not_emitted(EventName, Labels, MarkAsTested) ->
    case lookup(EventName, Labels) of
        [] ->
            event_tested(EventName, Labels);
        Events ->
            ct:fail("Measurements emitted but should not ~p", [Events])
    end.

assert_not_emitted(Events, MarkAsTested) ->
    [assert_not_emitted(Event, Label, MarkAsTested)
     || {Event, Label} <- Events].

%% @doc Remove previous events, and wait for a new one. Use for probes only.
-spec wait_for_new(event_name(), labels()) -> [measurements()].
wait_for_new(EventName, Labels) ->
    take(EventName, Labels),
    wait_for(EventName, Labels).

%% @doc Lookup an element, or wait for it, if it didn't happen yet.
-spec wait_for(event_name(), labels()) -> [measurements()].
wait_for(EventName, Labels) ->
    {ok, MeasurementsList} = mongoose_helper:wait_until(fun() -> lookup(EventName, Labels) end,
                                                        fun(L) -> L =/= [] end,
                                                        #{name => EventName}),
    MeasurementsList.

-spec lookup(event_name(), labels()) -> [measurements()].
lookup(EventName, Labels) ->
    [Measurements || {_, Measurements} <- rpc(mim(), ?HANDLER_MODULE, lookup, [EventName, Labels])].

-spec take(event_name(), labels()) -> [measurements()].
take(EventName, Labels) ->
    [Measurements || {_, Measurements} <- rpc(mim(), ?HANDLER_MODULE, take, [EventName, Labels])].

%% Internal functions

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
    ets:foldl(fun classify_event/2, #{tested => [], untested => []}, ?STATUS_TABLE).

classify_event({Event, Status}, M) ->
    M#{Status => [Event | maps:get(Status, M)]}.
