%% @doc A helper module for checking instrumentation events in tests.
%% For now it always uses mim(). We can extend it to other nodes when needed.

-module(instrument_helper).

-export([declared_events/1, start/1, stop/0, assert/3]).

-import(distributed_helper, [rpc/4, mim/0]).

-include_lib("eunit/include/eunit.hrl").

-define(STATUS_TABLE, instrument_event_status_table).
-define(HANDLER_MODULE, mongoose_instrument_event_table).

-type event_name() :: atom().
-type labels() :: #{atom() => term()}.
-type measurements() :: #{atom() => term()}.

%% API

%% @doc Helper to get `DeclaredEvents' needed by `start/1'
-spec declared_events([module()] | module()) -> [{event_name(), labels()}].
declared_events(Modules) when is_list(Modules) ->
    lists:flatmap(fun declared_events/1, Modules);
declared_events(Module) ->
    Specs = rpc(mim(), Module, instrumentation, [domain_helper:host_type()]),
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

%% @doc `CheckF' can return a boolean or fail with `function_clause', which means `false'.
%% This is for convenience - you only have to code one clause.
-spec assert(event_name(), labels(), fun((measurements()) -> boolean())) -> ok.
assert(EventName, Labels, CheckF) ->
    Events = rpc(mim(), ?HANDLER_MODULE, get_events, [EventName, Labels]),
    case lists:filter(fun({_, Measurements}) ->
                              try CheckF(Measurements) catch error:function_clause -> false end
                      end, Events) of
        [] ->
            ct:log("All ~p events with labels ~p:~n~p", [EventName, Labels, Events]),
            ct:fail("No instrumentation events matched");
        Filtered ->
            ct:log("Matching events: ~p", [Filtered]),
            event_tested(EventName, Labels)
    end.

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
