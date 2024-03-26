%% @doc This module is injected to the mim node by instrument_helper.
%% It collects events in an ETS table.

-module(mongoose_instrument_event_table).

-define(TABLE, ?MODULE).

%% API
-export([get_events/2, all_keys/0]).

%% mongoose_instrument callbacks
-export([start/0, stop/0, set_up/3, handle_event/4]).

start() ->
    ets_helper:new(?TABLE, [bag]). % repeating measurements are stored only once

stop() ->
    ets_helper:delete(?TABLE).

set_up(EventName, Labels, _Config) ->
    DeclaredEvents = mongoose_config:get_opt([instrumentation, event_table, declared_events]),
    lists:member({EventName, Labels}, DeclaredEvents).

handle_event(EventName, Labels, _Config, Measurements) ->
    ets:insert(?TABLE, {{EventName, Labels}, Measurements}).

get_events(EventName, Labels) ->
    ets:lookup(?TABLE, {EventName, Labels}).

all_keys() ->
    all_keys(?TABLE).

all_keys(Tab) ->
    all_keys(Tab, ets:first(Tab), []).

all_keys(_Tab, '$end_of_table', Acc) ->
    lists:sort(Acc);
all_keys(Tab, Key, Acc) ->
    all_keys(Tab, ets:next(Tab, Key), [Key|Acc]).
