%% @doc This module is injected to the mim node by instrument_helper.
%% It collects events in an ETS table.

-module(mongoose_instrument_event_table).

-define(TABLE, ?MODULE).

%% API
-export([start/1, stop/0, get_events/2]).

%% mongoose_instrument callbacks
-export([set_up/3, handle_event/4]).

start(DeclaredEvents) ->
    ets_helper:new(?TABLE, [bag]), % repeating measurements are stored only once
    mongoose_instrument:add_handler(event_table, #{declared_events => DeclaredEvents}).

stop() ->
    mongoose_instrument:remove_handler(event_table),
    Logged = all_keys(?TABLE),
    ets_helper:delete(?TABLE),
    {ok, Logged}.

set_up(EventName, Labels, _Config) ->
    DeclaredEvents = mongoose_config:get_opt([instrumentation, event_table, declared_events]),
    lists:member({EventName, Labels}, DeclaredEvents).

handle_event(EventName, Labels, _Config, Measurements) ->
    ets:insert(?TABLE, {{EventName, Labels}, Measurements}).

get_events(EventName, Labels) ->
    ets:lookup(?TABLE, {EventName, Labels}).

all_keys(Tab) ->
    all_keys(Tab, ets:first(Tab), []).

all_keys(_Tab, '$end_of_table', Acc) ->
    lists:sort(Acc);
all_keys(Tab, Key, Acc) ->
    all_keys(Tab, ets:next(Tab, Key), [Key|Acc]).
