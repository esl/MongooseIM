%% @doc This module is injected to the mim node by instrument_helper.
%% It collects events in an ETS table.

-module(mongoose_instrument_event_table).

-define(TABLE, ?MODULE).

%% API
-export([select/2, select_new/3, all_keys/0, timestamp/0]).

%% mongoose_instrument callbacks
-export([start/1, stop/1, set_up/3, handle_event/4]).

start(_Opts) ->
    ets_helper:new(?TABLE, [duplicate_bag]).

stop(_Opts) ->
    ets_helper:delete(?TABLE).

set_up(EventName, Labels, _Config) ->
    DeclaredEvents = mongoose_config:get_opt([instrumentation, event_table, declared_events]),
    lists:member({EventName, Labels}, DeclaredEvents).

handle_event(EventName, Labels, _Config, Measurements) ->
    ets:insert(?TABLE, {{EventName, Labels}, Measurements, timestamp()}).

select(EventName, Labels) ->
    ets:select(?TABLE, [{{{EventName, Labels}, '$2', '$3'}, [], ['$2']}]).

select_new(EventName, Labels, Timestamp) ->
    ets:select(?TABLE, [{{{EventName, Labels}, '$2', '$3'}, [{'>=', '$3', Timestamp}], ['$2']}]).

timestamp() ->
    erlang:monotonic_time().

all_keys() ->
    all_keys(?TABLE).

all_keys(Tab) ->
    all_keys(Tab, ets:first(Tab), []).

all_keys(_Tab, '$end_of_table', Acc) ->
    lists:sort(Acc);
all_keys(Tab, Key, Acc) ->
    all_keys(Tab, ets:next(Tab, Key), [Key|Acc]).
