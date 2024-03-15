%% @doc This module is injected to the mim node by instrument_helper.
%% It collects events in an ETS table.

-module(instrument_event_table).

-export([set_up/1, tear_down/0, get_events/2]).

set_up(DeclaredEvents) ->
    ets_helper:new(?MODULE, [bag]), % repeating measurements are stored only once
    meck:new(mongoose_instrument_log, [no_link, passthrough]),
    meck:expect(mongoose_instrument_log, handle_event,
                fun(Event, Labels, _Config, Measurements) ->
                        case lists:member({Event, Labels}, DeclaredEvents) of
                            true -> ets:insert(?MODULE, {{Event, Labels}, Measurements});
                            false -> ok
                        end,
                        meck:passthrough()
                end).

tear_down() ->
    meck:unload(mongoose_instrument_log),
    Logged = all_keys(?MODULE),
    ets_helper:delete(?MODULE),
    {ok, Logged}.

get_events(EventName, Labels) ->
    ets:lookup(?MODULE, {EventName, Labels}).

all_keys(Tab) ->
    all_keys(Tab, ets:first(Tab), []).

all_keys(_Tab, '$end_of_table', Acc) ->
    lists:sort(Acc);
all_keys(Tab, Key, Acc) ->
    all_keys(Tab, ets:next(Tab, Key), [Key|Acc]).
