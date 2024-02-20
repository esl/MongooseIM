-module(mongoose_instrument).

-export([set_up/1, set_up/3,
         tear_down/1, tear_down/2,
         span/4, span/5,
         execute/3]).

-ignore_xref([set_up/3, tear_down/2, span/4, span/5, execute/3]).

-type event_name() :: atom().
-type labels() :: #{host_type => mongooseim:host_type()}. % to be extended
-type metrics() :: #{atom() => spiral | histogram}. % to be extended
-type measurements() :: #{atom() => integer() | atom() | binary()}.
-type spec() :: {event_name(), labels(), config()}.
-type config() :: #{metrics => metrics()}. % to be extended
-type handler_fun() :: fun((event_name(), labels(), config(), measurements()) -> any()).
-type handlers() :: {[handler_fun()], config()}.
-type execution_time() :: integer().
-type measure_fun(Result) :: fun((execution_time(), Result) -> measurements()).

-callback set_up(event_name(), labels(), config()) -> boolean().
-callback handle_event(event_name(), labels(), config(), measurements()) -> any().

-export_type([event_name/0, labels/0, config/0, measurements/0, spec/0, handlers/0]).

-spec set_up([spec()]) -> ok.
set_up(Specs) ->
    lists:foreach(fun({EventName, Labels, Config}) -> set_up(EventName, Labels, Config) end, Specs).

-spec tear_down([spec()]) -> ok.
tear_down(Specs) ->
    lists:foreach(fun({EventName, Labels, _Config}) -> tear_down(EventName, Labels) end, Specs).

-spec set_up(event_name(), labels(), config()) -> ok.
set_up(EventName, Labels, Config) ->
    AllModules = handler_modules(),
    UsedModules = lists:filter(fun(Mod) -> Mod:set_up(EventName, Labels, Config) end, AllModules),
    HandlerFuns = [fun Mod:handle_event/4 || Mod <- UsedModules],
    mongoose_instrument_registry:attach(EventName, Labels, {HandlerFuns, Config}).

-spec tear_down(event_name(), labels()) -> ok.
tear_down(EventName, Labels) ->
    mongoose_instrument_registry:detach(EventName, Labels).

-spec span(event_name(), labels(), fun(() -> Result), measure_fun(Result)) -> Result.
span(Event, Labels, F, MeasureF) ->
    span(Event, Labels, F, [], MeasureF).

-spec span(event_name(), labels(), fun((...) -> Result), list(), measure_fun(Result)) -> Result.
span(Event, Labels, F, Args, MeasureF) ->
    {ok, Handlers} = mongoose_instrument_registry:lookup(Event, Labels),
    {Time, Result} = timer:tc(F, Args),
    handle_event(Event, Labels, MeasureF(Time, Result), Handlers),
    Result.

-spec execute(event_name(), labels(), measurements()) -> ok.
execute(Event, Labels, Measurements) ->
    {ok, Handlers} = mongoose_instrument_registry:lookup(Event, Labels),
    handle_event(Event, Labels, Measurements, Handlers).

-spec handle_event(event_name(), labels(), measurements(), handlers()) -> ok.
handle_event(Event, Labels, Measurements, {EventHandlers, Config}) ->
    lists:foreach(fun(Handler) -> Handler(Event, Labels, Config, Measurements) end, EventHandlers).

-spec handler_modules() -> [module()].
handler_modules() ->
    [list_to_existing_atom("mongoose_instrument_" ++ atom_to_list(Key))
     || Key <- maps:keys(mongoose_config:get_opt(instrumentation))].
