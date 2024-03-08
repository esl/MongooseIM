-module(mongoose_instrument).

-behaviour(gen_server).

%% API
-export([start_link/0, persist/0,
         set_up/1, set_up/3,
         tear_down/1, tear_down/2,
         span/4, span/5,
         execute/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2, terminate/2]).

-ignore_xref([start_link/0, set_up/3, tear_down/2, span/4, span/5, execute/3]).

-include("mongoose.hrl").

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

%% API

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Saves the state to a persistent term, improving performance of `execute' and `span'.
%% On the other hand, future calls to `set_up' or `tear_down' will update the persistent term,
%% which makes them less performant.
%% You should call this function only once - after the initial setup, but before handling any load.
-spec persist() -> ok.
persist() ->
    gen_server:call(?MODULE, persist).

%% @doc Sets up instrumentation for multiple events.
%% @see set_up/3
-spec set_up([spec()]) -> ok.
set_up(Specs) ->
    lists:foreach(fun({EventName, Labels, Config}) -> set_up(EventName, Labels, Config) end, Specs).

%% @doc Tears down instrumentation for multiple events.
%% @see tear_down/2
-spec tear_down([spec()]) -> ok.
tear_down(Specs) ->
    lists:foreach(fun({EventName, Labels, _Config}) -> tear_down(EventName, Labels) end, Specs).

%% @doc Sets up instrumentation for an event identified by `EventName' and `Labels'
%% according to `Config'. Fails if the event is already registered, or if the keys of `Labels'
%% are different than for already registered events with `EventName'.
-spec set_up(event_name(), labels(), config()) -> ok.
set_up(EventName, Labels, Config) ->
    case gen_server:call(?MODULE, {set_up, EventName, Labels, Config}) of
        ok -> ok;
        {error, ErrorMap} -> error(ErrorMap)
    end.

%% @doc Tears down instrumentation for an event identified by `EventName' and `Labels'.
%% This operation is idempotent.
-spec tear_down(event_name(), labels()) -> ok.
tear_down(EventName, Labels) ->
    gen_server:call(?MODULE, {tear_down, EventName, Labels}).

%% @doc Calls `F', measuring its result with `MeasureF', and calls attached event handlers.
%% @see span/5
-spec span(event_name(), labels(), fun(() -> Result), measure_fun(Result)) -> Result.
span(EventName, Labels, F, MeasureF) ->
    span(EventName, Labels, F, [], MeasureF).

%% @doc Calls `F' with `Args', measuring its execution time.
%% The time and the result are passed to `MeasureF', which returns measurements.
%% The measurements are then passed to all handlers attached to
%% the event identified by `EventName' and `Labels'.
%% Fails without calling `F' if the event is not registered.
-spec span(event_name(), labels(), fun((...) -> Result), list(), measure_fun(Result)) -> Result.
span(EventName, Labels, F, Args, MeasureF) ->
    Handlers = get_handlers(EventName, Labels),
    {Time, Result} = timer:tc(F, Args),
    handle_event(EventName, Labels, MeasureF(Time, Result), Handlers),
    Result.

%% @doc Executes all handlers attached to the event identified by `EventName' and `Labels',
%% passing `Measurements' to them. Fails if the event is not registered.
-spec execute(event_name(), labels(), measurements()) -> ok.
execute(EventName, Labels, Measurements) ->
    Handlers = get_handlers(EventName, Labels),
    handle_event(EventName, Labels, Measurements, Handlers).

%% gen_server callbacks

-type state() :: #{event_name() => #{labels() => handlers()}}.

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(trap_exit, true), % Make sure that terminate is called
    persistent_term:erase(?MODULE), % Prevent inconsistency when restarted after a kill
    {ok, #{}}.

-spec handle_call(any(), gen_server:from(), state()) ->
          {reply, ok | {ok, handlers()} | {error, map()}, state()}.
handle_call({set_up, EventName, Labels, Config}, _From, State) ->
    case set_up_and_register(EventName, Labels, Config, State) of
        {error, _} = Error ->
            {reply, Error, State};
        NewState = #{} ->
            update_if_persisted(State, NewState),
            {reply, ok, NewState}
    end;
handle_call({tear_down, EventName, Labels}, _From, State) ->
    NewState = deregister(EventName, Labels, State),
    update_if_persisted(State, NewState),
    {reply, ok, NewState};
handle_call(persist, _From, State) ->
    persistent_term:put(?MODULE, State),
    {reply, ok, State};
handle_call({lookup, EventName, Labels}, _From, State) ->
    {reply, lookup(EventName, Labels, State), State};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, {error, #{what => unexpected_call, request => Request}}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    persistent_term:erase(?MODULE),
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

-spec update_if_persisted(state(), state()) -> ok.
update_if_persisted(State, NewState) ->
    try persistent_term:get(?MODULE) of
        State -> persistent_term:put(?MODULE, NewState)
    catch
        error:badarg -> ok
    end.

-spec set_up_and_register(event_name(), labels(), config(), state()) -> state() | {error, map()}.
set_up_and_register(EventName, Labels, Config, State) ->
    LabelKeys = label_keys(Labels),
    case State of
        #{EventName := #{Labels := _}} ->
            {error, #{what => event_already_registered,
                      event_name => EventName, labels => Labels}};
        #{EventName := HandlerMap} ->
            {ExistingLabels, _, _} = maps:next(maps:iterator(HandlerMap)),
            case label_keys(ExistingLabels) of
                LabelKeys ->
                    Handlers = do_set_up(EventName, Labels, Config),
                    State#{EventName := HandlerMap#{Labels => Handlers}};
                ExistingKeys ->
                    {error, #{what => inconsistent_labels,
                              event_name => EventName, labels => Labels,
                              existing_label_keys => ExistingKeys}}
            end;
        #{} ->
            Handlers = do_set_up(EventName, Labels, Config),
            State#{EventName => #{Labels => Handlers}}
    end.

-spec do_set_up(event_name(), labels(), config()) -> handlers().
do_set_up(EventName, Labels, Config) ->
    AllModules = handler_modules(),
    UsedModules = lists:filter(fun(Mod) -> Mod:set_up(EventName, Labels, Config) end, AllModules),
    {[fun Mod:handle_event/4 || Mod <- UsedModules], Config}.

-spec deregister(event_name(), labels(), state()) -> state().
deregister(EventName, Labels, State) ->
    case State of
        #{EventName := HandlerMap} ->
            case maps:remove(Labels, HandlerMap) of
                Empty when Empty =:= #{} ->
                    maps:remove(EventName, State);
                NewHandlerMap ->
                    State#{EventName := NewHandlerMap}
            end;
        #{} ->
            State
    end.

-spec lookup(event_name(), labels()) -> {ok, handlers()} | {error, map()}.
lookup(EventName, Labels) ->
    try persistent_term:get(?MODULE) of
        State ->
            lookup(EventName, Labels, State)
    catch
        %% Although persist/0 should be called before handling traffic,
        %% some instrumented events might happen before that, and they shouldn't fail.
        error:badarg ->
            ?LOG_INFO(#{what => mongoose_instrument_lookup_without_persistent_term,
                        event_name => EventName, labels => Labels}),
            gen_server:call(?MODULE, {lookup, EventName, Labels})
    end.

-spec lookup(event_name(), labels(), state()) -> {ok, handlers()} | {error, map()}.
lookup(EventName, Labels, State) ->
    case State of
        #{EventName := #{Labels := Handlers}} ->
            {ok, Handlers};
        #{} ->
            {error, #{what => event_not_registered, event_name => EventName, labels => Labels}}
    end.

-spec label_keys(labels()) -> [atom()].
label_keys(Labels) ->
    lists:sort(maps:keys(Labels)).

-spec get_handlers(event_name(), labels()) -> handlers().
get_handlers(EventName, Labels) ->
    case lookup(EventName, Labels) of
        {ok, Handlers} -> Handlers;
        {error, ErrorMap} -> error(ErrorMap)
    end.

-spec handle_event(event_name(), labels(), measurements(), handlers()) -> ok.
handle_event(Event, Labels, Measurements, {EventHandlers, Config}) ->
    lists:foreach(fun(Handler) -> Handler(Event, Labels, Config, Measurements) end, EventHandlers).

-spec handler_modules() -> [module()].
handler_modules() ->
    [list_to_existing_atom("mongoose_instrument_" ++ atom_to_list(Key))
     || Key <- maps:keys(mongoose_config:get_opt(instrumentation))].
