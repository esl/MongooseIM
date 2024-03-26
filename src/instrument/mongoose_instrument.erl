-module(mongoose_instrument).

-behaviour(gen_server).

%% API
-export([config_spec/0,
         start_link/0, persist/0,
         set_up/1, set_up/3,
         tear_down/1, tear_down/2,
         span/4, span/5,
         execute/3]).

%% Test API
-export([add_handler/2, remove_handler/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2, terminate/2]).

-ignore_xref([start_link/0, set_up/3, tear_down/2, span/4, add_handler/2, remove_handler/1]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-type event_name() :: atom().
-type labels() :: #{host_type => mongooseim:host_type()}. % to be extended
-type label_key() :: host_type. % to be extended
-type label_value() :: mongooseim:host_type(). % to be extended
-type metrics() :: #{metric_name() => metric_type()}.
-type metric_name() :: atom().
-type metric_type() :: spiral | histogram. % to be extended
-type measurements() :: #{atom() => term()}.
-type spec() :: {event_name(), labels(), config()}.
-type config() :: #{metrics => metrics()}. % to be extended
-type handler_key() :: atom(). % key in the `instrumentation' section of the config file
-type handler_fun() :: fun((event_name(), labels(), config(), measurements()) -> any()).
-type handlers() :: {[handler_fun()], config()}.
-type execution_time() :: integer().
-type measure_fun(Result) :: fun((execution_time(), Result) -> measurements()).

-callback config_spec() -> mongoose_config_spec:config_section().
-callback start() -> ok.
-callback stop() -> ok.
-callback set_up(event_name(), labels(), config()) -> boolean().
-callback handle_event(event_name(), labels(), config(), measurements()) -> any().

-optional_callbacks([config_spec/0, start/0, stop/0]).

-export_type([event_name/0, labels/0, label_key/0, label_value/0, config/0, measurements/0,
              spec/0, handlers/0, metric_name/0, metric_type/0]).

%% API

%% @doc Specifies the `instrumentation' section of the config file
-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Items = [{atom_to_binary(Key), config_spec(Key)} || Key <- all_handler_keys()],
    #section{items = maps:from_list(Items),
             wrap = global_config,
             include = always}.

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

%% Test API

-spec add_handler(handler_key(), mongoose_config:value()) -> ok.
add_handler(Key, ConfigVal) ->
    case gen_server:call(?MODULE, {add_handler, Key, ConfigVal}) of
        ok -> ok;
        {error, ErrorMap} -> error(ErrorMap)
    end.

-spec remove_handler(handler_key()) -> ok.
remove_handler(Key) ->
    case gen_server:call(?MODULE, {remove_handler, Key}) of
        ok -> ok;
        {error, ErrorMap} -> error(ErrorMap)
    end.

%% gen_server callbacks

-type state() :: #{event_name() => #{labels() => handlers()}}.

-spec init([]) -> {ok, state()}.
init([]) ->
    lists:foreach(fun start_handler/1, handler_modules()),
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
handle_call({add_handler, Key, ConfigOpts}, _From, State) ->
    case mongoose_config:lookup_opt([instrumentation, Key]) of
        {error, not_found} ->
            mongoose_config:set_opt([instrumentation, Key], ConfigOpts),
            Module = handler_module(Key),
            start_handler(Module),
            NewState = update_handlers(State, [], [Module]),
            update_if_persisted(State, NewState),
            {reply, ok, NewState};
        {ok, ExistingConfig} ->
            {reply, {error, #{what => handler_already_configured, handler_key => Key,
                              existing_config => ExistingConfig}},
             State}
    end;
handle_call({remove_handler, Key}, _From, State) ->
    case mongoose_config:lookup_opt([instrumentation, Key]) of
        {error, not_found} ->
            {reply, {error, #{what => handler_not_configured, handler_key => Key}}, State};
        {ok, _} ->
            mongoose_config:unset_opt([instrumentation, Key]),
            Module = handler_module(Key),
            NewState = update_handlers(State, [Module], []),
            update_if_persisted(State, NewState),
            stop_handler(Module),
            {reply, ok, NewState}
    end;
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
    lists:foreach(fun stop_handler/1, handler_modules()).

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
    HandlerFuns = set_up_handlers(EventName, Labels, Config, handler_modules()),
    {HandlerFuns, Config}.

-spec update_handlers(state(), [module()], [module()]) -> state().
update_handlers(State, ToRemove, ToAdd) ->
    maps:map(fun(EventName, HandlerMap) ->
                     maps:map(fun(Labels, Handlers) ->
                                      update_event_handlers(EventName, Labels, Handlers,
                                                            ToRemove, ToAdd)
                              end, HandlerMap)
             end, State).

-spec update_event_handlers(event_name(), labels(), handlers(), [module()], [module()]) ->
          handlers().
update_event_handlers(EventName, Labels, {HandlerFuns, Config}, ToRemove, ToAdd) ->
    FunsToRemove = modules_to_funs(ToRemove),
    FunsToAdd = set_up_handlers(EventName, Labels, Config, ToAdd),
    HandlerFuns = HandlerFuns -- FunsToAdd, % sanity check to prevent duplicates
    {(HandlerFuns -- FunsToRemove) ++ FunsToAdd, Config}.

-spec set_up_handlers(event_name(), labels(), config(), [module()]) -> [handler_fun()].
set_up_handlers(EventName, Labels, Config, Modules) ->
    UsedModules = lists:filter(fun(Mod) -> Mod:set_up(EventName, Labels, Config) end, Modules),
    modules_to_funs(UsedModules).

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
handle_event(EventName, Labels, Measurements, {EventHandlers, Config}) ->
    lists:foreach(fun(HandlerFun) ->
                          call_handler(HandlerFun, EventName, Labels, Config, Measurements)
                  end, EventHandlers).

-spec modules_to_funs([module()]) -> [handler_fun()].
modules_to_funs(Modules) ->
    [fun Module:handle_event/4 || Module <- Modules].

-spec handler_modules() -> [module()].
handler_modules() ->
    [handler_module(Key) || Key <- maps:keys(mongoose_config:get_opt(instrumentation))].

-spec handler_module(handler_key()) -> module().
handler_module(Key) ->
    list_to_existing_atom("mongoose_instrument_" ++ atom_to_list(Key)).

-spec config_spec(handler_key()) -> mongoose_config_spec:config_section().
config_spec(Key) ->
    Module = handler_module(Key),
    case mongoose_lib:is_exported(Module, config_spec, 0) of
        true -> Module:config_spec();
        false -> #section{}
    end.

-spec all_handler_keys() -> [handler_key()].
all_handler_keys() ->
    [prometheus, exometer, log].

-spec start_handler(module()) -> ok.
start_handler(Module) ->
    case mongoose_lib:is_exported(Module, start, 0) of
        true -> Module:start();
        false -> ok
    end.

-spec stop_handler(module()) -> ok.
stop_handler(Module) ->
    case mongoose_lib:is_exported(Module, stop, 0) of
        true -> Module:stop();
        false -> ok
    end.

-spec call_handler(handler_fun(), event_name(), labels(), config(), measurements()) -> any().
call_handler(HandlerFun, EventName, Labels, Config, Measurements) ->
    try
        HandlerFun(EventName, Labels, Config, Measurements)
    catch
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => event_handler_failed,
                         handler_fun => HandlerFun,
                         event_name => EventName, labels => Labels, config => Config,
                         measurements => Measurements,
                         class => Class, reason => Reason, stacktrace => StackTrace})
    end.
