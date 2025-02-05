-module(mongoose_instrument).

-behaviour(gen_server).

%% API
-export([config_spec/0,
         start_link/0, persist/0,
         set_up/1, set_up/3,
         tear_down/1, tear_down/2,
         span/4, span/5, span/6,
         execute/3]).

%% Test API
-export([add_handler/2, remove_handler/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-ignore_xref([start_link/0, set_up/3, tear_down/2, span/4, add_handler/2, remove_handler/1]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-type event_name() :: atom().
-type labels() :: #{host_type => mongooseim:host_type(),
                    connection_type => mongoose_listener:connection_type(),
                    function => atom(),
                    cache_name => atom(),
                    pool_id => atom(),
                    pool_tag => mongoose_wpool:tag()}. % to be extended
-type label_key() :: host_type | function | cache_name | pool_id | pool_tag. % to be extended
-type label_value() :: mongooseim:host_type() | atom() | mongoose_wpool:tag(). % to be extended
-type metrics() :: #{metric_name() => metric_type()}.
-type metric_name() :: atom().

-type metric_type() :: gauge % last value (integer)
                     | counter % sum of integers
                     | spiral % sum of non-negative integers - total and in a time window
                     | histogram. % statistics of integers
-type measurements() :: #{atom() => term()}.
-type spec() :: {event_name(), labels(), config()}.
-type config() :: #{metrics => metrics(),
                    loglevel => logger:level(),
                    probe => probe_config()}.
-type probe_config() :: #{module := module(),
                          interval => pos_integer()}.
-type handler_key() :: atom(). % key in the `instrumentation' section of the config file
-type handler_fun() :: fun((event_name(), labels(), config(), measurements()) -> any()).
-type handlers() :: {[handler_fun()], config()}.
-type handler_map() :: #{labels() => handlers()}.
-type execution_time() :: integer().
-type measure_fun(Result) :: fun((execution_time(), Result) -> measurements()).
-type handler_module_opts() :: #{atom() => any()}.

-callback config_spec() -> mongoose_config_spec:config_section().
-callback start(handler_module_opts()) -> ok.
-callback stop(handler_module_opts()) -> ok.
-callback set_up(event_name(), labels(), config()) -> boolean().
-callback handle_event(event_name(), labels(), config(), measurements()) -> any().

-optional_callbacks([config_spec/0, start/1, stop/1]).

-export_type([event_name/0, labels/0, label_key/0, label_value/0, config/0, measurements/0,
              spec/0, handlers/0, metrics/0, metric_name/0, metric_type/0, probe_config/0]).

%% API

%% @doc Specifies the `instrumentation' section of the config file
-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Items = [{atom_to_binary(Key), config_spec(Key)} || Key <- all_handler_keys()],
    Options = #{<<"probe_interval">> => #option{type = integer, validate = positive}},
    #section{items = maps:merge(maps:from_list(Items), Options),
             defaults = #{<<"probe_interval">> => 15},
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

%% @doc Calls `Mod:F' with `Args', measuring its execution time.
%% @see span/5
-spec span(event_name(), labels(), module(), atom(), list(), measure_fun(Result)) -> Result.
span(EventName, Labels, Mod, F, Args, MeasureF) ->
    Handlers = get_handlers(EventName, Labels),
    {Time, Result} = timer:tc(Mod, F, Args),
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

-type state() :: #{events := event_map(), probe_timers := probe_timer_map()}.
-type event_map() :: #{event_name() => #{labels() => handlers()}}.
-type probe_timer_map() :: #{{event_name(), labels()} => timer:tref()}.

-spec init([]) -> {ok, state()}.
init([]) ->
    [start_handler(handler_module(Key), Opts)
     || {Key, Opts = #{}} <- maps:to_list(mongoose_config:get_opt(instrumentation))],
    erlang:process_flag(trap_exit, true), % Make sure that terminate is called
    persistent_term:erase(?MODULE), % Prevent inconsistency when restarted after a kill
    {ok, #{events => #{}, probe_timers => #{}}}.

-spec handle_call(any(), gen_server:from(), state()) ->
          {reply, ok | {ok, handlers()} | {error, map()}, state()} |
          {reply, ok | {ok, handlers()} | {error, map()}, state(), hibernate}.
handle_call({set_up, EventName, Labels, Config}, _From,
            #{events := Events, probe_timers := ProbeTimers} = State) ->
    case set_up_and_register_event(EventName, Labels, Config, Events) of
        {error, _} = Error ->
            {reply, Error, State};
        NewEvents = #{} ->
            update_if_persisted(Events, NewEvents),
            NewProbeTimers = start_probe_if_needed(EventName, Labels, Config, ProbeTimers),
            {reply, ok, #{events => NewEvents, probe_timers => NewProbeTimers}}
    end;
handle_call({tear_down, EventName, Labels}, _From,
            #{events := Events, probe_timers := ProbeTimers}) ->
    NewProbeTimers = deregister_probe_timer(EventName, Labels, ProbeTimers),
    NewEvents = deregister_event(EventName, Labels, Events),
    update_if_persisted(Events, NewEvents),
    {reply, ok, #{events => NewEvents, probe_timers => NewProbeTimers}};
handle_call({add_handler, Key, ConfigOpts}, _From, State = #{events := Events}) ->
    case mongoose_config:lookup_opt([instrumentation, Key]) of
        {error, not_found} ->
            mongoose_config:set_opt([instrumentation, Key], ConfigOpts),
            Module = handler_module(Key),
            start_handler(Module, ConfigOpts),
            NewEvents = update_handlers(Events, [], [Module]),
            update_if_persisted(Events, NewEvents),
            {reply, ok, State#{events := NewEvents}};
        {ok, ExistingConfig} ->
            {reply, {error, #{what => handler_already_configured, handler_key => Key,
                              existing_config => ExistingConfig}},
             State}
    end;
handle_call({remove_handler, Key}, _From, State = #{events := Events}) ->
    case mongoose_config:lookup_opt([instrumentation, Key]) of
        {error, not_found} ->
            {reply, {error, #{what => handler_not_configured, handler_key => Key}}, State};
        {ok, _} ->
            ConfigOpts = mongoose_config:get_opt([instrumentation, Key]),
            mongoose_config:unset_opt([instrumentation, Key]),
            Module = handler_module(Key),
            NewEvents = update_handlers(Events, [Module], []),
            update_if_persisted(Events, NewEvents),
            stop_handler(Module, ConfigOpts),
            {reply, ok, State#{events := NewEvents}}
    end;
handle_call(persist, _From, State = #{events := Events}) ->
    persistent_term:put(?MODULE, Events),
    {reply, ok, State, hibernate};
handle_call({lookup, EventName, Labels}, _From, State = #{events := Events}) ->
    {reply, lookup(EventName, Labels, Events), State};
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
    [stop_handler(handler_module(Key), Opts)
     || {Key, Opts = #{}} <- maps:to_list(mongoose_config:get_opt(instrumentation))],
    ok.

%% Internal functions

-spec update_if_persisted(event_map(), event_map()) -> ok.
update_if_persisted(Events, NewEvents) ->
    try persistent_term:get(?MODULE) of
        Events -> persistent_term:put(?MODULE, NewEvents)
    catch
        error:badarg -> ok
    end.

-spec set_up_and_register_event(event_name(), labels(), config(), event_map()) ->
          event_map() | {error, map()}.
set_up_and_register_event(EventName, Labels, Config, Events) ->
    LabelKeys = label_keys(Labels),
    case Events of
        #{EventName := #{Labels := _}} ->
            {error, #{what => event_already_registered,
                      event_name => EventName, labels => Labels}};
        #{EventName := HandlerMap} ->
            {ExistingLabels, _, _} = maps:next(maps:iterator(HandlerMap)),
            case label_keys(ExistingLabels) of
                LabelKeys ->
                    handle_metrics_config(EventName, Labels, Config, HandlerMap, Events);
                ExistingKeys ->
                    {error, #{what => inconsistent_labels,
                              event_name => EventName, labels => Labels,
                              existing_label_keys => ExistingKeys}}
            end;
        #{} ->
            handle_metrics_config(EventName, Labels, Config, new, Events)
    end.

-spec handle_metrics_config(event_name(), labels(), config(), handler_map() | new, event_map()) ->
          event_map() | {error, map()}.
handle_metrics_config(EventName, Labels, Config, HandlerMap, Events) ->
    case filter_improper_probe_metrics(Config) of
        {ok, _} ->
            Handlers = do_set_up(EventName, Labels, Config),
            case HandlerMap of
                new ->
                    Events#{EventName => #{Labels => Handlers}};
                _ ->
                    Events#{EventName := HandlerMap#{Labels => Handlers}}
            end;
        {error, FilteredMetrics} ->
            {error, #{what => non_gauge_metrics_in_probe,
                      event_name => EventName, labels => Labels,
                      improper_metrics => FilteredMetrics}}
    end.

-spec filter_improper_probe_metrics(config()) -> {ok, #{}} | {error, metrics()}.
filter_improper_probe_metrics(Config) ->
    case Config of
        #{probe := _, metrics := Metrics} ->
            FilteredMetrics = maps:filter(fun(_, V) -> V =/= gauge end, Metrics),
            case map_size(FilteredMetrics) of
                0 -> {ok, FilteredMetrics};
                _ -> {error, FilteredMetrics}
            end;
        _ ->
          {ok, #{}}
    end.

-spec do_set_up(event_name(), labels(), config()) -> handlers().
do_set_up(EventName, Labels, Config) ->
    HandlerFuns = set_up_handlers(EventName, Labels, Config, handler_modules()),
    {HandlerFuns, Config}.

-spec start_probe_if_needed(event_name(), labels(), config(), probe_timer_map()) ->
          probe_timer_map().
start_probe_if_needed(EventName, Labels, #{probe := ProbeConfig}, ProbeTimers) ->
    TRef = mongoose_instrument_probe:start_probe_timer(EventName, Labels, ProbeConfig),
    add_probe_timer(EventName, Labels, TRef, ProbeTimers);
start_probe_if_needed(_EventName, _Labels, _Config, ProbeTimers) ->
    ProbeTimers.

-spec add_probe_timer(event_name(), labels(), timer:tref(), probe_timer_map()) -> probe_timer_map().
add_probe_timer(EventName, Labels, TRef, ProbeTimers) ->
    false = maps:is_key({EventName, Labels}, ProbeTimers), % sanity check to detect timer leak
    ProbeTimers#{{EventName, Labels} => TRef}.

-spec update_handlers(event_map(), [module()], [module()]) -> event_map().
update_handlers(Events, ToRemove, ToAdd) ->
    maps:map(fun(EventName, HandlerMap) ->
                     maps:map(fun(Labels, Handlers) ->
                                      update_event_handlers(EventName, Labels, Handlers,
                                                            ToRemove, ToAdd)
                              end, HandlerMap)
             end, Events).

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

-spec deregister_event(event_name(), labels(), event_map()) -> event_map().
deregister_event(EventName, Labels, Events) ->
    case Events of
        #{EventName := HandlerMap} ->
            case maps:remove(Labels, HandlerMap) of
                Empty when Empty =:= #{} ->
                    maps:remove(EventName, Events);
                NewHandlerMap ->
                    Events#{EventName := NewHandlerMap}
            end;
        #{} ->
            Events
    end.

-spec deregister_probe_timer(event_name(), labels(), probe_timer_map()) -> probe_timer_map().
deregister_probe_timer(EventName, Labels, ProbeTimers) ->
    case maps:take({EventName, Labels}, ProbeTimers) of
        {TRef, NewProbeTimers} ->
            timer:cancel(TRef),
            NewProbeTimers;
        error ->
            ProbeTimers % no timer was registered
    end.

-spec lookup(event_name(), labels()) -> {ok, handlers()} | {error, map()}.
lookup(EventName, Labels) ->
    try persistent_term:get(?MODULE) of
        Events ->
            lookup(EventName, Labels, Events)
    catch
        %% Although persist/0 should be called before handling traffic,
        %% some instrumented events might happen before that, and they shouldn't fail.
        error:badarg ->
            ?LOG_INFO(#{what => mongoose_instrument_lookup_without_persistent_term,
                        event_name => EventName, labels => Labels}),
            gen_server:call(?MODULE, {lookup, EventName, Labels})
    end.

-spec lookup(event_name(), labels(), event_map()) -> {ok, handlers()} | {error, map()}.
lookup(EventName, Labels, Events) ->
    case Events of
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
    Keys = [Key || {Key, #{}} <- maps:to_list(mongoose_config:get_opt(instrumentation))],
    lists:map(fun handler_module/1, Keys).

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

-spec start_handler(module(), handler_module_opts()) -> ok.
start_handler(Module, Opts) ->
    case mongoose_lib:is_exported(Module, start, 1) of
        true -> Module:start(Opts);
        false -> ok
    end.

-spec stop_handler(module(), handler_module_opts()) -> ok.
stop_handler(Module, Opts) ->
    case mongoose_lib:is_exported(Module, stop, 1) of
        true -> Module:stop(Opts);
        false -> ok
    end.

-spec call_handler(handler_fun(), event_name(), labels(), config(), measurements()) -> any().
call_handler(HandlerFun, EventName, Labels, Config, Measurements) ->
    safely:apply_and_log(HandlerFun, [EventName, Labels, Config, Measurements],
                         #{what => event_handler_failed, handler_fun => HandlerFun,
                           event_name => EventName, labels => Labels, config => Config,
                           measurements => Measurements}).
