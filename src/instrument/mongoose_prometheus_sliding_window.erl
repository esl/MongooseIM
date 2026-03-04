-module(mongoose_prometheus_sliding_window).

%% Manages a sliding window using DDSketch instances for quantile summaries.
%% Window parameters are configurable via window_size_ms and window_step_ms constants.

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%% API
-export([child_spec/0,
         start_link/0]).

-export([declare/1,
         observe/3,
         value/2,
         values/1,
         remove/2,
         get_all_metric_names/0,
         get_metric_spec/1]).

-define(METRICS_TABLE, mongoose_prometheus_sliding_window_metrics).
-define(DDSKERL_TABLE, mongoose_prometheus_sliding_window_ddskerl).

-record(state, {
    timer_ref :: timer:tref()
}).

-record(metric, {
    name :: name(),
    states = #{} :: #{label_values() => metric_state()},
    spec = proplists:proplist()
}).

-type name() :: string().
-type label_values() :: [mongoose_instrument:label_value()].
-type window_data() :: #{sketch := ddskerl_ets:ddsketch(),
                         name := term()}.
-type metric_state() :: #{windows => [window_data()],
                          current_index => non_neg_integer()}.

%% Window parameters

-spec window_size_ms() -> pos_integer().
window_size_ms() -> 60000. % Total window size: 60 seconds

-spec window_step_ms() -> pos_integer().
window_step_ms() -> 3000.  % Step size: 3 seconds per sub-window

-spec window_count() -> pos_integer().
window_count() -> window_size_ms() div window_step_ms().

%% DDSketch parameters

-spec default_quantiles() -> [number()].
default_quantiles() -> [0.5, 0.75, 0.90, 0.95, 0.99, 0.999].

-spec default_error() -> number().
default_error() -> 0.01.

%% Setting the default bound to 1260 means that:
%% * measuring in µs suffices for actions lasting up to a day (with 1% accuracy),
%% * measuring in bytes suffices for sizes up to 81 GB (with 1% accuracy).
%% See: https://hexdocs.pm/ddskerl/ddskerl_ets.html
-spec default_bound() -> non_neg_integer().
default_bound() -> 1260.

%% gen_server

init(_Args) ->
    EtsOpts = [named_table, set, public, {read_concurrency, true}, {write_concurrency, true}],
    ets:new(?METRICS_TABLE, [{keypos, #metric.name} | EtsOpts]),
    ets:new(?DDSKERL_TABLE, EtsOpts),
    {ok, TimerRef} = timer:send_interval(window_step_ms(), rotate),
    {ok, #state{timer_ref = TimerRef}}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(rotate, State) ->
    rotate_all_windows(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer_ref = TimerRef}) ->
    timer:cancel(TimerRef).

%% API

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, []},
      restart => permanent,
      shutdown => timer:seconds(5),
      type => worker,
      modules => [?MODULE]}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec declare(proplists:proplist()) -> boolean().
declare(Spec) ->
    Name = proplists:get_value(name, Spec),
    ets:insert_new(?METRICS_TABLE, #metric{name = Name, spec = Spec}).

-spec observe(name(), label_values(), number()) -> ok.
observe(Name, LabelValues, Value) ->
    case ets:lookup(?METRICS_TABLE, Name) of
        [] ->
            % TODO: log error
            ok;
        [Metric] ->
            {Windows, CurrentIndex} = ensure_windows_initialized(LabelValues, Metric),

            %% Get current window and add observation
            CurrentWindow = lists:nth(CurrentIndex + 1, Windows),
            Sketch = maps:get(sketch, CurrentWindow),
            UpdatedSketch = ddskerl_ets:insert(Sketch, Value),
            UpdatedWindow = CurrentWindow#{sketch := UpdatedSketch},

            %% Update windows list and metric state
            UpdatedWindows = set_nth(CurrentIndex + 1, UpdatedWindow, Windows),
            UpdatedMetricState = maps:put(LabelValues,
                                          #{windows => UpdatedWindows, current_index => CurrentIndex},
                                          Metric#metric.states),

            ets:insert(?METRICS_TABLE, Metric#metric{states = UpdatedMetricState}),
            ok
    end.

-spec value(name(), label_values()) -> undefined | {non_neg_integer(), number(), [{number(), number()}]}.
value(Name, LabelValues) ->
    case get_metric_state(Name, LabelValues) of
        undefined -> undefined;
        State -> get_state_value(State)
    end.

-spec values(name()) -> [{label_values(), {non_neg_integer(), number(), [{number(), number()}]}}].
values(Name) ->
    case ets:lookup(?METRICS_TABLE, Name) of
        [] ->
            [];
        [#metric{states = States}] ->
            [{LabelValues, get_state_value(State)} || LabelValues := State <- States]
    end.

-spec remove(name(), label_values()) -> boolean().
remove(Name, LabelValues) ->
    case ets:lookup(?METRICS_TABLE, Name) of
        [] ->
            true;
        [#metric{states = States} = Metric] ->
            States1 = remove_state(LabelValues, States),
            ets:insert(?METRICS_TABLE, Metric#metric{states = States1})
    end.
    
-spec get_all_metric_names() -> [name()].
get_all_metric_names() ->
    ets:foldl(fun(#metric{name = Name}, Acc) -> [Name | Acc] end,
             [],
             ?METRICS_TABLE).

-spec get_metric_spec(name()) -> proplists:proplist() | undefined.
get_metric_spec(Name) ->
    case ets:lookup(?METRICS_TABLE, Name) of
        [] -> undefined;
        [#metric{spec = Spec}] -> Spec
    end.

%% Private

rotate_all_windows() ->
    WindowCount = window_count(),
    ets:foldl(
        fun(#metric{states = States} = Metric, _Acc) ->
            States1 = maps:map(rotate_windows(WindowCount), States),
            ets:insert(?METRICS_TABLE, Metric#metric{states = States1})
        end,
        ok,
        ?METRICS_TABLE
    ).

rotate_windows(WindowCount) ->
    fun(_, #{windows := Windows, current_index := CurrentIndex}) ->
        NewIndex = (CurrentIndex + 1) rem WindowCount,
        WindowToReset = lists:nth(NewIndex + 1, Windows),
        ResetWindow = reset_window(WindowToReset),
        UpdatedWindows = set_nth(NewIndex + 1, ResetWindow, Windows),
        #{windows => UpdatedWindows, current_index => NewIndex}
    end.

reset_window(#{sketch := Sketch} = Window) ->
    ResetSketch = ddskerl_ets:reset(Sketch),
    Window#{sketch := ResetSketch}.

ensure_windows_initialized(LabelValues, #metric{states = States} = Metric) ->
    case maps:get(LabelValues, States, undefined) of
        undefined ->
            Windows = [new_window(LabelValues, Metric, Index) || Index <- lists:seq(0, window_count() - 1)],
            {Windows, 0};
        #{windows := Windows, current_index := CurrentIndex} ->
            {Windows, CurrentIndex}
    end.

new_window(LabelValues, #metric{name = Name, spec = Spec}, Index) ->
    WindowName = {Name, LabelValues, Index},
    {Error, Bound} = metric_options(Spec),
    Sketch = ddskerl_ets:new(#{ets_table => ?DDSKERL_TABLE,
                               name => WindowName,
                               error => Error,
                               bound => Bound}),
    #{sketch => Sketch,
      name => WindowName}.

metric_options(Spec) ->
    Error = proplists:get_value(error, Spec, default_error()),
    Bound = proplists:get_value(bound, Spec, default_bound()),
    {Error, Bound}.

set_nth(1, Value, [_ | Rest]) ->
    [Value | Rest];
set_nth(N, Value, [H | Rest]) when N > 1 ->
    [H | set_nth(N - 1, Value, Rest)].

get_metric_state(Name, LabelValues) ->
    case ets:lookup(?METRICS_TABLE, Name) of
        [] ->
            undefined;
        [#metric{states = States}] ->
            maps:get(LabelValues, States, undefined)
    end.

get_state_value(#{windows := Windows}) ->
    WindowTuples = [window_tuple(Window) || Window <- Windows],
    Merged = lists:foldl(fun(undefined, Acc) -> Acc;
                            (Val, undefined) -> Val;
                            (Val, Acc) -> ddskerl_ets:merge_tuples(Acc, Val)
                         end, undefined, WindowTuples),
    case Merged of
        undefined ->
            undefined;
        _ ->
            Count = ddskerl_ets:total_tuple(Merged),
            case Count of
                0 ->
                    undefined;
                _ ->
                    Sum = ddskerl_ets:sum_tuple(Merged),
                    Quantiles = [{Q, ddskerl_ets:quantile_tuple(Merged, Q)} || Q <- default_quantiles()],
                    {Count, Sum, Quantiles}
            end
    end.

-spec window_tuple(window_data()) -> ddskerl_ets:object() | undefined.
window_tuple(#{name := Name}) ->
    case ets:lookup(?DDSKERL_TABLE, Name) of
        [Val] -> Val;
        [] -> undefined
    end.

remove_state(LabelValues, States) ->
    case maps:take(LabelValues, States) of
        {#{windows := Windows}, States1} ->
            [ets:delete(Ref, Name) || #{ref := Ref, name := Name} <- Windows],
            States1;
        error ->
            States
    end.
