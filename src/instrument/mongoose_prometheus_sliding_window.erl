-module(mongoose_prometheus_sliding_window).

%% Manages a sliding window using DDSketch instances for quantile summaries.
%% Window parameters are configurable via WINDOW_SIZE_MS and WINDOW_STEP_MS constants.

-export([child_spec/0,
         declare/1,
         observe/3,
         values/1,
         remove/2,
         get_all_metric_names/0,
         get_metric_spec/1]).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([default_quantiles/0, default_error/0, default_bound/0, window_size_s/0]).

-ignore_xref([start_link/0]).

-behaviour(gen_server).

%% Configurable window parameters
-define(WINDOW_SIZE_MS, 60000). % Total window size: 60 seconds
-define(WINDOW_STEP_MS, 3000).  % Step size: 3 seconds per sub-window
-define(WINDOW_COUNT, (?WINDOW_SIZE_MS div ?WINDOW_STEP_MS)). % Number of sub-windows


-spec default_quantiles() -> [number()].
default_quantiles() -> [0.5, 0.75, 0.90, 0.95, 0.99, 0.999].

-spec default_error() -> number().
default_error() -> 0.01.

-spec default_bound() -> non_neg_integer().
default_bound() -> 1260.

-spec window_size_s() -> pos_integer().
window_size_s() -> ?WINDOW_SIZE_MS div 1000.

-record(state, {
    timer_ref :: reference() | undefined,
    ets_table :: ets:tab() | undefined,
    metrics = #{} :: #{name() => #{{name(), label_values()} => metric_state()}},
    metric_specs = #{} :: #{name() => proplists:proplist()}
}).

-type name() :: string().
-type label_values() :: [mongoose_instrument:label_value()].
-type window_data() :: #{sketch := ddskerl_ets:ddsketch(),
                         ref := ets:tab(),
                         name := term()}.
-type metric_state() :: #{windows => [{window_data(), non_neg_integer()}],
                          current_index => non_neg_integer()}.

%% Public API

-spec declare(proplists:proplist()) -> boolean().
declare(MetricSpec) ->
    Name = proplists:get_value(name, MetricSpec),
    gen_server:call(?MODULE, {declare, Name, MetricSpec}).

-spec observe(name(), label_values(), number()) -> ok.
observe(Name, LabelValues, Value) ->
    gen_server:cast(?MODULE, {observe, Name, LabelValues, Value}).

-spec values(name()) -> [{label_values(), {non_neg_integer(), number(), [{number(), number()}]}}].
values(Name) ->
    gen_server:call(?MODULE, {values, Name}).

-spec remove(name(), label_values()) -> boolean().
remove(Name, LabelValues) ->
    gen_server:call(?MODULE, {remove, Name, LabelValues}).

-spec get_all_metric_names() -> [name()].
get_all_metric_names() ->
    gen_server:call(?MODULE, get_all_metric_names).

-spec get_metric_spec(name()) -> proplists:proplist() | undefined.
get_metric_spec(Name) ->
    gen_server:call(?MODULE, {get_metric_spec, Name}).

%% gen_server callbacks

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    EtsTable = ets:new(?MODULE, [set, private, {read_concurrency, true}, {write_concurrency, true}]),
    TimerRef = erlang:start_timer(?WINDOW_STEP_MS, self(), rotate),
    {ok, #state{timer_ref = TimerRef, ets_table = EtsTable}}.

handle_call({declare, Name, MetricSpec}, _From, State) ->
    case maps:is_key(Name, State#state.metrics) of
        true ->
            {reply, false, State};
        false ->
            NewMetrics = maps:put(Name, #{}, State#state.metrics),
            NewSpecs = maps:put(Name, MetricSpec, State#state.metric_specs),
            {reply, true, State#state{metrics = NewMetrics, metric_specs = NewSpecs}}
    end;
handle_call({values, Name}, _From, State) ->
    {Result, NewState} = get_values(Name, State),
    {reply, Result, NewState};
handle_call({remove, Name, LabelValues}, _From, State) ->
    NewState = do_remove(Name, LabelValues, State),
    {reply, true, NewState};
handle_call(get_all_metric_names, _From, State) ->
    Names = maps:keys(State#state.metrics),
    {reply, Names, State};
handle_call({get_metric_spec, Name}, _From, State) ->
    Spec = maps:get(Name, State#state.metric_specs, undefined),
    {reply, Spec, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({observe, Name, LabelValues, Value}, State) ->
    NewState = do_observe(Name, LabelValues, Value, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _TimerRef, rotate}, State) ->
    %% The rotation is handled in ensure_windows_rotated, so we just reschedule
    TimerRef = erlang:start_timer(?WINDOW_STEP_MS, self(), rotate),
    {noreply, State#state{timer_ref = TimerRef}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Child spec for supervision tree

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, []},
      restart => permanent,
      shutdown => timer:seconds(5),
      type => worker,
      modules => [?MODULE]}.

%% Internal functions

-spec do_observe(name(), label_values(), number(), #state{}) -> #state{}.
do_observe(Name, LabelValues, Value, State) ->
    case maps:find(Name, State#state.metrics) of
        error ->
            State; % metric not declared, ignore
        {ok, MetricState} ->
            Key = {Name, LabelValues},
            CurrentTime = erlang:monotonic_time(millisecond),
            {Windows, CurrentIndex} =
                ensure_windows_rotated(Key, CurrentTime, State),

            %% Get current window and add observation
            {CurrentWindow, WindowStartTime} = lists:nth(CurrentIndex + 1, Windows),
            Sketch = maps:get(sketch, CurrentWindow),
            UpdatedSketch = ddskerl_ets:insert(Sketch, Value),
            UpdatedWindow = CurrentWindow#{sketch := UpdatedSketch},

            %% Update windows list and metric state
            UpdatedWindows = set_nth(CurrentIndex + 1, {UpdatedWindow, WindowStartTime}, Windows),
            UpdatedMetricState = maps:put(Key,
                #{windows => UpdatedWindows,
                  current_index => CurrentIndex},
                MetricState),

            UpdatedMetrics = maps:put(Name, UpdatedMetricState, State#state.metrics),
            State#state{metrics = UpdatedMetrics}
    end.

-spec ensure_windows_rotated({name(), label_values()}, non_neg_integer(), #state{}) ->
    {[{window_data(), non_neg_integer()}], non_neg_integer()}.
ensure_windows_rotated(Key, CurrentTime, State) ->
    case get_metric_state(Key, State) of
        undefined ->
            %% Initialize new windows, all starting at current time (they'll be rotated as needed)
            Windows = [new_window(Key, Index, CurrentTime, State) || Index <- lists:seq(0, ?WINDOW_COUNT - 1)],
            {Windows, 0};
        #{windows := Windows, current_index := CurrentIndex} ->
            {_CurrentWindow, WindowStartTime} = lists:nth(CurrentIndex + 1, Windows),
            Elapsed = CurrentTime - WindowStartTime,
            if
                Elapsed >= ?WINDOW_STEP_MS ->
                    NewIndex = (CurrentIndex + 1) rem ?WINDOW_COUNT,
                    {CurrentWindow, _} = lists:nth(CurrentIndex + 1, Windows),
                    ResetWindow = reset_window(CurrentWindow),
                    UpdatedWindows = set_nth(CurrentIndex + 1, {ResetWindow, CurrentTime}, Windows),
                    {UpdatedWindows, NewIndex};
                true ->
                    {Windows, CurrentIndex}
            end
    end.

-spec get_value(name(), label_values(), #state{}) ->
    {undefined | {non_neg_integer(), number(), [{number(), number()}]}, #state{}}.
get_value(Name, LabelValues, State) ->
    Key = {Name, LabelValues},
    case get_metric_state(Key, State) of
        undefined ->
            {undefined, State};
        #{} ->
            CurrentTime = erlang:monotonic_time(millisecond),
            {UpdatedWindows, NewIndex} = ensure_windows_rotated(Key, CurrentTime, State),

            %% Update metric state with rotated windows
            {ok, MetricState} = maps:find(Name, State#state.metrics),
            UpdatedMetricState = maps:put(Key,
                #{windows => UpdatedWindows,
                  current_index => NewIndex},
                MetricState),
            UpdatedMetrics = maps:put(Name, UpdatedMetricState, State#state.metrics),
            NewState = State#state{metrics = UpdatedMetrics},

            %% Filter windows to only include those within the last window size
            CutoffTime = CurrentTime - ?WINDOW_SIZE_MS,
            ActiveWindows = [Window || {Window, StartTime} <- UpdatedWindows,
                                      StartTime >= CutoffTime],

            %% Merge all active windows
            WindowTuples = [window_tuple(Window) || Window <- ActiveWindows],
            Merged = lists:foldl(fun
                                      (undefined, Acc) -> Acc;
                                      (Val, undefined) -> Val;
                                      (Val, Acc) -> ddskerl_ets:merge_tuples(Acc, Val)
                                  end, undefined, WindowTuples),

            Result = case Merged of
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
            end,
            {Result, NewState}
    end.

-spec get_values(name(), #state{}) -> {[{label_values(), {non_neg_integer(), number(), [{number(), number()}]}}], #state{}}.
get_values(Name, State) ->
    case maps:find(Name, State#state.metrics) of
        error ->
            {[], State};
        {ok, MetricState} ->
            Keys = maps:keys(MetricState),
            {ResultsRev, FinalState} = lists:foldl(
                fun({_, LabelValues}, {AccRes, S}) ->
                    {Res, S2} = get_value(Name, LabelValues, S),
                    NewAcc = case Res of
                        undefined -> AccRes;
                        _ -> [{LabelValues, Res} | AccRes]
                    end,
                    {NewAcc, S2}
                end, {[], State}, Keys),
            {lists:reverse(ResultsRev), FinalState}
    end.

-spec do_remove(name(), label_values(), #state{}) -> #state{}.
do_remove(Name, LabelValues, State) ->
    Key = {Name, LabelValues},
    case maps:find(Name, State#state.metrics) of
        error ->
            State;
        {ok, MetricState} ->
            UpdatedMetricState = remove_metric_state(Key, MetricState),
            if
                map_size(UpdatedMetricState) == 0 ->
                    UpdatedMetrics = maps:remove(Name, State#state.metrics),
                    State#state{metrics = UpdatedMetrics};
                true ->
                    UpdatedMetrics = maps:put(Name, UpdatedMetricState, State#state.metrics),
                    State#state{metrics = UpdatedMetrics}
            end
    end.

-spec get_metric_state({name(), label_values()}, #state{}) -> undefined | metric_state().
get_metric_state({Name, LabelValues}, State) ->
    case State#state.metrics of
       #{Name := MetricState} ->
           maps:get({Name, LabelValues}, MetricState, undefined);
       #{} ->
           undefined
   end.

-spec set_nth(pos_integer(), T, [T]) -> [T].
set_nth(1, Value, [_ | Rest]) ->
    [Value | Rest];
set_nth(N, Value, [H | Rest]) when N > 1 ->
    [H | set_nth(N - 1, Value, Rest)].

%% Window helpers

-spec metric_options(name(), #state{}) -> {number(), non_neg_integer()}.
metric_options(Name, State) ->
    MetricSpec = maps:get(Name, State#state.metric_specs, []),
    Error = proplists:get_value(error, MetricSpec, default_error()),
    Bound = proplists:get_value(bound, MetricSpec, default_bound()),
    {Error, Bound}.

-spec window_name({name(), label_values()}, non_neg_integer()) -> term().
window_name({Name, LabelValues}, Index) ->
    {Name, LabelValues, Index}.

-spec new_window({name(), label_values()}, non_neg_integer(), non_neg_integer(), #state{}) ->
    {window_data(), non_neg_integer()}.
new_window(Key = {Name, _LabelValues}, Index, StartTime, State) ->
    {Error, Bound} = metric_options(Name, State),
    WindowName = window_name(Key, Index),
    Sketch = ddskerl_ets:new(#{ets_table => State#state.ets_table,
                               name => WindowName,
                               error => Error,
                               bound => Bound}),
    WindowData = #{sketch => Sketch,
                   ref => State#state.ets_table,
                   name => WindowName},
    {WindowData, StartTime}.

-spec reset_window(window_data()) -> window_data().
reset_window(WindowData) ->
    Sketch = maps:get(sketch, WindowData),
    ResetSketch = ddskerl_ets:reset(Sketch),
    WindowData#{sketch := ResetSketch}.

-spec window_tuple(window_data()) -> ddskerl_ets:object() | undefined.
window_tuple(WindowData) ->
    Ref = maps:get(ref, WindowData),
    Name = maps:get(name, WindowData),
    case ets:lookup(Ref, Name) of
        [Val] -> Val;
        [] -> undefined
    end.

-spec remove_metric_state({name(), label_values()}, #{}) -> #{}.
remove_metric_state(Key, MetricState) ->
    UpdatedMetricState =
        case maps:find(Key, MetricState) of
            error ->
                MetricState;
            {ok, #{windows := Windows}} ->
                lists:foreach(fun({Window, _}) -> delete_window(Window) end, Windows),
                maps:remove(Key, MetricState)
        end,
    UpdatedMetricState.

-spec delete_window(window_data()) -> ok.
delete_window(WindowData) ->
    Ref = maps:get(ref, WindowData),
    Name = maps:get(name, WindowData),
    ets:delete(Ref, Name),
    ok.
