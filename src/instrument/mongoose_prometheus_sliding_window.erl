-module(mongoose_prometheus_sliding_window).

%% Manages a sliding window of 20 DDSketch instances (3 seconds each = 60 seconds total)
%% Each window uses ddskerl to store quantile summaries

-export([declare/1,
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

-export([default_quantiles/0, default_error/0]).

-behaviour(gen_server).

-define(WINDOW_COUNT, 20).
-define(WINDOW_DURATION_MS, 3000). % 3 seconds per window
-define(TOTAL_WINDOW_MS, (?WINDOW_COUNT * ?WINDOW_DURATION_MS)). % 60 seconds total

-spec default_quantiles() -> [number()].
default_quantiles() -> [0.5, 0.75, 0.90, 0.95, 0.99, 0.999].

-spec default_error() -> number().
default_error() -> 0.01.

-record(state, {
    timer_ref :: reference() | undefined,
    metrics = #{} :: #{name() => #{label_values() => metric_state()}},
    metric_specs = #{} :: #{name() => proplists:proplist()}
}).

-type name() :: string().
-type label_values() :: [mongoose_instrument:label_value()].
-type metric_state() :: #{windows => [{ddskerl_ets:ddsketch(), non_neg_integer()}],
                          current_index => non_neg_integer()}.

%% Public API

-spec declare(proplists:proplist()) -> boolean().
declare(MetricSpec) ->
    ok = ensure_started(),
    Name = proplists:get_value(name, MetricSpec),
    gen_server:call(?MODULE, {declare, Name, MetricSpec}).

-spec observe(name(), label_values(), number()) -> ok.
observe(Name, LabelValues, Value) ->
    ok = ensure_started(),
    gen_server:cast(?MODULE, {observe, Name, LabelValues, Value}).

-spec values(name()) -> [{label_values(), {non_neg_integer(), number(), [{number(), number()}]}}].
values(Name) ->
    ok = ensure_started(),
    gen_server:call(?MODULE, {values, Name}).

-spec remove(name(), label_values()) -> boolean().
remove(Name, LabelValues) ->
    ok = ensure_started(),
    gen_server:call(?MODULE, {remove, Name, LabelValues}).

-spec get_all_metric_names() -> [name()].
get_all_metric_names() ->
    ok = ensure_started(),
    gen_server:call(?MODULE, get_all_metric_names).

-spec get_metric_spec(name()) -> proplists:proplist() | undefined.
get_metric_spec(Name) ->
    ok = ensure_started(),
    gen_server:call(?MODULE, {get_metric_spec, Name}).

%% gen_server callbacks

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    TimerRef = erlang:start_timer(?WINDOW_DURATION_MS, self(), rotate),
    {ok, #state{timer_ref = TimerRef}}.

handle_call({declare, Name, MetricSpec}, _From, State) ->
    case maps:is_key(Name, State#state.metrics) of
        true ->
            {reply, false, State};
        false ->
            NewMetrics = maps:put(Name, #{}, State#state.metrics),
            NewSpecs = maps:put(Name, MetricSpec, State#state.metric_specs),
            {reply, true, State#state{metrics = NewMetrics, metric_specs = NewSpecs}}
    end;

handle_call({observe, Name, LabelValues, Value}, _From, State) ->
    NewState = do_observe(Name, LabelValues, Value, State),
    {reply, ok, NewState};

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
    TimerRef = erlang:start_timer(?WINDOW_DURATION_MS, self(), rotate),
    {noreply, State#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal helpers

-spec ensure_started() -> ok.
ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            case start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok;
                {error, Reason} -> exit(Reason)
            end;
        _Pid ->
            ok
    end.

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
            UpdatedWindow = ddskerl_std:insert(CurrentWindow, Value),

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
    {[{ddskerl_std:ddsketch(), non_neg_integer()}], non_neg_integer()}.
ensure_windows_rotated(Key, CurrentTime, State) ->
    case get_metric_state(Key, State) of
        undefined ->
            %% Initialize new windows, all starting at current time (they'll be rotated as needed)
            Windows = [{ddskerl_std:new(#{error => default_error()}), CurrentTime} || _ <- lists:seq(1, ?WINDOW_COUNT)],
            {Windows, 0};
        #{windows := Windows, current_index := CurrentIndex} ->
            {_CurrentWindow, WindowStartTime} = lists:nth(CurrentIndex + 1, Windows),
            Elapsed = CurrentTime - WindowStartTime,
            if
                Elapsed >= ?WINDOW_DURATION_MS ->
                    NewIndex = (CurrentIndex + 1) rem ?WINDOW_COUNT,
                    NewWindow = {ddskerl_std:new(#{error => default_error()}), CurrentTime},
                    UpdatedWindows = set_nth(CurrentIndex + 1, NewWindow, Windows),
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

            %% Filter windows to only include those within the last 60 seconds
            CutoffTime = CurrentTime - ?TOTAL_WINDOW_MS,
            ActiveWindows = [Window || {Window, StartTime} <- UpdatedWindows,
                                      StartTime >= CutoffTime],

            %% Merge all active windows
            Merged = lists:foldl(fun(Window, Acc) ->
                                    case Acc of
                                        undefined -> Window;
                                        _ -> ddskerl_std:merge(Acc, Window)
                                    end
                                end, undefined, ActiveWindows),

            Result = case Merged of
                undefined ->
                    undefined;
                _ ->
                    Count = ddskerl_std:total(Merged),
                    Sum = ddskerl_std:sum(Merged),
                    Quantiles = [{Q, ddskerl_std:quantile(Merged, Q)} || Q <- default_quantiles()],
                    {Count, Sum, Quantiles}
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
            UpdatedMetricState = maps:remove(Key, MetricState),
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
