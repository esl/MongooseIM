%% @doc
%% This module encapsulates an asynchronous buffer, that accumulates tasks until
%% either the buffer is full, or a timeout since the first task expires, and then
%% flushes the buffer. It takes the buffer size, the timeout interval, and a
%% callback for flushing, as its main parameters. It can also take callbacks for
%% initialising data structures, or for preparing them before accumulation.
%% @end
-module(mongoose_batch_worker).

-behaviour(gen_server).

-callback prepare(mongoose_async_pools:task(), mongoose_async_pools:pool_extra()) ->
    {ok, mongoose_async_pools:task()} | {error, term()}.
-callback flush([mongoose_async_pools:task()], mongoose_async_pools:pool_extra()) ->
    ok | {error, term()}.
-optional_callbacks([prepare/2]).

-include("mongoose_logger.hrl").

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2]).

-record(state, {
          host_type :: mongooseim:host_type(),
          pool_id :: mongoose_async_pools:pool_id(),
          batch_size :: non_neg_integer(),
          flush_interval :: non_neg_integer(), %% milliseconds
          flush_interval_tref :: undefined | reference(),
          flush_callback = fun(_, _) -> ok end :: mongoose_async_pools:flush_callback(),
          prep_callback :: undefined | mongoose_async_pools:prep_callback(),
          flush_queue = [] :: list() | censored, % see format_status/2 for censored
          flush_queue_length = 0 :: non_neg_integer(),
          flush_extra = #{} :: map()
         }).
-type state() :: #state{}.

%% gen_server callbacks
-spec init(map()) -> {ok, state()}.
init(#{host_type := HostType,
       pool_id := PoolId,
       batch_size := MaxSize,
       flush_interval := Interval,
       flush_callback := FlushCallback,
       flush_extra := FlushExtra} = Opts)
  when is_function(FlushCallback, 2), is_map(FlushExtra) ->
    ?LOG_DEBUG(#{what => batch_worker_start, host_type => HostType, pool_id => PoolId}),
    {ok, #state{host_type = HostType,
                pool_id = PoolId,
                batch_size = MaxSize,
                flush_interval = Interval,
                flush_callback = FlushCallback,
                prep_callback = maps:get(prep_callback, Opts, undefined),
                flush_extra = FlushExtra}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call(sync, _From, State = #state{host_type = HostType, pool_id = PoolId,
                                        flush_queue = [_|_]}) ->
    mongoose_metrics:update(HostType, [mongoose_async_pools, PoolId, timed_flushes], 1),
    {reply, ok, run_flush(State)};
handle_call(sync, _From, State = #state{flush_queue = []}) ->
    {reply, skipped, State};
handle_call(Msg, From, State) ->
    ?UNEXPECTED_CALL(Msg, From),
    {reply, unexpected_call, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({task, Task}, State) ->
    {noreply, handle_task(Task, State)};
handle_cast({task, _Key, Task}, State) ->
    {noreply, handle_task(Task, State)};
handle_cast({broadcast, Broadcast}, State) ->
    {noreply, handle_task(Broadcast, State)};
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({timeout, TimerRef, flush}, State = #state{flush_interval_tref = TimerRef,
                                                       host_type = HostType,
                                                       pool_id = PoolId}) ->
    mongoose_metrics:update(HostType, [mongoose_async_pools, PoolId, timed_flushes], 1),
    {noreply, run_flush(State)};
handle_info({garbage_collect, asynchronous_gc_triggered, true}, State) ->
    {noreply, State};
handle_info({timeout, _, flush}, State) -> % expired timeout, ignore
    {noreply, State};
handle_info({cancel_timer, _, _}, State) -> % timer canceled, ignore
    {noreply, State};
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State}.

-spec terminate(term(), state()) -> term().
terminate(Reason, State) ->
    ?LOG_INFO(log_fields(State, #{what => batch_worker_stopping, reason => Reason})),
    case State#state.flush_queue of
        [] -> ok;
        _ ->
            ?LOG_WARNING(log_fields(State, #{what => batch_worker_terminate_requires_flush,
                                             reason => Reason})),
            do_run_flush(State)
    end.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Don't leak the tasks to logs, can contain private information
format_status(_Opt, [_PDict, State | _]) ->
    [{data, [{"State", State#state{flush_queue = censored}}]}].

%% Batched tasks callbacks
handle_task(Task, State) ->
    State1 = maybe_schedule_flush(State),
    State2 = maybe_prep_task(State1, Task),
    maybe_run_flush(State2).

maybe_schedule_flush(#state{flush_interval_tref = undefined,
                            flush_queue_length = 0,
                            flush_interval = Interval} = State) ->
    State#state{flush_interval_tref = erlang:start_timer(Interval, self(), flush)};
maybe_schedule_flush(State) ->
    State.

maybe_prep_task(#state{prep_callback = undefined,
                       flush_queue = Acc,
                       flush_queue_length = Length} = State, Task) ->
    State#state{flush_queue = [Task | Acc],
                flush_queue_length = Length + 1};
maybe_prep_task(#state{prep_callback = PrepCallback,
                       flush_queue = Acc,
                       flush_queue_length = Length,
                       flush_extra = Extra} = State, Task) ->
    case PrepCallback(Task, Extra) of
        {ok, ProcessedTask} ->
            State#state{flush_queue = [ProcessedTask | Acc],
                        flush_queue_length = Length + 1};
        {error, Reason} ->
            ?LOG_ERROR(log_fields(State, #{what => preprocess_callback_failed, reason => Reason})),
            State
    end.

maybe_run_flush(#state{host_type = HostType,
                       pool_id = PoolId,
                       batch_size = MaxSize,
                       flush_queue_length = Length} = State) ->
    case Length >= MaxSize of
        false -> State;
        true ->
            mongoose_metrics:update(HostType, [mongoose_async_pools, PoolId, batch_flushes], 1),
            run_flush(State)
    end.

run_flush(State = #state{flush_interval_tref = TRef}) ->
    cancel_and_flush_timer(TRef),
    ?LOG_DEBUG(log_fields(State, #{what => batch_worker_flush})),
    NewState = do_run_flush(State#state{flush_interval_tref = undefined}),
    erlang:garbage_collect(self(), [{async, asynchronous_gc_triggered}, {type, major}]),
    NewState.

cancel_and_flush_timer(undefined) ->
    ok;
cancel_and_flush_timer(TRef) ->
    catch erlang:cancel_timer(TRef, [{async, true}]).

do_run_flush(State = #state{flush_callback = FlushCallback,
                            flush_queue_length = Length,
                            flush_queue = Queue,
                            flush_extra = Extra}) ->
    case FlushCallback(lists:reverse(Queue), Extra#{queue_length := Length}) of
        ok ->
            State#state{flush_queue = [], flush_queue_length = 0};
        {error, Reason} ->
            ?LOG_ERROR(log_fields(State,
                       #{what => batch_worker_flush_queue_failed, reason => Reason,
                         text => <<"flush_callback failed">>})),
            State#state{flush_queue = [], flush_queue_length = 0}
    end.

log_fields(State, LogMessage) ->
    LogMessage#{host_type => State#state.host_type, pool_id => State#state.pool_id,
                flush_queue_length => State#state.flush_queue_length}.
