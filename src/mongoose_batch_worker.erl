-module(mongoose_batch_worker).

-behaviour(gen_server).

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
          batch_size :: non_neg_integer(),
          flush_interval :: non_neg_integer(), %% milliseconds
          flush_interval_tref :: undefined | reference(),
          flush_callback = fun(_, _) -> ok end :: flush_callback(),
          flush_queue = [] :: list() | censored, % see format_status/2 for censored
          flush_extra = #{} :: map()
         }).
-type state() :: #state{}.
-type flush_callback() :: fun((list(), map()) -> ok | {error, term()}).

-export_type([flush_callback/0]).

%% gen_server callbacks
-spec init({mongooseim:host_type(),
            pos_integer(),
            pos_integer(),
            flush_callback(),
            map()}) -> {ok, state()}.
init({HostType, Interval, MaxSize, FlushCallback, FlushExtra}) ->
    ?LOG_DEBUG(#{what => batch_worker_start, host_type => HostType, pool_id => FlushCallback}),
    {ok, #state{host_type = HostType,
                batch_size = MaxSize,
                flush_interval = Interval,
                flush_callback = FlushCallback,
                flush_extra = FlushExtra}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call(Msg, From, State) ->
    ?UNEXPECTED_CALL(Msg, From),
    {reply, unexpected_call, State}.

-spec handle_cast({task, term()} | term(), state()) -> {noreply, state()}.
handle_cast({task, Task}, State) ->
    {noreply, handle_task(Task, State)};
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

-spec handle_info(flush | term(), state()) -> {noreply, state()}.
handle_info(flush, State) ->
    {noreply, run_flush(State)};
handle_info({garbage_collect, asynchronous_gc_triggered, true}, State) ->
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
            ?LOG_WARNING(log_fields(State, #{what => batch_worker_terminate_requires_flush})),
            do_run_flush(State)
    end.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Don't leak the tasks to logs, can contain private information
format_status(_Opt, [_PDict, State | _]) ->
    [{data, [{"State", State#state{flush_queue = censored}},
             {"Task Queue Length", length(State#state.flush_queue)}]}].

%% Batched tasks callbacks
handle_task(Task, State = #state{batch_size = MaxSize,
                                 flush_interval = Interval,
                                 flush_interval_tref = TRef0,
                                 flush_queue = Acc0}) ->
    Length = length(Acc0),
    TRef1 = maybe_schedule_flush(TRef0, Length, Interval),
    State1 = State#state{flush_interval_tref = TRef1,
                         flush_queue = [Task | Acc0]},
    case Length + 1 >= MaxSize of
        false -> State1;
        true -> run_flush(State1)
    end.

maybe_schedule_flush(undefined, 0, Interval) ->
    erlang:send_after(Interval, self(), flush);
maybe_schedule_flush(TRef, _, _) ->
    TRef.

run_flush(State = #state{flush_interval_tref = TRef}) ->
    cancel_and_flush_timer(TRef),
    ?LOG_DEBUG(log_fields(State, #{what => batch_worker_flush})),
    NewState = do_run_flush(State#state{flush_interval_tref = undefined}),
    erlang:garbage_collect(self(), [{async, asynchronous_gc_triggered}, {type, major}]),
    NewState.

cancel_and_flush_timer(undefined) ->
    ok;
cancel_and_flush_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
        false ->
            receive
                flush -> ok
            after 0 -> ok
            end;
        _ -> ok
    end.

do_run_flush(State = #state{flush_callback = FlushCallback,
                            flush_queue = Queue,
                            flush_extra = Extra}) ->
    case FlushCallback(lists:reverse(Queue), Extra) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_ERROR(log_fields(State,
                       #{what => batch_worker_flush_queue_failed, reason => Reason,
                         text => <<"flush_callback failed">>})),
            ok
    end,
    State#state{flush_queue = []}.

log_fields(State, LogMessage) ->
    LogMessage#{host_type => State#state.host_type,
                flush_queue_len => length(State#state.flush_queue),
                flush_callback => State#state.flush_callback}.
