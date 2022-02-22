%% @doc
%% This module encapsulates an asynchronous aggregator, that takes tasks classified by key,
%% to immediately preprocess and flush, but upon flushing, it submits asynchronous requests
%% and continues taking tasks and preprocessing them –aggregating them–, until the first
%% request is fulfilled, at which time it puts the next on flush.
%%
%% `request_callback' is a function that takes the new task and the extra metadata, and
%% essentially encapsulates `gen_server:send_request'. The returned reference is what will
%% be used in `handle_info' to match pending requests, so it is important that the return
%% value of `request_callback' is that same of the `gen_server:send_request'.
%% I have chosen to provide an arbitrary function callback so that it can encapsulate all
%% the logic that chooses the gen_server to request to, and possibly calculates other data
%% in the meantime.
%% @end
-module(mongoose_aggregator_worker).

-behaviour(gen_server).

-callback aggregate(mongoose_async_pools:task(),
                    mongoose_async_pools:task(),
                    mongoose_async_pools:pool_extra()) ->
    {ok, mongoose_async_pools:task()} | {error, term()}.
-callback request(mongoose_async_pools:task(), mongoose_async_pools:pool_extra()) ->
    reference().
-callback verify(term(), mongoose_async_pools:task(), mongoose_async_pools:pool_extra()) ->
    term().
-optional_callbacks([verify/3]).

-include("mongoose_logger.hrl").

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2]).

-type request() :: no_request_pending | {reference(), mongoose_async_pools:task()}.
-record(state, {
          host_type :: mongooseim:host_type(),
          pool_id :: mongoose_async_pools:pool_id(),
          async_request = no_request_pending :: request(),
          request_callback :: mongoose_async_pools:request_callback(),
          aggregate_callback :: mongoose_async_pools:aggregate_callback(),
          verify_callback :: undefined | mongoose_async_pools:verify_callback(),
          flush_elems = #{} :: map() | censored, % see format_status/2 for censored
          flush_queue = queue:new() :: queue:queue(),
          flush_extra = #{} :: map()
         }).
-type state() :: #state{}.

%% gen_server callbacks
-spec init(map()) -> {ok, state()}.
init(#{host_type := HostType,
       pool_id := PoolId,
       request_callback := Requester,
       aggregate_callback := Aggregator,
       flush_extra := FlushExtra} = Opts)
  when is_function(Requester, 2),
       is_function(Aggregator, 3),
       is_map(FlushExtra) ->
    ?LOG_DEBUG(#{what => aggregator_worker_start, host_type => HostType, pool_id => PoolId}),
    {ok, #state{host_type = HostType,
                pool_id = PoolId,
                request_callback = Requester,
                aggregate_callback = Aggregator,
                verify_callback = maps:get(verify_callback, Opts, undefined),
                flush_extra = FlushExtra}}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(sync, _From, State = #state{flush_elems = Elems}) ->
    case maps:size(Elems) of
        0 -> {reply, skipped, State};
        _ -> {reply, ok, run_flush(State)}
    end;
handle_call(Msg, From, State) ->
    ?UNEXPECTED_CALL(Msg, From),
    {reply, unexpected_call, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({task, Key, Value}, State) ->
    {noreply, handle_task(Key, Value, State)};
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Msg, #state{async_request = no_request_pending} = State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State};
handle_info(Msg, #state{async_request = {AsyncRequest, ReqTask}} = State) ->
    case gen_server:check_response(Msg, AsyncRequest) of
        {error, {Reason, _Ref}} ->
            ?LOG_ERROR(log_fields(State, #{what => asynchronous_request_failed, reason => Reason})),
            {noreply, State};
        {reply, {error, Reason}} ->
            ?LOG_ERROR(log_fields(State, #{what => asynchronous_request_failed, reason => Reason})),
            {noreply, State};
        {reply, Reply} ->
            maybe_verify_reply(Reply, ReqTask, State),
            {noreply, maybe_request_next(State)};
        no_reply ->
            ?UNEXPECTED_INFO(Msg),
            {noreply, State}
    end.

-spec terminate(term(), state()) -> term().
terminate(Reason, State) ->
    ?LOG_INFO(log_fields(State, #{what => aggregate_worker_stopping, reason => Reason})),
    case maps:size(State#state.flush_elems) of
        0 -> ok;
        _ ->
            ?LOG_WARNING(log_fields(State, #{what => aggregate_worker_terminate_requires_flush})),
            run_flush(State)
    end.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Don't leak the tasks to logs, can contain private information
format_status(_Opt, [_PDict, State | _]) ->
    [{data, [{"State", State#state{flush_elems = censored}}]}].

% If we don't have any request pending, it means that it is the first task submitted,
% so aggregation is not needed.
handle_task(_, Value, #state{async_request = no_request_pending} = State) ->
    State#state{async_request = make_async_request(Value, State)};
handle_task(Key, NewValue, #state{aggregate_callback = Aggregator,
                                  flush_elems = Acc,
                                  flush_queue = Queue,
                                  flush_extra = Extra} = State) ->
    case Acc of
        #{Key := OldValue} ->
            case Aggregator(OldValue, NewValue, Extra) of
                {ok, FinalValue} ->
                    State#state{flush_elems = Acc#{Key := FinalValue}};
                {error, Reason} ->
                    ?LOG_ERROR(log_fields(State, #{what => aggregation_failed, reason => Reason})),
                    State
            end;
        _ ->
            % The queue is used to ensure the order in which elements are flushed,
            % so that first requests are first flushed.
            State#state{flush_elems = Acc#{Key => NewValue},
                        flush_queue = queue:in(Key, Queue)}
    end.

maybe_request_next(#state{flush_elems = Acc, flush_queue = Queue} = State) ->
    case queue:out(Queue) of
        {{value, Key}, NewQueue} ->
            {Value, NewAcc} = maps:take(Key, Acc),
            State#state{async_request = make_async_request(Value, State),
                        flush_elems = NewAcc, flush_queue = NewQueue};
        {empty, _} ->
            State#state{async_request = no_request_pending}
    end.

make_async_request(Value, #state{host_type = HostType, pool_id = PoolId,
                                 request_callback = Requestor, flush_extra = Extra}) ->
    mongoose_metrics:update(HostType, [mongoose_async_pools, PoolId, async_request], 1),
    {Requestor(Value, Extra), Value}.

maybe_verify_reply(_, _, #state{verify_callback = undefined}) ->
    ok;
maybe_verify_reply(Reply, ReqTask, #state{verify_callback = Verifier, flush_extra = Extra}) ->
    Verifier(Reply, ReqTask, Extra).

run_flush(State) ->
    case maybe_request_next(State) of
        State1 = #state{async_request = no_request_pending} ->
            State1;
        State2 ->
            run_flush(State2)
    end.

log_fields(State, LogMessage) ->
    LogMessage#{host_type => State#state.host_type, pool_id => State#state.pool_id}.
