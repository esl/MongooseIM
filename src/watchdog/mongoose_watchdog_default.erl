%%%-------------------------------------------------------------------
%%% @author DenysGonchar
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2018 16:08
%%%-------------------------------------------------------------------
-module(mongoose_watchdog_default).
-author("DenysGonchar").

-behaviour(gen_event).


%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         terminate/2,
         code_change/3]).

%% API
-export([add_handler/2,
         verify/0]).

-define(DEFAULT_FN, fun default_fn/2).
-define(HIGH_MEM_PROCS, [?MODULE, high_mem_procs_no]).

-record(state, { memory_threshold = 0,
                 verify_timeout = 0, %seconds
                 timer_ref = undefined,
                 fn = ?DEFAULT_FN,
                 fn_state = []}).

%%%===================================================================
%%% API
%%%===================================================================
-spec add_handler(Handler :: atom() | {atom(), term()},
                  Args :: term()) ->
                     term().
add_handler(Fn, Args) when is_function(Fn, 2) ->
    mongoose_watchdog:call(?MODULE, {set_fn, Fn, Args}).

verify() ->
    mongoose_watchdog:call(?MODULE,verify).


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(InitArgs :: term()) ->
    {ok, State :: #state{}} |
    {ok, State :: #state{}, hibernate} |
    {error, Reason :: term()}.
init(Args) ->
    Timeout = proplists:get_value(timeout, Args, 10),
    add_metric(?HIGH_MEM_PROCS, histogram),
    spawn(fun process_current_alarms/0),
    {ok, #state{verify_timeout = Timeout}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), State :: #state{}) ->
    {ok, NewState :: #state{}} |
    {ok, NewState :: #state{}, hibernate} |
    {swap_handler, Args1 :: term(), NewState :: #state{},
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler).
handle_event({set_alarm, {process_memory_high_watermark, _}}, State) ->
    NewState = set_memory_threshold(State),
    {ok, process_memory_watermark_alarm(NewState)};
handle_event({clear_alarm, process_memory_high_watermark}, State) ->
    {ok, stop_timer(State)};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), State :: #state{}) ->
    {ok, Reply :: term(), NewState :: #state{}} |
    {ok, Reply :: term(), NewState :: #state{}, hibernate} |
    {swap_handler, Reply :: term(), Args1 :: term(), NewState :: #state{},
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    {remove_handler, Reply :: term()}).
handle_call({set_fn, NewFn, Args}, State) ->
    safe_run_fn(terminate, State),
    NewState = safe_run_fn(init, State#state{fn = NewFn, fn_state = Args}),
    {ok, ok, NewState};
handle_call(verify, State) ->
    NewState = State#state{timer_ref = undefined},
    {ok, ok, process_memory_watermark_alarm(NewState)};
handle_call({alarms, Alarms}, State) ->
    case proplists:is_defined(process_memory_high_watermark, Alarms) of
        true ->
            NewState = set_memory_threshold(State),
            {ok, ok, process_memory_watermark_alarm(NewState)};
        _ -> {ok, ok, State}
    end;
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Args :: (term() | {stop, Reason :: term()} | stop |
                         {error, {'EXIT', Reason :: term()}} |
                         remove_handler | {error, term()}),
                State :: term()) -> term().
terminate(_Arg, State) ->
    safe_run_fn(terminate, State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_metric(Metric, Type) ->
    case exometer:info(Metric, name) of
        undefined -> exometer:new(Metric, Type);
        _ -> ok
    end.

update_metric(Metric, Value) ->
    exometer:update(Metric, Value).

safe_run_fn(terminate, #state{fn = Fn, fn_state = FnState} = State) ->
    catch Fn(terminate, FnState),
    State#state{fn = ?DEFAULT_FN, fn_state = []};
safe_run_fn(Event, #state{fn = Fn, fn_state = FnState} = State) ->
    NewState = (catch Fn(Event, FnState)),
    case NewState of
        {'EXIT', _} ->
            catch Fn(exception, NewState),
            safe_run_fn(terminate, State);
        _ ->
            State#state{fn_state = NewState}
    end.

set_memory_threshold(State) ->
    {Total, _, _} = memsup:get_memory_data(),
    Watermark = memsup:get_procmem_high_watermark(),
    Threshold = trunc(Watermark * Total / 100),
    State#state{memory_threshold = Threshold}.

stop_timer(State) ->
    case State#state.timer_ref of
        undefined -> State;
        TRef ->
            timer:cancel(TRef),
            update_metric(?HIGH_MEM_PROCS,0),
            State#state{timer_ref = undefined, memory_threshold = 0}
    end.

process_memory_watermark_alarm(#state{memory_threshold = 0} = State) -> State;
process_memory_watermark_alarm(#state{memory_threshold = Threshold,
                                      verify_timeout   = Timeout} = State) ->

    Processes = [Pid || Pid <- processes(),
                        {memory, Mem} <- [process_info(Pid, memory)],
                        Mem > Threshold],
    update_metric(?HIGH_MEM_PROCS, length(Processes)),
    NewState = process_all_high_mem_processes(Processes, State),
    TRef = timer:apply_after(Timeout * 1000, ?MODULE, verify, []),
    NewState#state{timer_ref = TRef}.


process_all_high_mem_processes(Processes, State) ->
    lists:foldl(fun process_high_mem_process/2, State, Processes).

process_high_mem_process(Pid, State) ->
    case process_info(Pid, [memory,
                            message_queue_len,
                            initial_call,
                            dictionary]) of
        [{memory, Mem},
         {message_queue_len, MQL},
         {initial_call, InitCall},
         {dictionary, Dict}] ->
            InitialCall = proplists:get_value('$initial_call', Dict, InitCall),
            MsgQueueLen = MQL + proplists:get_value('$internal_queue_len', Dict, 0),
            Event = {Pid, InitialCall, Mem, MsgQueueLen},
            safe_run_fn(Event, State);
        _ -> State
    end.

process_current_alarms() ->
    Alarms = mongoose_watchdog:get_alarms(),
    mongoose_watchdog:call(?MODULE, {alarms, Alarms}).

default_fn(_Event, State) -> State.
