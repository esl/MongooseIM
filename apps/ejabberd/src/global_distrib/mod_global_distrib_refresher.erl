%%%-------------------------------------------------------------------
%%% @author Konrad Zemek <konradzemek@konradzemek.local>
%%% @copyright (C) 2017, Konrad Zemek
%%% @doc
%%%
%%% @end
%%% Created :  8 Jun 2017 by Konrad Zemek <konradzemek@konradzemek.local>
%%%-------------------------------------------------------------------
-module(mod_global_distrib_refresher).

-behaviour(gen_server).

%% API
-export([start_link/0, add_key/2, del_key/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
     pids = #{},
     keys = #{},
     queue = queue:new(),
     last_stamp,
     refresh_after,
     refresh_fun
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(RefreshAfter, RefreshFun) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {RefreshAfter * 1000, RefreshFun}, []).

add_key(Key, Pid) ->
    gen_server:call(?SERVER, {add_key, Key, Pid}).

del_key(Key) ->
    gen_server:call(?SERVER, {del_key, Key}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({RefreshAfter, RefreshFun}) ->
    Now = p1_time_compat:monotonic_time(milli_seconds).
    erlang:send_after(Now + 1000, self(), refresh, [{abs, true}]),
    process_flag(trap_exit, true),
    {ok, #state{refresh_after = RefreshAfter, refresh_fun = RefreshFun,
                last_stamp = Now + 1000}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_key, Key, Pid}, _From, State) ->
    RefreshAt = p1_time_compat:monotonic_time(milli_seconds) + State#state.refresh_after,
    _Ref = erlang:monitor(Pid),
    Keys = maps:put(Key, Pid, State#state.keys),
    Queue = queue:in({RefreshAt, Key}, State#state.queue),
    Pids = maps:put(Pid, Key, State#state.pids),
    {reply, ok, State#state{keys = Keys, pids = Pids, queue = Queue}};
handle_call({del_key, Key}, _From, State) ->
    Pid = maps:get(Key, State#state.keys),
    erlang:demonitor(Pid),
    Keys = maps:remove(Key, State#state.keys),
    Pids = maps:remove(Pid, State#state.pids),
    {reply, ok, State#state{keys = Keys, pids = Pids, queue = Queue}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(refresh, State) ->
    #state{refresh_fun = RefreshFun, last_stamp = LastStamp, refresh_after = RefreshAfter, keys = Keys},
    NextStamp = LastStamp + 1000,
    {Queue, ToRefresh} = refresh_queue(State#state.queue, NextStamp, Keys, RefreshAfter),
    RefreshFun(ToRefresh),
    erlang:send_after(NextStamp, self(), refresh, [{abs, true}]),
    {noreply, State#state{last_stamp = NextStamp, queue = Queue}};
handle_info(_Info, State) ->
    {noreply, State}.

refresh_queue(Queue, NextStamp, Keys, RefreshAfter) ->
    refresh_queue(Queue, NextStamp, Keys, RefreshAfter, []).

refresh_queue(Queue, NextStamp, Keys, RefreshAfter, ToRefresh) ->
    case queue:peek(Queue) of
        {value, {Stamp, Key}} when Stamp < NextStamp ->
            NewQ0 = queue:drop(Queue),
            NewQ =
                case maps:is_key(Key, Keys) of
                    true -> queue:in({Stamp + RefreshAfter, Key}, NewQ0);
                    false -> NewQ0
                end,
            refresh_queue(NewQ, NextStamp, Keys, RefreshAfter, [Key | ToRefresh]);

        _ ->
            {Queue, ToRefresh}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


