%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Collect messages and flush them into the database.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_odbc_async_pool_writer).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_archive).
-export([archive_size/4,
         archive_message/9,
         lookup_messages/3,
         remove_archive/4,
         purge_single_message/6,
         purge_multiple_messages/9]).

%% Helpers for debugging
-export([queue_length/1,
         queue_lengths/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PER_MESSAGE_FLUSH_TIME, [?MODULE, per_message_flush_time]).
-define(DEFAULT_POOL_SIZE, 32).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {
    flush_interval, %% milliseconds
    max_packet_size,
    max_subscribers,
    host,
    connection_pool :: atom(),
    number,
    acc=[],
    subscribers=[],
    flush_interval_tref}).

worker_prefix() ->
    "ejabberd_mod_mam_writer".

%% Ensure, that:
%% `worker_count(_) = Int * mod_mam_odbc_arch:partition_count()'
%%
%% For example,
%% `worker_count(_) = 32, partition_count() = 16'.
%% or
%% `worker_count(_) = 16, partition_count() = 16'.
worker_count(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, pool_size, ?DEFAULT_POOL_SIZE).

worker_names(Host) ->
    [{N, worker_name(Host, N)} || N <- lists:seq(0, worker_count(Host) - 1)].

worker_name(Host, N) ->
    list_to_atom(worker_prefix() ++ "_" ++ binary_to_list(Host) ++ "_" ++ integer_to_list(N)).

select_worker(Host, ArcID) ->
    N = worker_number(Host, ArcID),
    worker_name(Host, N).

worker_number(Host, ArcID) ->
    ArcID rem worker_count(Host).


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(Host, Opts) ->
    mongoose_metrics:ensure_metric(Host, ?PER_MESSAGE_FLUSH_TIME, histogram),
    PoolName = gen_mod:get_opt(odbc_pool, Opts, mongoose_rdbms_sup:pool(Host)),
    MaxSize = gen_mod:get_module_opt(Host, ?MODULE, max_packet_size, 30),
    mod_mam_odbc_arch:prepare_insert(insert_mam_message, 1),
    mod_mam_odbc_arch:prepare_insert(insert_mam_messages, MaxSize),

    start_workers(Host, PoolName, MaxSize),
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            start_pm(Host, Opts);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            start_muc(Host, Opts);
        false ->
            ok
    end.

stop(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            stop_pm(Host);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            stop_muc(Host);
        false ->
            ok
    end,
    stop_workers(Host).

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

start_pm(Host, _Opts) ->
    ejabberd_hooks:add(mam_archive_message, Host, ?MODULE, archive_message, 50),
    ejabberd_hooks:add(mam_archive_size, Host, ?MODULE, archive_size, 30),
    ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE, lookup_messages, 30),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 100),
    ejabberd_hooks:add(mam_purge_single_message, Host, ?MODULE, purge_single_message, 30),
    ejabberd_hooks:add(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 30),
    ok.

stop_pm(Host) ->
    ejabberd_hooks:delete(mam_archive_message, Host, ?MODULE, archive_message, 50),
    ejabberd_hooks:delete(mam_archive_size, Host, ?MODULE, archive_size, 30),
    ejabberd_hooks:delete(mam_lookup_messages, Host, ?MODULE, lookup_messages, 30),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 100),
    ejabberd_hooks:delete(mam_purge_single_message, Host, ?MODULE, purge_single_message, 30),
    ejabberd_hooks:delete(mam_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 30),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_archive_message, Host, ?MODULE, archive_message, 50),
    ejabberd_hooks:add(mam_muc_archive_size, Host, ?MODULE, archive_size, 30),
    ejabberd_hooks:add(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 30),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 100),
    ejabberd_hooks:add(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 30),
    ejabberd_hooks:add(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 30),
    ok.

stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_archive_message, Host, ?MODULE, archive_message, 50),
    ejabberd_hooks:delete(mam_muc_archive_size, Host, ?MODULE, archive_size, 30),
    ejabberd_hooks:delete(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 30),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 100),
    ejabberd_hooks:delete(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 30),
    ejabberd_hooks:delete(mam_muc_purge_multiple_messages, Host, ?MODULE,
                          purge_multiple_messages, 30),
    ok.


%%====================================================================
%% API
%%====================================================================

start_workers(Host, Pool, MaxSize) ->
    [start_worker(WriterProc, N, Host, Pool, MaxSize)
     || {N, WriterProc} <- worker_names(Host)].

stop_workers(Host) ->
    [stop_worker(WriterProc) ||  {_, WriterProc} <- worker_names(Host)].

start_worker(WriterProc, N, Host, Pool, MaxSize) ->
    WriterChildSpec =
    {WriterProc,
     {gen_server, start_link, [{local, WriterProc}, ?MODULE, [Host, N, Pool, MaxSize], []]},
     permanent,
     5000,
     worker,
     [mod_mam_odbc_async_writer]},
    supervisor:start_child(mod_mam_sup, WriterChildSpec).

stop_worker(Proc) ->
    supervisor:terminate_child(mod_mam_sup, Proc),
    supervisor:delete_child(mod_mam_sup, Proc).


-spec archive_message(_Result, jid:server(), MessID :: mod_mam:message_id(),
                      ArchiveID :: mod_mam:archive_id(), LocJID :: jid:jid(),
                      RemJID :: jid:jid(), SrcJID :: jid:jid(), Dir :: atom(),
                      Packet :: any()) -> ok.
archive_message(_Result, Host,
                MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    Row = mod_mam_odbc_arch:prepare_message(Host, MessID, ArcID, LocJID,
                                            RemJID, SrcJID, Dir, Packet),

    Worker = select_worker(Host, ArcID),
    WorkerPid = whereis(Worker),
    %% Send synchronously if queue length is too long.
    case is_overloaded(WorkerPid) of
        false ->
            gen_server:cast(Worker, {archive_message, Row});
        true ->
            {Pid, MonRef} = spawn_monitor(fun() ->
                                                  gen_server:call(Worker, wait_flushing),
                                                  gen_server:cast(Worker, {archive_message, Row})
                                          end),
            receive
                {'DOWN', MonRef, process, Pid, normal} -> ok;
                {'DOWN', MonRef, process, Pid, _} ->
                    ejabberd_hooks:run(mam_drop_message, Host, [Host]),
                    {error, timeout}
            end
    end.

is_overloaded(Pid) ->
    {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
    Len > 500.

%% For metrics.
queue_length(Host) ->
    Len = lists:sum(queue_lengths(Host)),
    {ok, Len}.

queue_lengths(Host) ->
    [worker_queue_length(SrvName) || {_, SrvName} <- worker_names(Host)].

worker_queue_length(SrvName) ->
    case whereis(SrvName) of
        undefined ->
            0;
        Pid ->
            {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
            Len
    end.


-spec archive_size(Size :: integer(), Host :: jid:server(),
                   ArchiveID :: mod_mam:archive_id(), ArcJID :: jid:jid()) -> integer().
archive_size(Size, Host, ArcID, _ArcJID) when is_integer(Size) ->
    wait_flushing(Host, ArcID),
    Size.


-spec lookup_messages(Result :: any(), Host :: jid:server(), Params :: map()) ->
    {ok, mod_mam:lookup_result()} | {error, 'policy-violation'}.
lookup_messages(Result, Host, #{archive_id := ArcID, end_ts := End, now := Now}) ->
    wait_flushing_before(Host, ArcID, End, Now),
    Result.

%% #rh
-spec remove_archive(Acc :: map(), Host :: jid:server(),
                     RoomId :: mod_mam:archive_id(),
                     RoomJID :: jid:jid()) -> map().
remove_archive(Acc, Host, ArcID, _ArcJID) ->
    wait_flushing(Host, ArcID),
    Acc.

-spec purge_single_message(Result :: any(), Host :: jid:server(),
                           MessID :: mod_mam:message_id(), ArchiveID :: mod_mam:archive_id(),
                           RoomJID :: jid:jid(), Now :: mod_mam:unix_timestamp())
                          -> ok | {error, 'not-allowed' | 'not-found'}.
purge_single_message(Result, Host, MessID, ArcID, _ArcJID, Now) ->
    {Microseconds, _NodeMessID} = mod_mam_utils:decode_compact_uuid(MessID),
    wait_flushing_before(Host, ArcID, Microseconds, Now),
    Result.


-spec purge_multiple_messages(Result :: any(), Host :: jid:server(),
                              RoomID :: mod_mam:archive_id(), ArchiveID :: jid:jid(),
                              Borders :: mod_mam:borders() | undefined,
                              Start :: mod_mam:unix_timestamp() | undefined,
                              End :: mod_mam:unix_timestamp() | undefined,
                              Now :: mod_mam:unix_timestamp(),
                              WithJID :: jid:jid() | undefined) -> ok | {error, 'not-allowed'}.
purge_multiple_messages(Result, Host, ArcID, _ArcJID, _Borders,
                        _Start, End, Now, _WithJID) ->
    wait_flushing_before(Host, ArcID, End, Now),
    Result.

wait_flushing(Host, ArcID) ->
    gen_server:call(select_worker(Host, ArcID), wait_flushing).

wait_flushing_before(Host, ArcID, End, Now) ->
    case are_recent_entries_required(End, Now) of
        true ->
            wait_flushing(Host, ArcID);
        false ->
            ok
    end.

are_recent_entries_required(End, Now) when is_integer(End) ->
    %% 10 seconds
    End + 10000000 > Now;
are_recent_entries_required(_End, _Now) ->
    true.


%%====================================================================
%% Internal functions
%%====================================================================

run_flush(State = #state{acc = []}) ->
    State;
run_flush(State = #state{host = Host, acc = Acc}) ->
    MessageCount = length(Acc),
    {FlushTime, NewState} = timer:tc(fun do_run_flush/2, [MessageCount, State]),
    mongoose_metrics:update(Host, ?PER_MESSAGE_FLUSH_TIME, round(FlushTime / MessageCount)),
    NewState.

do_run_flush(MessageCount, State = #state{host = Host, connection_pool = Pool,
                                          max_packet_size = MaxSize, flush_interval_tref = TRef,
                                          acc = Acc, subscribers = Subs}) ->
    cancel_and_flush_timer(TRef),
    ?DEBUG("Flushed ~p entries.", [MessageCount]),

    InsertResult =
        case MessageCount of
            MaxSize ->
                mongoose_rdbms:execute(Pool, insert_mam_messages, lists:append(Acc));
            OtherSize ->
                Results = [mongoose_rdbms:execute(Pool, insert_mam_message, Row) || Row <- Acc],
                case lists:keyfind(error, 1, Results) of
                    false -> {updated, OtherSize};
                    Error -> Error
                end
        end,

    case InsertResult of
        {updated, _Count} -> ok;
        {error, Reason} ->
            ejabberd_hooks:run(mam_drop_messages, Host, [Host, MessageCount]),
            ?ERROR_MSG("archive_message query failed with reason ~p", [Reason]),
            ok
    end,
    spawn_link(fun() ->
                       [gen_server:reply(Sub, ok) || Sub <- Subs],
                       ejabberd_hooks:run(mam_flush_messages, Host, [Host, MessageCount])
               end),
    erlang:garbage_collect(),
    State#state{acc=[], subscribers=[], flush_interval_tref=undefined}.

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

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, N, Pool, MaxSize]) ->
    %% Use a private ODBC-connection.
    Int = gen_mod:get_module_opt(Host, ?MODULE, flush_interval, 2000),
    MaxSubs = gen_mod:get_module_opt(Host, ?MODULE, max_subscribers, 100),
    {ok, #state{host=Host, connection_pool=Pool, number=N,
                flush_interval = Int,
                max_packet_size = MaxSize,
                max_subscribers = MaxSubs}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(wait_flushing, _From, State=#state{acc=[]}) ->
    {reply, ok, State};
handle_call(wait_flushing, From,
            State=#state{max_subscribers=MaxSubs, subscribers=Subs}) ->
    State2 = State#state{subscribers=[From|Subs]},
    %% Run flusging earlier, if there are too much IQ requests waiting.
    %% Write only full packets of messages in overloaded state.
    case length(Subs) + 1 >= MaxSubs andalso not is_overloaded(self()) of
        true -> {noreply, run_flush(State2)};
        false -> {noreply, State2}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({archive_message, Row},
            State=#state{acc=Acc, flush_interval_tref=TRef, flush_interval=Int,
                         max_packet_size=Max}) ->
    TRef2 = case {Acc, TRef} of
                {[], undefined} -> erlang:send_after(Int, self(), flush);
                {_, _} -> TRef
            end,
    State2 = State#state{acc=[Row|Acc], flush_interval_tref=TRef2},
    case length(Acc) + 1 >= Max of
        true -> {noreply, run_flush(State2)};
        false -> {noreply, State2}
    end;
handle_cast(Msg, State) ->
    ?WARNING_MSG("Strange message ~p.", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info(flush, State) ->
    {noreply, run_flush(State#state{flush_interval_tref=undefined})}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
