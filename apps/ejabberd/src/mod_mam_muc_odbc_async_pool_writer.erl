%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Collect messages and flush them into the database.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_muc_odbc_async_pool_writer).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_archive).
-export([archive_size/4,
         archive_message/9,
         lookup_messages/14,
         remove_archive/3,
         purge_single_message/6,
         purge_multiple_messages/9]).

%% Helpers for debugging
-export([queue_length/1,
         queue_lengths/1]).

%% Internal exports
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_POOL_SIZE, 32).


-include("ejabberd.hrl").
-include("jlib.hrl").

-type packet() :: any().
-record(state, {flush_interval =500 :: non_neg_integer(),
                max_packet_size=30  :: non_neg_integer(),
                max_subscribers=100 :: non_neg_integer(),
                host                :: ejabberd:server(),
                conn,
                number              :: non_neg_integer(),
                acc=[]              :: list(),
                subscribers=[]      :: list(),
                flush_interval_tref :: reference()
              }).
-type state() :: #state{}.

worker_prefix() ->
    "ejabberd_mod_mam_muc_writer".


%% @doc Ensure, that:
%% `worker_count(_) = Int * mod_mam_muc_odbc_arch:partition_count()'
%%
%% For example,
%% `worker_count(_) = 32, partition_count() = 16'.
%% or
%% `worker_count(_) = 16, partition_count() = 16'.
worker_count(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, pool_size, ?DEFAULT_POOL_SIZE).


-spec worker_names(ejabberd:server()) -> [{integer(),atom()}].
worker_names(Host) ->
    [{N, worker_name(Host, N)} || N <- lists:seq(0, worker_count(Host) - 1)].


-spec worker_name(ejabberd:server(), integer()) -> atom().
worker_name(Host, N) ->
    list_to_atom(worker_prefix() ++ "_" ++ binary_to_list(Host) ++ "_" ++ integer_to_list(N)).


-spec select_worker(ejabberd:server(), integer()) -> atom().
select_worker(Host, ArcID) ->
    N = worker_number(Host, ArcID),
    worker_name(Host, N).


-spec worker_number(ejabberd:server(), mod_mam:archive_id()) -> integer().
worker_number(Host, ArcID) ->
    ArcID rem worker_count(Host).


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(ejabberd:server(),_) -> 'ok'.
start(Host, Opts) ->
    start_workers(Host),
    start_muc(Host, Opts).


-spec stop(ejabberd:server()) -> ['ok' | {'error','not_found' | 'restarting'
                                  | 'running' | 'simple_one_for_one'}].
stop(Host) ->
    stop_muc(Host),
    stop_workers(Host).

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

-spec start_muc(ejabberd:server(),_) -> 'ok'.
start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_archive_message, Host, ?MODULE, archive_message, 50),
    ejabberd_hooks:add(mam_muc_archive_size, Host, ?MODULE, archive_size, 30),
    ejabberd_hooks:add(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 30),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 100),
    ejabberd_hooks:add(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 30),
    ejabberd_hooks:add(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 30),
    ok.


-spec stop_muc(ejabberd:server()) -> 'ok'.
stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_archive_message, Host, ?MODULE, archive_message, 50),
    ejabberd_hooks:delete(mam_muc_archive_size, Host, ?MODULE, archive_size, 30),
    ejabberd_hooks:delete(mam_muc_lookup_messages, Host, ?MODULE, lookup_messages, 30),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 100),
    ejabberd_hooks:delete(mam_muc_purge_single_message, Host, ?MODULE, purge_single_message, 30),
    ejabberd_hooks:delete(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 30),
    ok.

%%====================================================================
%% API
%%====================================================================

-spec start_workers(ejabberd:server()) -> [{'error',_}
                                        | {'ok','undefined' | pid()}
                                        | {'ok','undefined' | pid(),_}].
start_workers(Host) ->
    [start_worker(WriterProc, N, Host)
     || {N, WriterProc} <- worker_names(Host)].


-spec stop_workers(ejabberd:server()) -> ['ok'
    | {'error','not_found' | 'restarting' | 'running' | 'simple_one_for_one'}].
stop_workers(Host) ->
    [stop_worker(WriterProc) ||  {_, WriterProc} <- worker_names(Host)].


-spec start_worker(atom(), integer(), ejabberd:server())
      -> {'error', _}
         | {'ok','undefined' | pid()}
         | {'ok','undefined' | pid(), _}.
start_worker(WriterProc, N, Host) ->
    WriterChildSpec =
    {WriterProc,
     {mod_mam_muc_odbc_async_pool_writer, start_link, [WriterProc, N, Host]},
     permanent,
     5000,
     worker,
     [mod_mam_muc_odbc_async_writer]},
    supervisor:start_child(mod_mam_sup, WriterChildSpec).


-spec stop_worker(atom()) -> 'ok'
        | {'error','not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop_worker(Proc) ->
    supervisor:terminate_child(mod_mam_sup, Proc),
    supervisor:delete_child(mod_mam_sup, Proc).


-spec start_link(atom(),_,_) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(ProcName, N, Host) ->
    gen_server:start_link({local, ProcName}, ?MODULE, [Host, N], []).


-spec archive_message(_, Host :: ejabberd:server(), MessID :: mod_mam:message_id(),
    ArcID :: mod_mam:archive_id(), LocJID :: ejabberd:jid(),
    RemJID :: ejabberd:jid(), SrcJID :: ejabberd:jid(), Dir :: atom(),
    Packet :: packet()) -> any().
archive_message(_Result, Host,
        MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    Row = mod_mam_muc_odbc_arch:prepare_message(Host,
        MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet),
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


-spec is_overloaded(pid()) -> boolean().
is_overloaded(Pid) ->
    {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
    Len > 500.


%% @doc For metrics.
-spec queue_length(ejabberd:server()) -> {'ok',number()}.
queue_length(Host) ->
    Len = lists:sum(queue_lengths(Host)),
    {ok, Len}.


-spec queue_lengths(ejabberd:server()) -> [non_neg_integer()].
queue_lengths(Host) ->
    [worker_queue_length(SrvName) || {_, SrvName} <- worker_names(Host)].


-spec worker_queue_length(atom()) -> non_neg_integer().
worker_queue_length(SrvName) ->
    case whereis(SrvName) of
    undefined ->
        0;
    Pid ->
        {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
        Len
    end.


-spec archive_size(integer(), ejabberd:server(), mod_mam:archive_id(),
                   ejabberd:jid()) -> integer().
archive_size(Size, Host, ArcID, _ArcJID) when is_integer(Size) ->
    wait_flushing(Host, ArcID),
    Size.


-spec lookup_messages(Result :: any(), Host :: ejabberd:server(),
        ArchiveID :: mod_mam:archive_id(), ArchiveJID :: ejabberd:jid(),
        RSM :: jlib:rsm_in() | undefined, Borders :: mod_mam:borders() | undefined,
        Start :: mod_mam:unix_timestamp() | undefined,
        End :: mod_mam:unix_timestamp() | undefined, Now :: mod_mam:unix_timestamp(),
        WithJID :: ejabberd:jid() | undefined, PageSize :: integer(),
        LimitPassed :: boolean() | opt_count, MaxResultLimit :: integer(),
        IsSimple :: boolean()) -> {ok, mod_mam:lookup_result()}
                                | {error, 'policy-violation'}.
  lookup_messages(Result, Host, ArcID, _ArcJID,
                _RSM, _Borders,
                _Start, End, Now, _WithJID,
                _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    wait_flushing_before(Host, ArcID, End, Now),
    Result.


-spec remove_archive(ejabberd:server(), mod_mam:archive_id(), ejabberd:jid()) -> 'ok'.
remove_archive(Host, ArcID, _ArcJID) ->
    wait_flushing(Host, ArcID),
    ok.


-spec purge_single_message(any(), ejabberd:server(), MessId :: mod_mam:message_id(),
    ArcID :: mod_mam:archive_id(), _ArcJID :: ejabberd:jid(),
    Now :: mod_mam:unix_timestamp()) -> any().
purge_single_message(Result, Host, MessID, ArcID, _ArcJID, Now) ->
    {Microseconds, _NodeMessID} = mod_mam_utils:decode_compact_uuid(MessID),
    wait_flushing_before(Host, ArcID, Microseconds, Now),
    Result.


-spec purge_multiple_messages(Result :: any(), Host :: ejabberd:server(),
    ArcID :: mod_mam:archive_id(), _ArcJID :: ejabberd:jid(),
    _Borders :: mod_mam:borders(), _Start :: mod_mam:unix_timestamp(),
    End :: mod_mam:unix_timestamp(), Now :: mod_mam:unix_timestamp(),
    _WithJID :: ejabberd:jid()) -> any().
purge_multiple_messages(Result, Host, ArcID, _ArcJID, _Borders,
                        _Start, End, Now, _WithJID) ->
    wait_flushing_before(Host, ArcID, End, Now),
    Result.


-spec wait_flushing(ejabberd:server(), mod_mam:archive_id()) -> any().
wait_flushing(Host, ArcID) ->
    gen_server:call(select_worker(Host, ArcID), wait_flushing).


-spec wait_flushing_before(ejabberd:server(), mod_mam:archive_id(),
      End :: mod_mam:unix_timestamp(), Now :: mod_mam:unix_timestamp()) -> any().
wait_flushing_before(Host, ArcID, End, Now) ->
    case is_recent_entries_required(End, Now) of
        true ->
            wait_flushing(Host, ArcID);
        false ->
            ok
    end.


%% @doc Returns true, if `End' is too old.
-spec is_recent_entries_required(mod_mam:unix_timestamp(),
                                 mod_mam:unix_timestamp()) -> boolean().
is_recent_entries_required(End, Now) when is_integer(End) ->
    %% If `End' is older than 10 seconds?
    End + 10000000 < Now;
is_recent_entries_required(_End, _Now) ->
    true.


%%====================================================================
%% Internal functions
%%====================================================================

-spec run_flush(state()) -> state().
run_flush(State=#state{acc=[]}) ->
    State;
run_flush(State=#state{host=Host, conn=Conn, number=N,
                       flush_interval_tref=TRef, acc=Acc, subscribers=Subs}) ->
    MessageCount = length(Acc),
    cancel_and_flush_timer(TRef),
    ?DEBUG("Flushed ~p entries.", [MessageCount]),
    Result = mod_mam_muc_odbc_arch:archive_messages(Conn, Acc, N),
    case Result of
        {updated, _Count} -> ok;
        {error, Reason} ->
            ejabberd_hooks:run(mam_drop_messages, Host, [Host, MessageCount]),
            ?ERROR_MSG("archive_message query failed with reason ~p", [Reason]),
            ok
    end,
    spawn_link(fun() ->
            [gen_server:reply(Sub, ok) || Sub <- Subs],
            ejabberd_hooks:run(mam_muc_flush_messages, Host,
                               [Host, MessageCount])
        end),
    erlang:garbage_collect(),
    State#state{acc=[], subscribers=[], flush_interval_tref=undefined}.


-spec cancel_and_flush_timer('undefined' | reference()) -> 'ok'.
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
init([Host, N]) ->
    %% Use a private ODBC-connection.
    {ok, Conn} = ejabberd_odbc:get_dedicated_connection(Host),
    {ok, #state{host=Host, conn=Conn, number=N}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
-spec handle_call('wait_flushing', _, state())
      -> {'noreply', state()} | {'reply','ok',state()}.
handle_call(get_connection, _From, State=#state{conn = Conn}) ->
    {reply, Conn, State};
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

-spec handle_info('flush',state()) -> {'noreply',state()}.
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

