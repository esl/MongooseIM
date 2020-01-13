%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Collect messages and flush them into the database.
%%%
%%% Some backends allow ArcID can be not set (riak?).
%%% This means, that there are no "jid => integer" mapping.
%%%
%%% But this module requires ArcID to be an integer.
%%% ArcID is set by mod_mam_rdbms_user.
%%% If ArcID is undefined, it means that there can be issues
%%% in mod_mam_rdbms_user.
%%%
%%% We have `is_integer(ArcID)' check on each hook handler, so cases when
%%% `ArcID' is undefined would fail at the module entrance.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_rdbms_async_pool_writer).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_archive).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([archive_size/4,
         archive_message/9,
         lookup_messages/3]).

%% Helpers for debugging
-export([queue_length/1,
         queue_lengths/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PER_MESSAGE_FLUSH_TIME, [?MODULE, per_message_flush_time]).
-define(FLUSH_TIME, [?MODULE, flush_time]).
-define(DEFAULT_POOL_SIZE, 32).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {
    flush_interval, %% milliseconds
    max_batch_size,
    host,
    number,
    acc = [],
    flush_interval_tref}).

worker_prefix() ->
    "ejabberd_mod_mam_writer".

%% Ensure, that:
%% `worker_count(_) = Int * mod_mam_rdbms_arch:partition_count()'
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

worker_number(Host, ArcID) when is_integer(ArcID) ->
    ArcID rem worker_count(Host).

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(Host, Opts) ->
    mongoose_metrics:ensure_metric(Host, ?PER_MESSAGE_FLUSH_TIME, histogram),
    mongoose_metrics:ensure_metric(Host, ?FLUSH_TIME, histogram),
    MaxSize = gen_mod:get_module_opt(Host, ?MODULE, max_batch_size, 30),
    mod_mam_rdbms_arch:prepare_insert(insert_mam_message, 1),
    mod_mam_rdbms_arch:prepare_insert(insert_mam_messages, MaxSize),

    start_workers(Host, MaxSize),
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
    ok.

stop_pm(Host) ->
    ejabberd_hooks:delete(mam_archive_message, Host, ?MODULE, archive_message, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_archive_message, Host, ?MODULE, archive_message, 50),
    ok.

stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_archive_message, Host, ?MODULE, archive_message, 50),
    ok.


%%====================================================================
%% API
%%====================================================================

start_workers(Host, MaxSize) ->
    [start_worker(WriterProc, N, Host, MaxSize)
     || {N, WriterProc} <- worker_names(Host)].

stop_workers(Host) ->
    [stop_worker(WriterProc) ||  {_, WriterProc} <- worker_names(Host)].

start_worker(WriterProc, N, Host, MaxSize) ->
    WriterChildSpec =
    {WriterProc,
     {gen_server, start_link, [{local, WriterProc}, ?MODULE, [Host, N, MaxSize], []]},
     permanent,
     5000,
     worker,
     [mod_mam_rdbms_async_writer]},
    supervisor:start_child(mod_mam_sup, WriterChildSpec).

stop_worker(Proc) ->
    supervisor:terminate_child(mod_mam_sup, Proc),
    supervisor:delete_child(mod_mam_sup, Proc).


-spec archive_message(_Result, jid:server(), MessID :: mod_mam:message_id(),
                      ArchiveID :: mod_mam:archive_id(), LocJID :: jid:jid(),
                      RemJID :: jid:jid(), SrcJID :: jid:jid(), Dir :: atom(),
                      Packet :: any()) -> ok.
archive_message(_Result, Host,
                MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet)
        when is_integer(ArcID) ->
    Row = mod_mam_rdbms_arch:prepare_message(Host, MessID, ArcID, LocJID,
                                            RemJID, SrcJID, Dir, Packet),

    Worker = select_worker(Host, ArcID),
    WorkerPid = whereis(Worker),
    %% Send synchronously if queue length is too long.
    case is_overloaded(WorkerPid) of
        false ->
            gen_server:cast(Worker, {archive_message, Row});
        true ->
            {Pid, MonRef} = spawn_monitor(fun() ->
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
archive_size(Size, Host, ArcID, _ArcJID) when is_integer(Size), is_integer(ArcID) ->
    Size.

-spec lookup_messages(Result :: any(), Host :: jid:server(), Params :: map()) ->
    {ok, mod_mam:lookup_result()}.
lookup_messages(Result, Host, #{archive_id := ArcID, end_ts := End, now := Now})
    when is_integer(ArcID) ->
    Result.

%%====================================================================
%% Internal functions
%%====================================================================

run_flush(State = #state{acc = []}) ->
    State;
run_flush(State = #state{host = Host, acc = Acc}) ->
    MessageCount = length(Acc),
    {FlushTime, NewState} = timer:tc(fun do_run_flush/2, [MessageCount, State]),
    mongoose_metrics:update(Host, ?PER_MESSAGE_FLUSH_TIME, round(FlushTime / MessageCount)),
    mongoose_metrics:update(Host, ?FLUSH_TIME, FlushTime),
    NewState.

do_run_flush(MessageCount, State = #state{host = Host, max_batch_size = MaxSize,
                                          flush_interval_tref = TRef, acc = Acc}) ->
    cancel_and_flush_timer(TRef),
    ?DEBUG("Flushed ~p entries.", [MessageCount]),

    InsertResult =
        case MessageCount of
            MaxSize ->
                mongoose_rdbms:execute(Host, insert_mam_messages, lists:append(Acc));
            OtherSize ->
                Results = [mongoose_rdbms:execute(Host, insert_mam_message, Row) || Row <- Acc],
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
                       ejabberd_hooks:run(mam_flush_messages, Host, [Host, MessageCount])
               end),
    erlang:garbage_collect(),
    State#state{acc=[], flush_interval_tref=undefined}.

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
init([Host, N, MaxSize]) ->
    %% Use a private RDBMS-connection.
    Int = gen_mod:get_module_opt(Host, ?MODULE, flush_interval, 2000),
    {ok, #state{host=Host, number=N, flush_interval = Int, max_batch_size = MaxSize}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({archive_message, Row},
            State=#state{acc=Acc, flush_interval_tref=TRef, flush_interval=Int,
                         max_batch_size=Max}) ->
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
