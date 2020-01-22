%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Collect messages and flush them into the database.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_muc_rdbms_async_pool_writer).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_archive).
-export([archive_size/4,
         archive_message/9,
         lookup_messages/3,
         remove_archive/4]).

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

-type packet() :: any().
-record(state, {flush_interval      :: non_neg_integer(), %% milliseconds
                max_batch_size     :: non_neg_integer(),
                host                :: jid:server(),
                acc=[]              :: list(),
                flush_interval_tref :: reference() | undefined
              }).
-type state() :: #state{}.

worker_prefix() ->
    "ejabberd_mod_mam_muc_writer".


%% @doc Ensure, that:
%% `worker_count(_) = Int * mod_mam_muc_rdbms_arch:partition_count()'
%%
%% For example,
%% `worker_count(_) = 32, partition_count() = 16'.
%% or
%% `worker_count(_) = 16, partition_count() = 16'.
worker_count(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, pool_size, ?DEFAULT_POOL_SIZE).


-spec worker_names(jid:server()) -> [{integer(), atom()}].
worker_names(Host) ->
    [{N, worker_name(Host, N)} || N <- lists:seq(0, worker_count(Host) - 1)].


-spec worker_name(jid:server(), integer()) -> atom().
worker_name(Host, N) ->
    list_to_atom(worker_prefix() ++ "_" ++ binary_to_list(Host) ++ "_" ++ integer_to_list(N)).


-spec select_worker(jid:server(), integer()) -> atom().
select_worker(Host, ArcID) ->
    N = worker_number(Host, ArcID),
    worker_name(Host, N).


-spec worker_number(jid:server(), mod_mam:archive_id()) -> integer().
worker_number(Host, ArcID) ->
    ArcID rem worker_count(Host).


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(jid:server(), _) -> 'ok'.
start(Host, Opts) ->
    mongoose_metrics:ensure_metric(Host, ?PER_MESSAGE_FLUSH_TIME, histogram),
    mongoose_metrics:ensure_metric(Host, ?FLUSH_TIME, histogram),
    MaxSize = gen_mod:get_module_opt(Host, ?MODULE, max_batch_size, 30),
    mod_mam_muc_rdbms_arch:prepare_insert(insert_mam_muc_message, 1),
    mod_mam_muc_rdbms_arch:prepare_insert(insert_mam_muc_messages, MaxSize),
    start_workers(Host, MaxSize),
    start_muc(Host, Opts).


-spec stop(jid:server()) -> any().
stop(Host) ->
    stop_muc(Host),
    stop_workers(Host).

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

-spec start_muc(jid:server(), _) -> 'ok'.
start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_archive_message, Host, ?MODULE, archive_message, 50),
    ok.


-spec stop_muc(jid:server()) -> 'ok'.
stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_archive_message, Host, ?MODULE, archive_message, 50),
    ok.

%%====================================================================
%% API
%%====================================================================

-spec start_workers(jid:server(), MaxSize :: pos_integer()) -> [{'error', _}
                                        | {'ok', 'undefined' | pid()}
                                        | {'ok', 'undefined' | pid(), _}].
start_workers(Host, MaxSize) ->
    [start_worker(WriterProc, N, Host, MaxSize)
     || {N, WriterProc} <- worker_names(Host)].


-spec stop_workers(jid:server()) -> ['ok'
    | {'error', 'not_found' | 'restarting' | 'running' | 'simple_one_for_one'}].
stop_workers(Host) ->
    [stop_worker(WriterProc) ||  {_, WriterProc} <- worker_names(Host)].


-spec start_worker(atom(), integer(), jid:server(), MaxSize :: pos_integer())
      -> {'error', _}
         | {'ok', 'undefined' | pid()}
         | {'ok', 'undefined' | pid(), _}.
start_worker(WriterProc, _N, Host, MaxSize) ->
    WriterChildSpec =
    {WriterProc,
     {gen_server, start_link, [{local, WriterProc}, ?MODULE, [Host, MaxSize], []]},
     permanent,
     5000,
     worker,
     [mod_mam_muc_rdbms_async_pool_writer]},
    supervisor:start_child(mod_mam_sup, WriterChildSpec).


-spec stop_worker(atom()) -> 'ok'
        | {'error', 'not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop_worker(Proc) ->
    supervisor:terminate_child(mod_mam_sup, Proc),
    supervisor:delete_child(mod_mam_sup, Proc).

-spec archive_message(_Result, Host :: jid:server(), MessID :: mod_mam:message_id(),
                      RoomID :: mod_mam:archive_id(), _LocJID :: jid:jid(),
                      SenderJID :: jid:jid(), UserRoomJID :: jid:jid(), Dir :: atom(),
                      Packet :: packet()) -> ok | {error, timeout}.
archive_message(_Result, Host, MessID, RoomID, _LocJID = #jid{},
                SenderJID = #jid{}, UserRoomJID = #jid{}, _Dir, Packet) ->
    Row = mod_mam_muc_rdbms_arch:prepare_message(Host, MessID, RoomID,
                                                 SenderJID, UserRoomJID, Packet),
    Worker = select_worker(Host, RoomID),
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


-spec is_overloaded(pid()) -> boolean().
is_overloaded(Pid) ->
    {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
    Len > 500.


%% @doc For metrics.
-spec queue_length(jid:server()) -> {'ok', number()}.
queue_length(Host) ->
    Len = lists:sum(queue_lengths(Host)),
    {ok, Len}.


-spec queue_lengths(jid:server()) -> [non_neg_integer()].
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


-spec archive_size(integer(), jid:server(), mod_mam:archive_id(),
                   jid:jid()) -> integer().
archive_size(Size, Host, ArcID, _ArcJID) when is_integer(Size) ->
    Size.


-spec lookup_messages(Result :: any(), Host :: jid:server(), Params :: map()) ->
    {ok, mod_mam:lookup_result()}.
lookup_messages(Result, Host, #{archive_id := ArcID, end_ts := End, now := Now}) ->
    Result.

%% #rh
-spec remove_archive(map(), jid:server(), mod_mam:archive_id(), jid:jid()) -> map().
remove_archive(Acc, Host, ArcID, _ArcJID) ->
    Acc.

%%====================================================================
%% Internal functions
%%====================================================================

-spec run_flush(state()) -> state().
run_flush(State = #state{acc=[]}) ->
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
                mongoose_rdbms:execute(Host, insert_mam_muc_messages, lists:append(Acc));
            OtherSize ->
                Results = [mongoose_rdbms:execute(Host, insert_mam_muc_message, Row) || Row <- Acc],
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
                       ejabberd_hooks:run(mam_muc_flush_messages, Host,
                                          [Host, MessageCount])
               end),
    erlang:garbage_collect(),
    State#state{acc=[], flush_interval_tref=undefined}.


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
init([Host, MaxSize]) ->
    %% Use a private RDBMS-connection.
    Int = gen_mod:get_module_opt(Host, ?MODULE, flush_interval, 2000),
    {ok, #state{host=Host, flush_interval = Int, max_batch_size = MaxSize}}.

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

-spec handle_info('flush', state()) -> {'noreply', state()}.
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
