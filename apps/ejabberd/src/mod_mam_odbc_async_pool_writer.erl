%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Collect messages and flush them into the database.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_odbc_async_pool_writer).
%% Backend's callbacks
-export([start/2,
         stop/2,
         start_link/3,
         archive_message/9,
         wait_flushing/4]).

%% Helpers
-export([queue_length/1,
         queue_lengths/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {
    flush_interval=500,
    max_packet_size=30,
    mod,
    host,
    conn,
    acc=[],
    subscribers=[],
    flush_interval_tref}).

worker_prefix() ->
    "ejabberd_mod_mam_writer".

worker_count(_Host) ->
    32.

worker_names(Host) ->
    [worker_name(Host, N) || N <- lists:seq(0, worker_count(Host) - 1)].

worker_name(Host, N) ->
    list_to_atom(worker_prefix() ++ "_" ++ binary_to_list(Host) ++ "_" ++ integer_to_list(N)).

select_worker(Host, ArcID) ->
    N = worker_number(Host, ArcID),
    worker_name(Host, N).

worker_number(Host, ArcID) ->
    ArcID rem worker_count(Host).

%%====================================================================
%% API
%%====================================================================

start(Host, Mod) ->
    [start_worker(WriterProc, Host, Mod) ||  WriterProc <- worker_names(Host)].

stop(Host, _Mod) ->
    [stop_worker(WriterProc) ||  WriterProc <- worker_names(Host)].

start_worker(WriterProc, Host, Mod) ->
    WriterChildSpec =
    {WriterProc,
     {mod_mam_odbc_async_pool_writer, start_link, [WriterProc, Host, Mod]},
     permanent,
     5000,
     worker,
     [mod_mam_odbc_async_writer]},
    supervisor:start_child(ejabberd_sup, WriterChildSpec).

stop_worker(Proc) ->
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).


start_link(ProcName, Host, Mod) ->
    gen_server:start_link({local, ProcName}, ?MODULE, [Host, Mod], []).

archive_message(Host, _Mod,
        MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    Row = mod_mam_odbc_arch:prepare_message(Host,
        MessID, ArcID, LocJID, RemJID, SrcJID, Dir, Packet),
    Worker = select_worker(Host, ArcID),
    %% Send synchronously if queue length is too long.
    case erlang:process_info(whereis(Worker), message_queue_len) of
       {message_queue_len, Len} when Len < 500 ->
           gen_server:cast(Worker, {archive_message, Row});
       {message_queue_len, _} ->
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

%% For folsom.
queue_length(Host) ->
    Len = lists:sum(queue_lengths(Host)),
    {ok, Len}.

queue_lengths(Host) ->
    [worker_queue_length(SrvName) || SrvName <- worker_names(Host)].

worker_queue_length(SrvName) ->
    case whereis(SrvName) of
    undefined ->
        0;
    Pid ->
        {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
        Len
    end.

wait_flushing(Host, _Mod, ArcID, _ArcJID) ->
    gen_server:call(select_worker(Host, ArcID), wait_flushing).

%%====================================================================
%% Internal functions
%%====================================================================

run_flush(State=#state{acc=[]}) ->
    State;
run_flush(State=#state{mod=Mod,host=Host, conn=Conn,
                       flush_interval_tref=TRef, acc=Acc, subscribers=Subs}) ->
    MessageCount = length(Acc),
    cancel_and_flush_timer(TRef),
    ?DEBUG("Flushed ~p entries.", [MessageCount]),
    Result = mod_mam_odbc_arch:archive_messages(Conn, Acc),
    case Result of
        {updated, _Count} -> ok;
        {error, Reason} ->
            ?ERROR_MSG("archive_message query failed with reason ~p", [Reason]),
            ok
    end,
    spawn_link(fun() ->
            [gen_server:reply(Sub, ok) || Sub <- Subs],
            ejabberd_hooks:run(mam_flush_messages, Host,
                               [Host, Mod, MessageCount])
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
init([Host, Mod]) ->
    %% Use a private ODBC-connection.
    {ok, Conn} = ejabberd_odbc:get_dedicated_connection(Host),
    {ok, #state{host=Host, conn=Conn, mod=Mod}}.

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
handle_call(wait_flushing, From, State=#state{subscribers=Subs}) ->
    {noreply, State#state{subscribers=[From|Subs]}}.

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

