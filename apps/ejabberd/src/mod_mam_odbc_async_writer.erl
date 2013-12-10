%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Collect messages and flush them into the database.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_odbc_async_writer).
%% Backend's callbacks
-export([start/2,
         stop/2,
         start_link/2,
         srv_name/1,
         archive_message/9,
         wait_flushing/4]).

%% Helpers
-export([queue_length/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {
    flush_interval=500,
    max_packet_size=30,
    host,
    conn,
    acc=[],
    subscribers=[],
    flush_interval_tref}).

%% @see srv_name/1
srv_name() ->
    ejabberd_mod_mam_writer.

%%====================================================================
%% API
%%====================================================================

start(Host, _Mod) ->
    WriterProc = srv_name(Host),
    WriterChildSpec =
    {WriterProc,
     {mod_mam_odbc_async_writer, start_link, [WriterProc, Host]},
     permanent,
     5000,
     worker,
     [mod_mam_odbc_async_writer]},
    supervisor:start_child(ejabberd_sup, WriterChildSpec).

stop(Host, _Mod) ->
    Proc = srv_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).


start_link(ProcName, Host) ->
    gen_server:start_link({local, ProcName}, ?MODULE, [Host], []).

srv_name(Host) ->
    gen_mod:get_module_proc(Host, srv_name()).

archive_message(Host, _Mod,
        MessID, UserID, LocJID, RemJID, SrcJID, Dir, Packet) ->
    Row = mod_mam_odbc_arch:prepare_message(Host,
        MessID, UserID, LocJID, RemJID, SrcJID, Dir, Packet),
    gen_server:cast(srv_name(Host), {archive_message, Row}).

%% For folsom.
queue_length(Host) ->
    case whereis(srv_name(Host)) of
    undefined ->
        {error, not_running};
    Pid ->
        {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
        {ok, Len}
    end.

wait_flushing(Host, _Mod, _ArcID, _ArcJID) ->
    gen_server:call(srv_name(Host), wait_flushing).

%%====================================================================
%% Internal functions
%%====================================================================

run_flush(State=#state{acc=[]}) ->
    State;
run_flush(State=#state{conn=Conn, flush_interval_tref=TRef, acc=Acc,
                       subscribers=Subs}) ->
    TRef =/= undefined andalso erlang:cancel_timer(TRef),
    ?DEBUG("Flushed ~p entries.", [length(Acc)]),
    Result = mod_mam_odbc_arch:archive_messages(Conn, Acc),
    case Result of
        {updated, _Count} -> ok;
        {error, Reason} ->
            ?ERROR_MSG("archive_message query failed with reason ~p", [Reason]),
            ok
    end,
    [gen_server:reply(Sub, ok) || Sub <- Subs],
    State#state{acc=[], subscribers=[], flush_interval_tref=undefined}.

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
init([Host]) ->
    %% Use a private ODBC-connection.
    {ok, Conn} = ejabberd_odbc:get_dedicated_connection(Host),
    {ok, #state{host=Host, conn=Conn}}.

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
    {noreply, run_flush(State)}.

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

