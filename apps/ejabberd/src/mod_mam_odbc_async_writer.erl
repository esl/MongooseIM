%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Collect messages and flush them into the database.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_odbc_async_writer).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-export([archive_size/4,
         archive_message/9,
         lookup_messages/14,
         remove_archive/3,
         purge_single_message/6,
         purge_multiple_messages/9]).

%% Helpers for debugging
-export([queue_length/1,
         wait_flushing/1]).

%% Internal exports
-export([start_link/2]).

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



%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(Host, Opts) ->
    start_server(Host),
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
    stop_server(Host).

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
    ejabberd_hooks:delete(mam_muc_purge_multiple_messages, Host, ?MODULE, purge_multiple_messages, 30),
    ok.


%%====================================================================
%% API
%%====================================================================

start_link(ProcName, Host) ->
    gen_server:start_link({local, ProcName}, ?MODULE, [Host], []).

archive_message(_Result, Host,
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

archive_size(Size, Host, _UserID, _UserJID) when is_integer(Size) ->
    wait_flushing(Host),
    Size.

lookup_messages(Result, Host, _UserID, _UserJID,
                _RSM, _Borders,
                _Start, End, Now, _WithJID,
                _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    wait_flushing_before(Host, End, Now),
    Result.

remove_archive(Host, _UserID, _UserJID) ->
    wait_flushing(Host),
    ok.

purge_single_message(Result, Host, MessID, _UserID, _UserJID, Now) ->
    {Microseconds, _NodeMessID} = mod_mam_utils:decode_compact_uuid(MessID),
    wait_flushing_before(Host, Microseconds, Now),
    Result.

purge_multiple_messages(Result, Host, _UserID, _UserJID, _Borders,
                        _Start, End, Now, _WithJID) ->
    wait_flushing_before(Host, End, Now),
    Result.

wait_flushing(Host) ->
    gen_server:call(srv_name(Host), wait_flushing).

wait_flushing_before(Host, End, Now) ->
    case is_recent_entries_required(End, Now) of
        true ->
            wait_flushing(Host);
        false ->
            ok
    end.

%% @doc Returns true, if `End' is too old.
is_recent_entries_required(End, Now) when is_integer(End) ->
    %% If `End' is older than 10 seconds?
    End + 10000000 < Now;
is_recent_entries_required(_End, _Now) ->
    true.


%%====================================================================
%% Internal functions
%%====================================================================

start_server(Host) ->
    WriterProc = srv_name(Host),
    supervisor:start_child(ejabberd_sup, writer_child_spec(WriterProc, Host)).

stop_server(Host) ->
    Proc = srv_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

srv_name(Host) ->
    gen_mod:get_module_proc(Host, srv_name()).

writer_child_spec(WriterProc, Host) ->
    {WriterProc,
     {mod_mam_odbc_async_writer, start_link, [WriterProc, Host]},
     permanent,
     5000,
     worker,
     [mod_mam_odbc_async_writer]}.

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

