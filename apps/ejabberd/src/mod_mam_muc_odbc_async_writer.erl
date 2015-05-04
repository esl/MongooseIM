%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Collect messages and flush them into the database.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_muc_odbc_async_writer).

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
         wait_flushing/1]).

%% Internal exports
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {flush_interval=500,
                max_packet_size=30,
                host :: ejabberd:server(),
                conn,
                acc=[],
                subscribers=[],
                flush_interval_tref :: reference()
              }).
-type state() :: #state{}.
-type packet() :: any().

srv_name() ->
    ejabberd_mod_mam_muc_writer.


%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(ejabberd:server(),_) -> 'ok'.
start(Host, Opts) ->
    start_server(Host),
    start_muc(Host, Opts).


-spec stop(ejabberd:server()) -> 'ok'
      | {'error','not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop(Host) ->
    stop_muc(Host),
    stop_server(Host).


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

-spec start_link(atom(), ejabberd:server()) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(ProcName, Host) ->
    gen_server:start_link({local, ProcName}, ?MODULE, [Host], []).


-spec srv_name(ejabberd:server() | string()) -> atom().
srv_name(Host) ->
    gen_mod:get_module_proc(Host, srv_name()).


-spec archive_message(_Result, ejabberd:server(), MessID :: mod_mam:message_id(),
        RoomID :: mod_mam:archive_id(), LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid(),
        SrcJID :: ejabberd:jid(), incoming, Packet :: any()) -> ok.
archive_message(_Result, Host,
    MessID, RoomID, LocJID, RemJID, SrcJID, incoming, Packet) ->
    Row = mod_mam_muc_odbc_arch:prepare_message(Host,
        MessID, RoomID, LocJID, RemJID, SrcJID, incoming, Packet),
    gen_server:cast(srv_name(Host), {archive_message, Row}).


%% @doc For metrics.
-spec queue_length(ejabberd:server() | string()) -> {'error','not_running'}
                                                  | {'ok',non_neg_integer()}.
queue_length(Host) ->
    case whereis(srv_name(Host)) of
    undefined ->
        {error, not_running};
    Pid ->
        {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
        {ok, Len}
    end.


-spec archive_size(Size :: integer(), Host :: ejabberd:server(),
        ArcId :: mod_mam:archive_id(), ArcJID :: ejabberd:jid()) -> integer().
archive_size(Size, Host, _UserID, _UserJID) when is_integer(Size) ->
    wait_flushing(Host),
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
lookup_messages(Result, Host, _UserID, _UserJID,
                _RSM, _Borders,
                _Start, End, Now, _WithJID,
                _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    wait_flushing_before(Host, End, Now),
    Result.


-spec remove_archive(Host :: ejabberd:server(), RoomId :: mod_mam:archive_id(),
        RoomJID :: ejabberd:jid()) -> 'ok'.
remove_archive(Host, _UserID, _UserJID) ->
    wait_flushing(Host),
    ok.


-spec purge_single_message(Result :: any(), ejabberd:server(),
        MessID :: mod_mam:message_id(), _UserID, _UserJID :: ejabberd:jid(),
        Now :: mod_mam:unix_timestamp())
            -> ok | {error, 'not-allowed' | 'not-found'}.
purge_single_message(Result, Host, MessID, _UserID, _UserJID, Now) ->
    {Microseconds, _NodeMessID} = mod_mam_utils:decode_compact_uuid(MessID),
    wait_flushing_before(Host, Microseconds, Now),
    Result.


-spec purge_multiple_messages(Result :: any(), Host :: ejabberd:server(),
        RoomID :: mod_mam:archive_id(), RoomJID :: ejabberd:jid(),
        Borders :: mod_mam:borders() | undefined,
        Start :: mod_mam:unix_timestamp() | undefined,
        End :: mod_mam:unix_timestamp() | undefined,
        Now :: mod_mam:unix_timestamp(),
        WithJID :: ejabberd:jid() | undefined) -> ok | {error, 'not-allowed'}.
purge_multiple_messages(Result, Host, _UserID, _UserJID, _Borders,
                        _Start, End, Now, _WithJID) ->
    wait_flushing_before(Host, End, Now),
    Result.


-spec wait_flushing(ejabberd:server() | string()) -> any().
wait_flushing(Host) ->
    gen_server:call(srv_name(Host), wait_flushing).


-spec wait_flushing_before(ejabberd:server(), End :: mod_mam:unix_timestamp(),
        Now :: mod_mam:unix_timestamp()) -> ok.
wait_flushing_before(Host, End, Now) ->
    case is_recent_entries_required(End, Now) of
        true ->
            wait_flushing(Host);
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

-spec start_server(ejabberd:server() | string()) -> {'error',_}
                                                | {'ok','undefined' | pid()}
                                                | {'ok','undefined' | pid(),_}.
start_server(Host) ->
    WriterProc = srv_name(Host),
    supervisor:start_child(mod_mam_sup, writer_child_spec(WriterProc, Host)).


-spec stop_server(ejabberd:server()) -> 'ok'
        | {'error','not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop_server(Host) ->
    Proc = srv_name(Host),
    supervisor:terminate_child(mod_mam_sup, Proc),
    supervisor:delete_child(mod_mam_sup, Proc).

writer_child_spec(WriterProc, Host) ->
    {WriterProc,
     {?MODULE, start_link, [WriterProc, Host]},
     permanent,
     5000,
     worker,
     [?MODULE]}.


-spec run_flush(state()) -> state().
run_flush(State=#state{acc=[]}) ->
    State;
run_flush(State=#state{conn=Conn, flush_interval_tref=TRef, acc=Acc,
                       subscribers=Subs}) ->
    TRef =/= undefined andalso erlang:cancel_timer(TRef),
    ?DEBUG("Flushed ~p entries.", [length(Acc)]),
    Result = mod_mam_muc_odbc_arch:archive_messages(Conn, Acc),
    case Result of
        {updated, _Count} -> ok;
        {error, Reason} ->
            ?ERROR_MSG("archive_message query failed with reason ~p", [Reason]),
            ok
    end,
    [gen_server:reply(Sub, ok) || Sub <- Subs],
    State#state{acc=[], flush_interval_tref=undefined}.

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
-spec handle_call('wait_flushing',_, state()) -> {'noreply',state()}
                                               | {'reply','ok',state()}.
handle_call(get_connection, _From, State=#state{conn = Conn}) ->
    {reply, Conn, State};
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
-spec handle_cast(_,state()) -> {'noreply', state()}.
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

