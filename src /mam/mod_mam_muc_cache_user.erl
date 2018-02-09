%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Stores cache using ETS-table (special version for MUC).
%%% This module is a proxy for `mod_mam_odbc_user' (it should be started).
%%%
%%% There are 2 hooks for `mam_archive_id':
%%% `cached_archive_id/3' and `store_archive_id/3'.
%%%
%%% The differencies from `mod_mam_odbc_user' are:
%%%
%%% - This module deletes cached data after room deletion.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_muc_cache_user).

%% gen_mod handlers
-export([start/2, stop/1]).

%% ejabberd handlers
-export([cached_archive_id/3,
         store_archive_id/3,
         remove_archive/4]).

%% API
-export([clean_cache/1]).

%% Internal exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {}).

%% @private
srv_name() ->
    mod_mam_muc_cache.

tbl_name_archive_id() ->
    mod_mam_muc_cache_table_archive_id.

tbl_name_monitor() ->
    mod_mam_muc_cache_table_monitor.

su_key(#jid{lserver = LServer, luser = LUser}) ->
    {LServer, LUser}.

room_pid(RoomJID=#jid{}) ->
    ejabberd_hooks:run_fold(muc_room_pid, RoomJID#jid.lserver, undefined, [RoomJID]).

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(Host, Opts) ->
    start_server(Host),
    start_muc(Host, Opts).

stop(Host) ->
    stop_muc(Host),
    stop_server(Host).

writer_child_spec() ->
    {?MODULE,
     {?MODULE, start_link, []},
     permanent,
     5000,
     worker,
     [?MODULE]}.

start_server(_Host) ->
    supervisor:start_child(ejabberd_sup, writer_child_spec()).

stop_server(_Host) ->
    %% There is only one server for all hosts. Cannot stop it.
    ok.

%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc

start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_archive_id, Host, ?MODULE, cached_archive_id, 30),
    ejabberd_hooks:add(mam_muc_archive_id, Host, ?MODULE, store_archive_id, 70),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 100),
    ok.

stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_archive_id, Host, ?MODULE, cached_archive_id, 30),
    ejabberd_hooks:delete(mam_muc_archive_id, Host, ?MODULE, store_archive_id, 70),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 100),
    ok.


%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

cached_archive_id(undefined, _Host, ArcJID) ->
    case lookup_archive_id(ArcJID) of
        not_found ->
            put(mam_not_cached_flag, true),
            undefined;
        UserID ->
            UserID
    end.

store_archive_id(UserID, _Host, ArcJID) ->
    maybe_cache_archive_id(ArcJID, UserID),
    UserID.

%% #rh
remove_archive(Acc, _Host, _UserID, ArcJID) ->
    clean_cache(ArcJID),
    Acc.

%%====================================================================
%% Internal functions
%%====================================================================

maybe_cache_archive_id(ArcJID, UserID) ->
    case erase(mam_not_cached_flag) of
        undefined ->
            UserID;
        true ->
            cache_archive_id(ArcJID, UserID)
    end.

%% @doc Put the user id into cache.
%% @private
cache_archive_id(ArcJID, UserID) ->
    case room_pid(ArcJID) of
        {error, not_found} ->
            ok;
        RoomPid ->
            gen_server:call(srv_name(), {cache_archive_id, ArcJID, UserID, RoomPid})
    end.

lookup_archive_id(ArcJID) ->
    try
        ets:lookup_element(tbl_name_archive_id(), su_key(ArcJID), 2)
    catch error:badarg ->
        not_found
    end.

clean_cache(ArcJID) ->
    %% For each room all operations are executed on a single node only.
    gen_server:cast(srv_name(), {remove_user, ArcJID}).

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
init([]) ->
    TOpts = [named_table, protected,
             {write_concurrency, false},
             {read_concurrency, true}],
    ets:new(tbl_name_archive_id(), TOpts),
    ets:new(tbl_name_monitor(),    TOpts),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({cache_archive_id, ArcJID, UserID, RoomPid}, _From, State) ->
    Key = su_key(ArcJID),
    case RoomPid of
        {ok, processless} ->
            ok;
        {ok, Pid} ->
            MonRef = erlang:monitor(process, Pid),
            ets:insert(tbl_name_monitor(), {MonRef, Key})
    end,
    ets:insert(tbl_name_archive_id(), {Key, UserID}),
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({remove_user, ArcJID}, State) ->
    ets:delete(tbl_name_archive_id(), su_key(ArcJID)),
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Strange message ~p.", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------


handle_info({'DOWN', MonRef, process, Pid, Reason}, State) ->
    case ets:lookup(tbl_name_archive_id(), MonRef) of
        [] ->
            ?WARNING_MSG("Unknown monitor ~p from ~p with ~p.", [MonRef, Pid, Reason]);
        [{MonRef, Key}] ->
            ets:delete(tbl_name_archive_id(), MonRef),
            ets:delete(tbl_name_monitor(), Key)
    end,
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

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

