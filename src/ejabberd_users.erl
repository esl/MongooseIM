%%% @doc Stores info about non-anonymous users using Mnesia table.
-module(ejabberd_users).
%% API
-export([start/1,
         stop/1,
         start_link/2,
         does_user_exist/2]).

%% Hooks.
-export([remove_user/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {}).

%% @see srv_name/1
srv_name() ->
    ejabberd_users.


-spec srv_name(jid:server() | string()) -> atom().
srv_name(Host) ->
    gen_mod:get_module_proc(Host, srv_name()).


tbl_name() ->
    ejabberd_users.


-spec tbl_name(jid:server() | string()) -> atom().
tbl_name(Host) ->
    gen_mod:get_module_proc(Host, tbl_name()).

%%====================================================================
%% API
%%====================================================================

-spec start(jid:server()) -> 'ok'.
start(Host) ->
    UserProc = srv_name(Host),
    UserChildSpec =
    {UserProc,
     {ejabberd_users, start_link, [UserProc, Host]},
     permanent,
     5000,
     worker,
     [ejabberd_users]},
    %% XXX lifecycle of this child is broken
    supervisor:start_child(ejabberd_sup, UserChildSpec),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ok.


-spec stop(jid:server()) -> 'ok'.
stop(Host) ->
    %% TODO properly remove ejabberd_sup child
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ok.


-spec start_link(ProcName :: atom(),
                 Host :: jid:server())
      -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(ProcName, Host) ->
    gen_server:start_link({local, ProcName}, ?MODULE, [Host], []).


-spec does_user_exist(LUser :: jid:luser(),
                     LServer :: jid:lserver() | string()) -> boolean().
does_user_exist(LUser, LServer) ->
    case does_cached_user_exist(LUser, LServer) of
        true -> true;
        false ->
            case does_stored_user_exist(LUser, LServer) of
                true ->
                    put_user_into_cache(LUser, LServer),
                    true;
                false -> false
        end
    end.


%%====================================================================
%% Hooks
%%====================================================================

-spec remove_user(Acc :: mongoose_acc:t(),
                  LUser :: jid:luser(),
                  LServer :: jid:lserver() | string()) -> mongoose_acc:t().
remove_user(Acc, LUser, LServer) ->
    delete_user(LUser, LServer),
    Acc.

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
-spec init([jid:server() | string(), ...]) -> {'ok', #state{}}.
init([Host]) ->
    Tab = tbl_name(Host),
    TabOpts = [named_table, public, set,
               {read_concurrency, true},
               {write_concurrency, false}],
    ets:new(Tab, TabOpts),
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
handle_call(_, _From, State=#state{}) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?WARNING_MSG("Strange message ~p.", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Msg, State) ->
    ?WARNING_MSG("Strange message ~p.", [Msg]),
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


%%====================================================================
%% Helpers
%%====================================================================

-spec does_stored_user_exist(jid:luser(), jid:lserver()) -> boolean().
does_stored_user_exist(LUser, LServer) ->
    ejabberd_auth:is_user_exists(LUser, LServer)
    andalso not is_anonymous_user(LUser, LServer).


-spec is_anonymous_user(jid:luser(), jid:lserver()) -> boolean().
is_anonymous_user(LUser, LServer) ->
    case lists:member(ejabberd_auth_anonymous, ejabberd_auth:auth_modules(LServer)) of
        true ->
            ejabberd_auth_anonymous:does_user_exist(LUser, LServer);
        false ->
            false
    end.


-spec does_cached_user_exist(jid:luser(), jid:lserver() | string()) -> boolean().
does_cached_user_exist(LUser, LServer) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(LServer),
    ets:info(Tab) =/= undefined andalso ets:member(Tab, Key).


-spec put_user_into_cache(jid:luser(), jid:lserver()) -> 'ok'.
put_user_into_cache(LUser, LServer) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(LServer),
    ets:insert(Tab, {Key}),
    ok.


-spec delete_user(jid:luser(), jid:lserver() | string()) -> 'ok'.
delete_user(LUser, LServer) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(LServer),
    ets:delete(Tab, Key),
    ok.


-spec key(jid:luser(), jid:lserver()
         ) -> {jid:luser(), jid:lserver()}.
key(LUser, LServer) ->
    {LUser, LServer}.
