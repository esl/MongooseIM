%% @doc Stores info about non-anonymous users using Mnesia table.
-module(ejabberd_users).
%% API
-export([start/1,
         stop/1,
         start_link/2,
         is_user_exists/2]).

%% Hooks.
-export([remove_user/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {}).

%% @see srv_name/1
srv_name() ->
    ejabberd_users.

srv_name(Host) ->
    gen_mod:get_module_proc(Host, srv_name()).

tbl_name() ->
    ejabberd_users.

tbl_name(Host) ->
    gen_mod:get_module_proc(Host, tbl_name()).

%%====================================================================
%% API
%%====================================================================

start(Host) ->
    UserProc = srv_name(Host),
    UserChildSpec =
    {UserProc,
     {ejabberd_users, start_link, [UserProc, Host]},
     permanent,
     5000,
     worker,
     [ejabberd_users]},
    supervisor:start_child(ejabberd_sup, UserChildSpec),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ok.

start_link(ProcName, Host) ->
    gen_server:start_link({local, ProcName}, ?MODULE, [Host], []).


is_user_exists(LUser, LServer) ->
    case is_cached_user_exists(LUser, LServer) of
        true -> true;
        false ->
            case is_stored_user_exists(LUser, LServer) of
                true ->
                    put_user_into_cache(LUser, LServer),
                    true;
                false -> false
        end
    end.


%%====================================================================
%% Hooks
%%====================================================================

remove_user(LUser, LServer) ->
    delete_user(LUser, LServer),
    ok.

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
    
is_stored_user_exists(LUser, LServer) ->
    ejabberd_auth:is_user_exists(LUser, LServer)
    andalso not is_anonymous_user(LUser, LServer).

is_anonymous_user(LUser, LServer) ->
    case lists:member(ejabberd_auth_anonymous, ejabberd_auth:auth_modules(LServer)) of
        true ->
            ejabberd_auth_anonymous:is_user_exists(LUser, LServer);
        false ->
            false
    end.

is_cached_user_exists(LUser, LServer) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(LServer),
    ets:member(Tab, Key).

put_user_into_cache(LUser, LServer) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(LServer),
    ets:insert(Tab, {Key}),
    ok.

delete_user(LUser, LServer) ->
    Key = key(LUser, LServer),
    Tab = tbl_name(LServer),
    ets:delete(Tab, Key),
    ok.

key(LUser, LServer) ->
    {LUser, LServer}.

