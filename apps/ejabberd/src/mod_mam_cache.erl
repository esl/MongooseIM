%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Stores cache using ETS-table.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_cache).
-export([start_link/0,
         user_id/2,
         remove_user_from_db/2,
         remove_user_from_cache/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {}).

%% @private
srv_name() ->
    mod_mam_cache.

tbl_name_user_id() ->
    mod_mam_cache_table_user_id.

group_name() ->
    mod_mam_cache.

su_key(LServer, LUserName) ->
    {LServer, LUserName}.

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

user_id(LServer, UserName) ->
    case lookup_user_id(LServer, UserName) of
        not_found ->
            UserId = query_user_id(LServer, UserName),
            update_user_id(LServer, UserName, UserId),
            UserId;
        UserId ->
            UserId
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
update_user_id(LServer, UserName, UserId) ->
    gen_server:call(srv_name(), {update_user_id, LServer, UserName, UserId}).

lookup_user_id(LServer, UserName) ->
    try
        ets:lookup_element(tbl_name_user_id(), su_key(LServer, UserName), 2)
    catch error:badarg ->
        not_found
    end.

query_user_id(LServer, UserName) ->
    SUserName = ejabberd_odbc:escape(UserName),
    Result =
    ejabberd_odbc:sql_query(
      LServer,
      ["SELECT id "
       "FROM mam_user "
       "WHERE user_name='", SUserName, "' "
       "LIMIT 1"]),

    case Result of
        {selected, ["id"], [{IdBin}]} ->
            binary_to_integer(IdBin);
        {selected, ["id"], []} ->
            %% The user is not found
            create_user_archive(LServer, UserName),
            query_user_id(LServer, UserName)
    end.
    
create_user_archive(LServer, UserName) ->
    SUserName = ejabberd_odbc:escape(UserName),
    {updated, 1} =
    ejabberd_odbc:sql_query(
      LServer,
      ["INSERT INTO mam_user "
       "(user_name) VALUES ('", SUserName, "')"]),
    ok.

remove_user_from_db(LServer, UserName) ->
    SUserName = ejabberd_odbc:escape(UserName),
    {updated, _} =
    ejabberd_odbc:sql_query(
      LServer,
      ["DELETE FROM mam_user "
       "WHERE user_name = '", SUserName, "'"]),
    ok.

remove_user_from_cache(Server, User) ->
    case pg2:get_members(group_name()) of
        Pids when is_list(Pids) ->
            [gen_server:cast(Pid, {remove_user, User, Server})
            || Pid <- Pids],
            ok;
        {error, _Reason} -> ok
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
init([]) ->
    pg2:create(group_name()),
    pg2:join(group_name(), self()),
    TOpts = [named_table, protected,
             {write_concurrency, false},
             {read_concurrency, true}],
    ets:new(tbl_name_user_id(), TOpts),
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
handle_call({update_user_id, LServer, UserName, UserId}, _From, State) ->
    ets:insert(tbl_name_user_id(), {su_key(LServer, UserName), UserId}),
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({remove_user, User, Server}, State) ->
    ets:delete(tbl_name_user_id(), su_key(Server, User)),
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

