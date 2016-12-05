%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Generic cassandra worker
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_cassandra_worker).

%% ----------------------------------------------------------------------
%% Exports

%% API
-export([cql_query/5,
         cql_query_pool/5,
         cql_query_async/5,
         cql_query_pool_async/5,
         cql_query_multi_async/5,
         cql_query_pool_multi_async/5,
         cql_batch/4,
         cql_batch_pool/4,
         cql_batch/5,
         cql_batch_pool/5]).

%% Helpers for debugging
-export([test_query/1,
         test_query/2,
         queue_length/1,
         queue_lengths/1,
         total_count_query/2]).

%% Internal exports
-export([start_link/4]).

%% mongoose_cassandra_worker callbacks
-export([prepared_queries/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-behaviour(gen_server).
-behaviour(mongoose_cassandra).

-callback prepared_queries() -> proplists:proplist().

%% ----------------------------------------------------------------------
%% Imports

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(state, {
          pool_name,
          conn,
          prepared_queries,
          query_refs,
          query_refs_count}).

-record(prepared_query, {
          query_id,
          query_types}).

%%====================================================================
%% Internal functions
%%====================================================================

start_link(PoolName, Addr, Port, ClientOptions) ->
    gen_server:start_link(?MODULE, [PoolName, Addr, Port, ClientOptions], []).

%% @doc Do CQL query
%%
%% UserJID is used for rate-limiting, statistics and debugging
cql_query(Worker, _UserJID, Module, QueryName, Params) when is_pid(Worker) ->
    ResultF = gen_server:call(Worker, {cql_query, Module, QueryName, Params}),
    {ok, Result} = ResultF(),
    case Result of
        void ->
            {ok, []};
        _ ->
            {ok, seestar_result:rows(Result)}
    end.

cql_query_async(Worker, _UserJID, Module, QueryName, Params) when is_pid(Worker) ->
    gen_server:cast(Worker, {async_cql_query, Module, QueryName, Params}).

cql_query_multi_async(Worker, _UserJID, Module, QueryName, MultiParams) when is_pid(Worker) ->
    gen_server:cast(Worker, {multi_async_cql_query, Module, QueryName, MultiParams}).

cql_batch(Worker, UserJID, Module, Queries, not_batch) ->
    cql_query_multi(Worker, UserJID, Module, Queries);
cql_batch(Worker, _UserJID, Module, Queries, BatchType) ->
    ResultF = gen_server:call(Worker, {cql_batch, Module, BatchType, Queries}),
    {ok, Result} = ResultF(),
    case Result of
        void ->
            {ok, []};
        _ ->
            {ok, seestar_result:rows(Result)}
    end.

%% @doc Run queries, abort if an error. No rollback
cql_query_multi(Worker, UserJID, Module, Queries) ->
    cql_query_multi(Worker, UserJID, Module, Queries, []).

cql_query_multi(Worker, UserJID, Module, [{QueryName, Params} | Queries], Results) ->
    case catch cql_query(Worker, UserJID, Module, QueryName, Params) of
        {ok, Result} ->
            cql_query_multi(Worker, UserJID, Module, Queries, [Result | Results]);
        Reason ->
            {error, [{reason, Reason}, {results, Results}]}
    end;
cql_query_multi(_Worker, _UserJID, _Module, [], Results) ->
    {ok, lists:reverse(Results)}.

cql_batch_pool(PoolName, UserJID, Module, Queries, BatchType) ->
    Worker = mongoose_cassandra_sup:select_worker(PoolName, UserJID),
    cql_batch(Worker, UserJID, Module, Queries, BatchType).

cql_batch(Worker, UserJID, Module, Queries) ->
    cql_batch(Worker, UserJID, Module, Queries, unlogged).

cql_batch_pool(PoolName, UserJID, Module, Queries) ->
    cql_batch_pool(PoolName, UserJID, Module, Queries, unlogged).

%% @doc Select worker and do cql query
cql_query_pool(PoolName, UserJID, Module, QueryName, Params) ->
    Worker = mongoose_cassandra_sup:select_worker(PoolName, UserJID),
    cql_query(Worker, UserJID, Module, QueryName, Params).

cql_query_pool_async(PoolName, UserJID, Module, QueryName, Params) ->
    Worker = mongoose_cassandra_sup:select_worker(PoolName, UserJID),
    cql_query_async(Worker, UserJID, Module, QueryName, Params).

cql_query_pool_multi_async(PoolName, UserJID, Module, QueryName, MultiParams) ->
    Worker = mongoose_cassandra_sup:select_worker(PoolName, UserJID),
    cql_query_multi_async(Worker, UserJID, Module, QueryName, MultiParams).

%% ----------------------------------------------------------------------
%% mongoose_cassandra_worker behaviour callbacks

prepared_queries() ->
    [{test_query, test_query_sql()}] ++
        total_count_queries().

tables() ->
    [mam_message,
     mam_muc_message,
     mam_config,
     private_storage,
     privacy_default_list,
     privacy_list,
     privacy_item,
     rosterusers,
     roster_version].

%% ----------------------------------------------------------------------
%% TEST CONNECTION

test_query_sql() ->
    "SELECT now() FROM system.local". %% "SELECT 1" for cassandra

test_query(PoolName) ->
    test_query(PoolName, undefined).

test_query(PoolName, UserJID) ->
    Workers = mongoose_cassandra_sup:get_all_workers(PoolName),
    [{Worker, try cql_query(Worker, UserJID, ?MODULE, test_query, []) of
                  {ok, [[_Now]]} -> ok;
                  Other -> {error, Other}
              catch Class:Reason -> {error, {Class, Reason}}
              end} || Worker <- Workers].

%% ----------------------------------------------------------------------
%% COUNT OBJECTS
%% Don't use these queries in production. Just for testing.

total_count_query(PoolName, Table) ->
    UserJID = undefined,
    Res = cql_query_pool(PoolName, UserJID, ?MODULE, {total_count_query, Table}, []),
    {ok, [[Count]]} = Res,
    Count.

total_count_queries() ->
    [{{total_count_query, T}, total_count_query_cql(T)} || T <- tables()].

total_count_query_cql(T) when is_atom(T) ->
    "SELECT COUNT(*) FROM " ++ atom_to_list(T).


%% ----------------------------------------------------------------------
%% QUEUE LENGTH

%% For metrics.
queue_length(PoolName) ->
    Len = lists:sum(queue_lengths(PoolName)),
    {ok, Len}.

queue_lengths(PoolName) ->
    Workers = mongoose_cassandra_sup:get_all_workers(PoolName),
    [worker_queue_length(Worker) || Worker <- Workers].

worker_queue_length(Worker) ->
    %% We really don't want to call process, because it can does not respond
    Info = erlang:process_info(Worker, [message_queue_len, dictionary]),
    case Info of
        undefined -> %% dead
            0;
        [{message_queue_len, ExtLen}, {dictionary, Dict}] ->
            %% External queue contains not only queued queries but also waiting responds.
            %% But it's usually 0.
            IntLen = proplists:get_value(query_refs_count, Dict, 0),
            ExtLen + IntLen
    end.

%%====================================================================
%% Internal SQL part
%%====================================================================

get_prepared_query(Conn, Module, QueryName, State = #state{prepared_queries = PreparedQueries}) ->
    Key = {Module, QueryName},
    case dict:find(Key, PreparedQueries) of
        {ok, Query} ->
            {ok, Query, State};
        error ->
            get_prepared_query_first_time(Conn, Module, QueryName, State)
    end.

get_prepared_query_first_time(Conn, Module, QueryName,
                              State = #state{prepared_queries = PreparedQueries}) ->
    Key = {Module, QueryName},
    case get_query_cql(Module, QueryName) of
        {ok, Cql} ->
            case prepare_query(Conn, Cql, Module, QueryName) of
                {ok, PreparedQuery} ->
                    PreparedQueries2 = dict:store(Key, PreparedQuery, PreparedQueries),
                    State2 = State#state{prepared_queries = PreparedQueries2},
                    {ok, PreparedQuery, State2};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_query_cql(Module, QueryName) ->
    try get_query_cql_unsafe(Module, QueryName)
    catch Class:Error ->
            Stacktrace = erlang:get_stacktrace(),
            ?ERROR_MSG("issue=get_query_cql_failed, query_module=~p, query_name=~p,
                        reason=~p:~p, stacktrace=~1000p",
               [Module, QueryName, Class, Error, Stacktrace]),
            {error, get_query_cql_failed}
    end.

get_query_cql_unsafe(Module, QueryName) ->
    List = Module:prepared_queries(),
    {QueryName, Cql} = lists:keyfind(QueryName, 1, List),
    {ok, Cql}.

prepare_query(Conn, Query, Module, QueryName) ->
    case seestar_session:prepare(Conn, Query) of
        {ok, Res} ->
            Types = seestar_result:types(Res),
            QueryID = seestar_result:query_id(Res),
            {ok, #prepared_query{query_id = QueryID, query_types = Types}};
        {error, Reason} ->
            ?ERROR_MSG("issue=preparing_query_failed, query_module=~p, query_name=~p, reason=~p",
                       [Module, QueryName, Reason]),
            {error, Reason}
    end.

execute_prepared_query(Conn, #prepared_query{query_id = QueryID, query_types = Types}, Params) ->
    seestar_session:execute_async(Conn, QueryID, Types, Params, one).

new_prepared_query(#prepared_query{query_id = QueryID, query_types = Types}, Params) ->
    seestar_session:new_batch_execute(QueryID, Types, Params).

new_batch_queries(Conn, Module, Queries, State) ->
    new_batch_queries(Conn, Module, Queries, [], State).

new_batch_queries(Conn, Module, [{QueryName, Params} | Queries], BatchQueries, State) ->
    case get_prepared_query(Conn, Module, QueryName, State) of
        {ok, PreparedQuery, State2} ->
            BatchQuery = new_prepared_query(PreparedQuery, Params),
            new_batch_queries(Conn, Module, Queries, [BatchQuery | BatchQueries], State2);
        {error, Reason} ->
            {error, Reason, State}
    end;
new_batch_queries(_Conn, _Module, [], BatchQueries, State) ->
    {ok, lists:reverse(BatchQueries), State}.

run_batch(Conn, BatchType, BatchQueries) ->
    seestar_session:batch_async(Conn, BatchType, BatchQueries, one).

save_query_ref(From, QueryRef, State = #state{query_refs = Refs, query_refs_count = RefsCount}) ->
    Refs2 = dict:store(QueryRef, From, Refs),
    put(query_refs_count, RefsCount + 1),
    State#state{query_refs = Refs2, query_refs_count = RefsCount + 1}.

forward_query_respond(ResultF, QueryRef,
                      State = #state{query_refs = Refs, query_refs_count = RefsCount}) ->
    case dict:find(QueryRef, Refs) of
        {ok, From} ->
            Refs2 = dict:erase(QueryRef, Refs),
            gen_server:reply(From, ResultF),
            put(query_refs_count, RefsCount - 1),
            State#state{query_refs = Refs2, query_refs_count = RefsCount - 1};
        error -> % lager:warning("Ignore response ~p ~p", [QueryRef, ResultF()]),
            State
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
init([PoolName, Addr, Port, ClientOptions]) ->
    async_spawn(Addr, Port, ClientOptions),
    State = #state{pool_name = PoolName},
    {ok, State}.

init_connection(ConnPid, Conn, State = #state{pool_name = PoolName}) ->
    erlang:monitor(process, ConnPid),
    mongoose_cassandra_sup:register_worker(PoolName, self()),
    put(query_refs_count, 0),
    State#state{
      conn             = Conn,
      query_refs       = dict:new(),
      query_refs_count = 0,
      prepared_queries = dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_, _From, State = #state{conn = undefined}) ->
    {reply, no_connection, State};
handle_call({cql_query, Module, QueryName, Params}, From,
            State = #state{conn = Conn}) ->
    case get_prepared_query(Conn, Module, QueryName, State) of
        {ok, PreparedQuery, State2} ->
            QueryRef = execute_prepared_query(Conn, PreparedQuery, Params),
            {noreply, save_query_ref(From, QueryRef, State2)};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({cql_batch, Module, BatchType, Queries}, From,
            State = #state{conn = Conn}) ->
    case new_batch_queries(Conn, Module, Queries, State) of
        {ok, BatchQueries, State2} ->
            QueryRef = run_batch(Conn, BatchType, BatchQueries),
            {noreply, save_query_ref(From, QueryRef, State2)};
        {error, Reason, State2} ->
            {reply, {error, Reason}, State2}
    end;
handle_call(_, _From, State = #state{}) ->
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(_, State = #state{conn = undefined}) ->
    {noreply, State};
handle_cast({async_cql_query, Module, QueryName, Params},
            State = #state{conn = Conn}) ->
    case get_prepared_query(Conn, Module, QueryName, State) of
        {ok, PreparedQuery, State2} ->
            execute_prepared_query(Conn, PreparedQuery, Params),
            {noreply, State2};
        {error, _Reason} ->
            {noreply, State}
    end;
handle_cast({multi_async_cql_query, Module, QueryName, MultiParams},
            State = #state{conn = Conn}) ->
    case get_prepared_query(Conn, Module, QueryName, State) of
        {ok, PreparedQuery, State2} ->
            [execute_prepared_query(Conn, PreparedQuery, Params) || Params <- MultiParams],
            {noreply, State2};
        {error, _Reason} ->
            {noreply, State}
    end;
handle_cast(Msg, State) ->
    ?WARNING_MSG("Strange cast message ~p.", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------


handle_info({connection_result, {ok, ConnPid, Conn}}, State = #state{conn = undefined}) ->
    State2 = init_connection(ConnPid, Conn, State),
    {noreply, State2};
handle_info({connection_result, Reason}, State = #state{conn = undefined}) ->
    ?ERROR_MSG("issue=\"Fail to connect to Cassandra\", reason=~1000p", [Reason]),
    {stop, {connection_result, Reason}, State};
handle_info(_, State = #state{conn = undefined}) ->
    {noreply, State};
handle_info({seestar_response, QueryRef, ResultF}, State) ->
    {noreply, forward_query_respond(ResultF, QueryRef, State)};
handle_info({'DOWN', _, process, Pid, Reason}, State = #state{conn = Pid}) ->
    ?ERROR_MSG("issue=\"Cassandra connection closed\", reason=~1000p", [Reason]),
    {stop, {dead_connection, Pid, Reason}, State};
handle_info(Msg, State) ->
    ?WARNING_MSG("Strange info message ~p.", [Msg]),
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

%% Helpers
%%--------------------------------------------------------------------

async_spawn(Addr, Port, ClientOptions) ->
    ?INFO_MSG("issue=\"Connecting to Cassandra\", address=~p, port=~p", [Addr, Port]),
    Parent = self(),
    proc_lib:spawn_link(
      fun() ->
              ConnectOptions = proplists:get_value(socket_options, ClientOptions, []),
              ?DEBUG("issue=\"seestar_session:start_link\", address=~p, port=~p, "
                     "client_options=~p, connect_options=~p",
                     [Addr, Port, ClientOptions, ConnectOptions]),
              Res = (catch seestar_session:start_link(Addr, Port, ClientOptions, ConnectOptions)),
              ?DEBUG("issue=\"seestar_session:start_link result\", result=~p",
                     [Res]),
              Parent ! {connection_result, Res},
              maybe_waitfor(Res)
      end).

-spec maybe_waitfor({ok, pid()} | term()) -> ok.
maybe_waitfor(Res) ->
    case Res of
        {ok, Pid} ->
            Mon = erlang:monitor(process, Pid),
            receive
                {'DOWN', Mon, process, Pid, _} -> ok
            end;
        _ ->
            ok
    end.

