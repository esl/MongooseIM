%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Generic cassandra worker
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_cassandra_worker).

%% ----------------------------------------------------------------------
%% Exports


%% Internal exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([queue_length/1]).

-behaviour(gen_server).

%% ----------------------------------------------------------------------
%% Imports

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(state, {
          pool_name,
          query_refs = #{}
         }).

%%====================================================================
%% Internal functions
%%====================================================================

start_link(PoolName) ->
    gen_server:start_link(?MODULE, [PoolName], []).


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
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([PoolName]) ->
    mongoose_cassandra_sup:register_worker(PoolName, self()),
    {ok, #state{pool_name = PoolName}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_, _From, State = #state{}) ->
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({cql_write, QueryExecutor}, State = #state{query_refs = Refs}) ->
    QueryRef = QueryExecutor(),
    NewRefs = maps:put(QueryRef, QueryExecutor, Refs),
    {noreply, State#state{query_refs = NewRefs}};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unknown cast message ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------


handle_info({result, Tag, _}, State = #state{query_refs = Refs}) ->
    {noreply, State#state{query_refs = maps:remove(Tag, Refs)}};
handle_info({error, Tag, {16#1100, _, _}}, State = #state{query_refs = Refs}) ->
    QueryExecutor = maps:get(Tag, Refs),
    NewRefs1 = maps:remove(Tag, Refs),
    NewRefs2 = maps:put(QueryExecutor(), QueryExecutor, NewRefs1),
    {noreply, State#state{query_refs = NewRefs2}};
handle_info({error, Tag, _}, State = #state{query_refs = Refs}) ->
    {noreply, State#state{query_refs = maps:remove(Tag, Refs)}};
handle_info(Msg, State) ->
    ?WARNING_MSG("Unknown info message ~p.", [Msg]),
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
