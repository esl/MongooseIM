-module(mongoose_cleaner).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("mongoose.hrl").

-define(NODE_CLEANUP_LOCK(Node), {node_cleanup_lock, Node}).
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    case net_kernel:monitor_nodes(true) of
        ok ->
            {ok, #state{}};
        Error ->
            ?ERROR_MSG("can't monitor nodes: ~p~n", [Error]),
            {stop, Error}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodedown, Node}, State) ->
    ?WARNING_MSG("node=~p down", [Node]),
    cleanup_modules(Node),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cleanup_modules(Node) ->
    LockRequest = {?NODE_CLEANUP_LOCK(Node), self()},
    C = fun () -> run_node_cleanup(Node) end,
    Nodes = [node() | nodes()],
    Retries = 1,
    case global:trans(LockRequest, C, Nodes, Retries) of
        aborted ->
            ?DEBUG("could not get ~p~n", [?NODE_CLEANUP_LOCK(Node)]),
            {ok, aborted};
        Result ->
            {ok, Result}
    end.

run_node_cleanup(Node) ->
    {Elapsed, RetVal} = timer:tc(ejabberd_hooks, run_fold, [node_cleanup, #{}, [Node]]),
    ?WARNING_MSG("cleanup took=~pms, result: ~p~n",
                 [erlang:round(Elapsed / 1000), RetVal]),
    RetVal.
