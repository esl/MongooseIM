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

-ignore_xref([start_link/0]).

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
            sync(), % If the node was just restarted, wait for the cleanup to finish
            {ok, #state{}};
        Error ->
            ?LOG_ERROR(#{what => cleaner_monitor_failed,
                         text => <<"mongoose_cleaner failed to monitor nodes">>,
                         reason => Error}),
            {stop, Error}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodedown, Node}, State) ->
    ?LOG_WARNING(#{what => cleaner_nodedown,
                   text => <<"mongoose_cleaner received nodenown event">>,
                   down_node => Node}),
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
    global_trans(Node, fun() -> run_node_cleanup(Node) end).

sync() ->
    global_trans(node(), fun() -> ok end).

global_trans(Node, Fun) ->
    LockKey = ?NODE_CLEANUP_LOCK(Node),
    LockRequest = {LockKey, self()},
    Nodes = [node() | nodes()],
    Retries = 10,
    case global:trans(LockRequest, Fun, Nodes, Retries) of
        aborted ->
            ?LOG_WARNING(#{what => cleaner_trans_aborted,
                           text => <<"mongoose_cleaner failed to get the global lock, run cleanup anyway">>,
                           remote_node => Node, lock_key => LockKey, retries => Retries}),
            Fun();
        Result ->
            Result
    end.

run_node_cleanup(Node) ->
    {Elapsed, RetVal} = timer:tc(fun() ->
            mongoose_hooks:node_cleanup(Node),
            [mongoose_hooks:node_cleanup_for_host_type(HostType, Node) || HostType <- ?ALL_HOST_TYPES],
            ok
        end),
    ?LOG_NOTICE(#{what => cleaner_done,
                  text => <<"Finished cleaning after dead node">>,
                  duration => erlang:round(Elapsed / 1000),
                  down_node => Node, result => RetVal}),
    RetVal.
