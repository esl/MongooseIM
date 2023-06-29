%% Generates a unique ID on the node start.
%% Registers the ID on all other nodes.
%% Used in ejabberd_local to find to which node to route IQ responses.
-module(mongoose_start_node_id).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([node_id/0, node_id_to_name/1]).
-export([register_on_remote_node_rpc/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0, register_on_remote_node_rpc/3]).

-include("mongoose.hrl").
-include("mongoose_logger.hrl").

-type id() :: binary().

-record(state, {start_id :: id(), mon_ref_to_start_id :: map()}).
-define(KEY, ?MODULE).

-spec node_id() -> id().
node_id() ->
    persistent_term:get(?KEY).

-spec node_id_to_name(id()) -> {ok, node()} | {error, unknown_id}.
node_id_to_name(ID) ->
    case persistent_term:get({?KEY, ID}, undefined) of
        undefined ->
            {error, unknown_id};
        Name ->
            {ok, Name}
    end.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    net_kernel:monitor_nodes(true),
    StartId = mongoose_bin:gen_from_crypto(),
    persistent_term:put(mongoose_start_node_id, StartId),
    [register_on_remote_node(RemoteNode, StartId)
     || RemoteNode <- [node()|nodes()]],
    {ok, #state{start_id = StartId, mon_ref_to_start_id = #{}}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({register_cleaning_task, StartId, RemotePid},
            State = #state{mon_ref_to_start_id = Mon2StartId}) ->
    MonRef = erlang:monitor(process, RemotePid),
    Mon2StartId2 = maps:put(MonRef, StartId, Mon2StartId),
    {noreply, State#state{mon_ref_to_start_id = Mon2StartId2}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, RemoteNode}, State = #state{start_id = StartId}) ->
    register_on_remote_node(RemoteNode, StartId),
    {noreply, State};
handle_info({'DOWN', MonRef, process, RemotePid, Reason},
             State = #state{mon_ref_to_start_id = Mon2StartId}) ->
    case maps:get(MonRef, Mon2StartId, undefined) of
        undefined ->
            ?LOG_ERROR(#{what => node_id_unexpected_monitor,
                         reason => Reason,
                         monitor_ref => MonRef,
                         remote_pid => RemotePid,
                         remote_node => node(RemotePid)});
        StartId ->
            persistent_term:erase({?KEY, StartId}),
            ?LOG_WARNING(#{what => node_id_node_down,
                           reason => Reason,
                           monitor_ref => MonRef,
                           remote_pid => RemotePid,
                           remote_node => node(RemotePid)})
    end,
    %% We use pid monitors instead of node monitors to avoid cleaning
    %% start id when a node is restarting and reappearing very quicky.
    %% I.e. node name could be reused by a newly started node, while Refs - not.
    %% Pids could be also reused, but collisions are rare.
    {noreply, State#state{mon_ref_to_start_id = maps:remove(MonRef, Mon2StartId)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
     ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

register_on_remote_node(RemoteNode, StartId) ->
    Res = rpc:call(RemoteNode, ?MODULE, register_on_remote_node_rpc,
                   [node(), StartId, self()]),
    case Res of
        ok ->
            ok;
        _ ->
            ?LOG_ERROR(#{what => node_id_register_on_remote_node_failed,
                         remote_node => RemoteNode, reason => Res})
    end.

register_on_remote_node_rpc(RemoteNode, StartId, RemotePid) ->
    persistent_term:put({?KEY, StartId}, RemoteNode),
    gen_server:cast(?MODULE, {register_cleaning_task, StartId, RemotePid}),
    ok.
