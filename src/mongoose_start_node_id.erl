%% Generates an unique ID on the node start.
%% Registers the ID on all other nodes.
%% Used in ejabberd_local to find to which node to route IQ responses.
-module(mongoose_start_node_id).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([node_id/0, node_id_to_name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0]).

-include("mongoose.hrl").

-type id() :: binary().

-record(state, {start_id :: id()}).
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
    {ok, #state{start_id = StartId}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, RemoteNode}, State = #state{start_id = StartId}) ->
    register_on_remote_node(RemoteNode, StartId),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
     ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

register_on_remote_node(RemoteNode, StartId) ->
    rpc:call(RemoteNode, persistent_term, put, [{?KEY, StartId}, node()]).
