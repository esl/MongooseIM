%%% @doc Allocates unique integer ids for each node.
-module(service_node_id).
-export([start/1, stop/0, config_spec/0]).
-export([node_id/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").

-type nodeid() :: non_neg_integer().
-export_type([nodeid/0]).

start(Opts) ->
    {ok, NodeId} = service_node_id_backend:init(Opts),
    persistent_term:put(service_node_id, NodeId).

stop() ->
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{}.

%% @doc Return an integer node ID.
-spec node_id() -> nodeid().
node_id() ->
    %% We just return 0 if service is not running.
    persistent_term:get(service_node_id, 0).
