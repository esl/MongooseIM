-module(cth_validate_nodes).

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_end_per_suite/4]).
-export([terminate/1]).

-record(state, {node_keys = []}).

%% CT callbacks

id(_Opts) ->
    "cth_validate_nodes_001".

init(_Id, _Opts) ->
    {ok, #state{}}.

pre_init_per_suite(_Suite, Config, State) ->
    case distributed_helper:validate_nodes() of
        {ok, NodeKeys} ->
            {Config, State#state{node_keys = NodeKeys}};
        {error, Reason} ->
            case os:getenv("SKIP_VALIDATE_NODES") of
                "true" ->
                    ct:pal("Skip failing with ~p in ct_check_rpc_nodes", [Reason]),
                    {Config, State};
                _ ->
                    {{fail, Reason}, State}
            end
    end.

post_end_per_suite(_SuiteName, _Config, Return, State = #state{node_keys = NodeKeys}) ->
    %% In case a suite is restarting the node at the end of the suite execution
    %% ensure we wait enough for it actually start
    distributed_helper:wait_for_nodes_to_start(NodeKeys),
    {Return, State#state{node_keys = []}}.

terminate(_State) ->
    ok.
