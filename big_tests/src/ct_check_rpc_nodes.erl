-module(ct_check_rpc_nodes).

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_end_per_suite/4]).
-export([terminate/1]).

-record(state, {}).

id(_Opts) ->
    "ct_check_rpc_nodes_hook_001".

init(_Id, _Opts) ->
    {ok, #state{}}.

pre_init_per_suite(_Suite, Config, State) ->
    case distributed_helper:validate_nodes() of
        ok ->
            {Config, State};
        {error, Reason} ->
            case os:getenv("SKIP_CHECK_RPC_NODES") of
                "true" ->
                    ct:pal("Skip failing with ~p in ct_check_rpc_nodes", [Reason]),
                    {Config, State};
                _ ->
                    {{fail, Reason}, State}
            end
    end.

post_end_per_suite(Suite,_Config, Return, State) ->
    {Return, State}.

terminate(State) ->
    ok.
