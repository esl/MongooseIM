-module(service_node_id_backend).
-export([init/1]).

-define(MAIN_MODULE, service_node_id).

%% ----------------------------------------------------------------------
%% Callbacks
%% (exactly the same as specs in this module)

-callback init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts :: gen_mod:module_opts().

%% ----------------------------------------------------------------------
%% API Functions

-spec init(Opts) -> ok when
    Opts :: gen_mod:module_opts().
init(Opts) ->
    TrackedFuns = [],
    mongoose_backend:init(global, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [Opts],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
