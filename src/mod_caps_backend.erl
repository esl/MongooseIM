-module(mod_caps_backend).
-export([init/2,
         read/2,
         write/3,
         delete_node/2]).

-define(MAIN_MODULE, mod_caps).

%% ----------------------------------------------------------------------
%% Callbacks
%% (exactly the same as specs in this module)

-callback init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts :: gen_mod:module_opts().

-callback read(mongooseim:host_type(), mod_caps:node_pair()) ->
    {ok, mod_caps:maybe_pending_features()} | error.

-callback write(mongooseim:host_type(), mod_caps:node_pair(),
                mod_caps:maybe_pending_features()) -> ok.

-callback delete_node(mongooseim:host_type(), mod_caps:node_pair()) -> ok.

%% ----------------------------------------------------------------------
%% API Functions

-spec init(HostType, Opts) -> ok when
    HostType :: mongooseim:host_type(),
    Opts :: gen_mod:module_opts().
init(HostType, Opts) ->
    TrackedFuns = [],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec read(mongooseim:host_type(), mod_caps:node_pair()) ->
    {ok, mod_caps:maybe_pending_features()} | error.
read(HostType, Node) ->
    Args = [HostType, Node],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec write(mongooseim:host_type(), mod_caps:node_pair(),
            mod_caps:maybe_pending_features()) -> ok.
write(HostType, Node, Features) ->
    Args = [HostType, Node, Features],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec delete_node(mongooseim:host_type(), mod_caps:node_pair()) -> ok.
delete_node(HostType, Node) ->
    Args = [HostType, Node],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
