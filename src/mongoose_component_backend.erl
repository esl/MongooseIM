-module(mongoose_component_backend).

-callback init(map()) -> any().

-callback node_cleanup(node()) -> ok.

-export([init/1,
         node_cleanup/1]).

-ignore_xref([behaviour_info/1]).

-define(MAIN_MODULE, mongoose_component).

-spec init(map()) -> any().
init(Opts) ->
    Args = [Opts],
    mongoose_backend:init(global, ?MAIN_MODULE, [], Opts),
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

node_cleanup(Node) ->
    Args = [Node],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
