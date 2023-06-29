-module(mongoose_component_backend).

-callback init(map()) -> any().

-callback node_cleanup(node()) -> ok.

-export([init/1,
         node_cleanup/1,
         register_components/1,
         unregister_components/1,
         lookup_component/1,
         lookup_component/2,
         get_all_components/1]).

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

register_components(Components) ->
    Args = [Components],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

unregister_components(Components) ->
    Args = [Components],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

lookup_component(Domain) ->
    Args = [Domain],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

lookup_component(Domain, Node) ->
    Args = [Domain, Node],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

get_all_components(ReturnHidden) ->
    Args = [ReturnHidden],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
