-module(mongoose_component_backend).

-type external_component() :: mongoose_component:external_component().

-callback init(map()) -> any().

-callback node_cleanup(node()) -> ok.

-callback register_components(Components :: [external_component()]) -> ok.

-callback unregister_components(Components :: [external_component()]) -> ok.

-callback lookup_component(Domain :: jid:lserver()) -> [external_component()].

-callback lookup_component(Domain :: jid:lserver(), Node :: node()) -> [external_component()].

-callback get_all_components(ReturnHidden :: mongoose_component:return_hidden()) -> [jid:lserver()].

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

-spec node_cleanup(node()) -> ok.
node_cleanup(Node) ->
    Args = [Node],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec register_components(Components :: [external_component()]) -> ok.
register_components(Components) ->
    Args = [Components],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec unregister_components(Components :: [external_component()]) -> ok.
unregister_components(Components) ->
    Args = [Components],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec lookup_component(Domain :: jid:lserver()) -> [external_component()].
lookup_component(Domain) ->
    Args = [Domain],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec lookup_component(Domain :: jid:lserver(), Node :: node()) -> [external_component()].
lookup_component(Domain, Node) ->
    Args = [Domain, Node],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_all_components(ReturnHidden :: mongoose_component:return_hidden()) -> [jid:lserver()].
get_all_components(ReturnHidden) ->
    Args = [ReturnHidden],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
