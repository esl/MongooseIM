-module(mongoose_component).
%% API
-export([has_component/1,
         dirty_get_all_components/1,
         register_components/4,
         unregister_components/1,
         lookup_component/1,
         lookup_component/2]).

-export([start/0, stop/0]).
-export([node_cleanup/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("external_component.hrl").

-type domain() :: jid:server().

-type external_component() :: #external_component{domain :: domain(),
                                                  handler :: mongoose_packet_handler:t(),
                                                  node :: node(),
                                                  is_hidden :: boolean()}.

-export_type([external_component/0]).

% Not simple boolean() because is probably going to support third value in the future: only_hidden.
% Besides, it increases readability.
-type return_hidden() :: only_public | all.

-export_type([return_hidden/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    Backend = mongoose_config:get_opt(component_backend),
    mongoose_component_backend:init(#{backend => Backend}),
    gen_hook:add_handlers(hooks()).

stop() ->
    gen_hook:delete_handlers(hooks()).

-spec hooks() -> [gen_hook:hook_tuple()].
hooks() ->
    [{node_cleanup, global, fun ?MODULE:node_cleanup/3, #{}, 90}].

-spec register_components(Domain :: [domain()],
                          Node :: node(),
                          Handler :: mongoose_packet_handler:t(),
                          AreHidden :: boolean()) -> {ok, [external_component()]} | {error, any()}.
register_components(Domains, Node, Handler, AreHidden) ->
    try
        register_components_unsafe(Domains, Node, Handler, AreHidden)
    catch Class:Reason:Stacktrace ->
        ?LOG_ERROR(#{what => component_register_failed,
                     class => Class, reason => Reason, stacktrace => Stacktrace}),
        {error, Reason}
    end.

register_components_unsafe(Domains, Node, Handler, AreHidden) ->
    LDomains = prepare_ldomains(Domains),
    Components = make_components(LDomains, Node, Handler, AreHidden),
    assert_can_register_components(Components),
    register_components(Components),
    %% We do it outside of Mnesia transaction
    lists:foreach(fun run_register_hook/1, Components),
    {ok, Components}.

register_components(Components) ->
    mongoose_component_backend:register_components(Components).

make_components(LDomains, Node, Handler, AreHidden) ->
    [make_record_component(LDomain, Handler, Node, AreHidden) || LDomain <- LDomains].

make_record_component(LDomain, Handler, Node, IsHidden) ->
    #external_component{domain = LDomain, handler = Handler,
                        node = Node, is_hidden = IsHidden}.

run_register_hook(#external_component{domain = LDomain, is_hidden = IsHidden}) ->
    mongoose_hooks:register_subhost(LDomain, IsHidden),
    ok.

run_unregister_hook(#external_component{domain = LDomain}) ->
    mongoose_hooks:unregister_subhost(LDomain),
    ok.

-spec unregister_components(Components :: [external_component()]) -> ok.
unregister_components(Components) ->
    lists:foreach(fun run_unregister_hook/1, Components),
    mongoose_component_backend:unregister_components(Components).

assert_can_register_components(Components) ->
    ConflictComponents = lists:filter(fun is_already_registered/1, Components),
    ConflictDomains = records_to_domains(ConflictComponents),
    case ConflictDomains of
        [] ->
            ok;
         _ ->
            error({routes_already_exist, ConflictDomains})
    end.

records_to_domains(Components) ->
    [LDomain || #external_component{domain = LDomain} <- Components].

%% Returns true if any component route is registered for the domain.
-spec has_component(jid:lserver()) -> boolean().
has_component(Domain) ->
    [] =/= lookup_component(Domain).

%% @doc Check if the component/route is already registered somewhere.
-spec is_already_registered(external_component()) -> boolean().
is_already_registered(#external_component{domain = LDomain, node = Node}) ->
    has_dynamic_domains(LDomain)
        orelse has_domain_route(LDomain)
        orelse has_component_registered(LDomain, Node).

has_dynamic_domains(LDomain) ->
    {error, not_found} =/= mongoose_domain_api:get_host_type(LDomain).

%% check that route for this domain is not already registered
has_domain_route(LDomain) ->
    no_route =/= mongoose_router:lookup_route(LDomain).

%% check that there is no component registered globally for this node
has_component_registered(LDomain, Node) ->
    no_route =/= get_component(LDomain, Node).

%% Find a component registered globally for this node (internal use)
get_component(LDomain, Node) ->
    filter_component(lookup_component(LDomain), Node).

filter_component([], _) ->
    no_route;
filter_component([Comp|Tail], Node) ->
    case Comp of
        #external_component{node = Node} ->
            Comp;
        _ ->
            filter_component(Tail, Node)
    end.

%% @doc Returns a list of components registered for this domain by any node,
%% the choice is yours.
-spec lookup_component(Domain :: jid:lserver()) -> [external_component()].
lookup_component(Domain) ->
    mongoose_component_backend:lookup_component(Domain).

%% @doc Returns a list of components registered for this domain at the given node.
%% (must be only one, or nothing)
-spec lookup_component(Domain :: jid:lserver(), Node :: node()) -> [external_component()].
lookup_component(Domain, Node) ->
    mongoose_component_backend:lookup_component(Domain, Node).

-spec dirty_get_all_components(return_hidden()) -> [jid:lserver()].
dirty_get_all_components(ReturnHidden) ->
    mongoose_component_backend:get_all_components(ReturnHidden).

-spec node_cleanup(map(), map(), map()) -> {ok, map()}.
node_cleanup(Acc, #{node := Node}, _) ->
    mongoose_component_backend:node_cleanup(Node),
    {ok, maps:put(?MODULE, ok, Acc)}.

prepare_ldomains(Domains) ->
    LDomains = [jid:nameprep(Domain) || Domain <- Domains],
    Zip = lists:zip(Domains, LDomains),
    InvalidDomains = [Domain || {Domain, error} <- Zip],
    case InvalidDomains of
        [] ->
            LDomains;
         _ ->
            error({invalid_domains, InvalidDomains})
    end.
