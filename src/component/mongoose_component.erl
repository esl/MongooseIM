-module(mongoose_component).

-behaviour(mongoose_packet_handler).

%% API
-export([has_component/1,
         dirty_get_all_components/1,
         register_component/5,
         unregister_component/1,
         lookup_component/1,
         lookup_component/2]).

-export([start/0, stop/0]).
-export([node_cleanup/3]).
-export([process_packet/5]).

-include("mongoose.hrl").
-include("external_component.hrl").

-type external_component() :: #external_component{domain :: jid:lserver(),
                                                  handler :: mongoose_packet_handler:t(),
                                                  node :: node(),
                                                  is_subdomain :: boolean(),
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

-spec process_packet(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), #{pid := pid()}) ->
    mongoose_acc:t().
process_packet(Acc, _From, _To, _El, #{pid := Pid}) ->
    mongoose_component_connection:route(Pid, Acc),
    Acc.

-spec register_component(Domain :: jid:lserver(),
                         Node :: node(),
                         Handler :: mongoose_packet_handler:t(),
                         IsSubdomain :: boolean(),
                         IsHidden :: boolean()) -> {ok, external_component()} | {error, any()}.
register_component(Domain, Node, Handler, IsSubdomain, IsHidden) ->
    try
        register_component_unsafe(Domain, Node, Handler, IsSubdomain, IsHidden)
    catch Class:Reason:Stacktrace ->
        ?LOG_ERROR(#{what => component_register_failed,
                     class => Class, reason => Reason, stacktrace => Stacktrace}),
        {error, Reason}
    end.

register_component_unsafe(Domain, Node, Handler, IsSubdomain, IsHidden) ->
    LDomain = prepare_ldomain(Domain),
    Component = make_components(LDomain, Node, Handler, IsSubdomain, IsHidden),
    assert_can_register_components(Component),
    register_component(Component),
    %% We do it outside of Mnesia transaction
    run_register_hook(Component),
    {ok, Component}.

register_component(Component) ->
    mongoose_component_backend:register_components([Component]).

make_components(LDomain, Node, Handler, AreSubdomain, AreHidden) ->
    make_record_component(LDomain, Handler, Node, AreSubdomain, AreHidden).

make_record_component(LDomain, Handler, Node, IsSubdomain, IsHidden) ->
    #external_component{domain = LDomain, handler = Handler,
                        node = Node, is_subdomain = IsSubdomain, is_hidden = IsHidden}.

run_register_hook(#external_component{domain = LDomain, is_hidden = IsHidden}) ->
    mongoose_hooks:register_subhost(LDomain, IsHidden),
    ok.

run_unregister_hook(#external_component{domain = LDomain}) ->
    mongoose_hooks:unregister_subhost(LDomain),
    ok.

-spec unregister_component(external_component()) -> ok.
unregister_component(Component) ->
    run_unregister_hook(Component),
    mongoose_component_backend:unregister_components([Component]).

assert_can_register_components(Component) ->
    case is_already_registered(Component) of
        true ->
            error({routes_already_exist, Component#external_component.domain});
        false ->
            ok
    end.

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

prepare_ldomain(Domain) ->
    case jid:nameprep(Domain) of
        error ->
            error({invalid_domain, Domain});
        LDomain when is_binary(LDomain) ->
            LDomain
    end.
