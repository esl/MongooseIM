%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Main router
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_router).
-author('alexey@process-one.net').

-behaviour(gen_server).
%% API
-export([route/3,
         route/4,
         route_error/4,
         route_error_reply/4,
         is_component_dirty/1,
         dirty_get_all_components/1,
         register_components/2,
         register_components/3,
         register_components/4,
         register_component/2,
         register_component/3,
         register_component/4,
         lookup_component/1,
         lookup_component/2,
         unregister_component/1,
         unregister_component/2,
         unregister_components/1,
         unregister_components/2
        ]).

-export([start_link/0]).
-export([routes_cleanup_on_nodedown/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% debug exports for tests
-export([update_tables/0]).

-ignore_xref([register_component/2, register_component/3, register_component/4,
              register_components/2, register_components/3, route_error/4, start_link/0,
              unregister_component/1, unregister_component/2, unregister_components/2,
              unregister_routes/1, update_tables/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("external_component.hrl").

-record(state, {}).

-type domain() :: binary().

-type external_component() :: #external_component{domain :: domain(),
                                                  handler :: mongoose_packet_handler:t(),
                                                  is_hidden :: boolean()}.

% Not simple boolean() because is probably going to support third value in the future: only_hidden.
% Besides, it increases readability.
-type return_hidden() :: only_public | all.

-export_type([return_hidden/0]).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc The main routing function. It puts the message through a chain
%% of filtering/routing modules, as defined in config 'routing_modules'
%% setting (default is hardcoded in default_routing_modules function
%% of this module). Each of those modules should use xmpp_router behaviour
%% and implement two functions:
%% filter/3 - should return either 'drop' atom or its args
%% route/3, which should either:
%%   - deliver the message locally by calling mongoose_local_delivery:do_route/5
%%     and return 'done'
%%   - deliver the message it its own way and return 'done'
%%   - return its args
%%   - return a tuple of {From, To, Packet} which might be modified
%% For both functions, returning a 'drop' or 'done' atom terminates the procedure,
%% while returning a tuple means 'proceed' and the tuple is passed to
%% the next module in sequence.
-spec route(From   :: jid:jid(),
    To     :: jid:jid(),
    Packet :: mongoose_acc:t()|exml:element()) -> mongoose_acc:t().
route(From, To, #xmlel{} = Packet) ->
    % ?LOG_ERROR("Deprecated - it should be Acc: ~p", [Packet]),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => From#jid.lserver,
                              element => Packet,
                              from_jid => From,
                              to_jid => To }),
    % (called by broadcasting)
    route(From, To, Acc);
route(From, To, Acc) ->
    ?LOG_DEBUG(#{what => route, acc => Acc}),
    El = mongoose_acc:element(Acc),
    RoutingModules = mongoose_router:routing_modules_list(),
    NewAcc = route(From, To, Acc, El, RoutingModules),
    ?LOG_DEBUG(#{what => routing_result,
                 routing_result => mongoose_acc:get(router, result, {drop, undefined}, NewAcc),
                 routing_modules => RoutingModules, acc => Acc}),
    NewAcc.

-spec route(From   :: jid:jid(),
            To     :: jid:jid(),
            Acc :: mongoose_acc:t(),
            El :: exml:element() | {error, term()}) -> mongoose_acc:t().
route(_From, _To, Acc, {error, Reason} = Err) ->
    ?LOG_INFO(#{what => cannot_route_stanza, acc => Acc, reason => Reason}),
    mongoose_acc:append(router, result, Err, Acc);
route(From, To, Acc, El) ->
    Acc1 = mongoose_acc:update_stanza(#{ from_jid => From,
                                         to_jid => To,
                                         element => El }, Acc),
    ?LOG_DEBUG(#{what => route, acc => Acc1}),
    RoutingModules = mongoose_router:routing_modules_list(),
    NewAcc = route(From, To, Acc1, El, RoutingModules),
    ?LOG_DEBUG(#{what => routing_result,
                 routing_result => mongoose_acc:get(router, result, {drop, undefined}, NewAcc),
                 routing_modules => RoutingModules, acc => Acc}),
    NewAcc.

%% Route the error packet only if the originating packet is not an error itself.
%% RFC3920 9.3.1
-spec route_error(From   :: jid:jid(),
                  To     :: jid:jid(),
                  Acc :: mongoose_acc:t(),
                  ErrPacket :: exml:element()) -> mongoose_acc:t().
route_error(From, To, Acc, ErrPacket) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"error">> ->
            Acc;
        _ ->
            route(From, To, Acc, ErrPacket)
    end.

-spec route_error_reply(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    mongoose_acc:t().
route_error_reply(From, To, Acc, Error) ->
    {Acc1, ErrorReply} = jlib:make_error_reply(Acc, Error),
    route_error(From, To, Acc1, ErrorReply).


-spec register_components([Domain :: domain()],
                          Handler :: mongoose_packet_handler:t()) -> ok | {error, any()}.
register_components(Domains, Handler) ->
    register_components(Domains, node(), Handler).

-spec register_components([Domain :: domain()],
                          Node :: node(),
                          Handler :: mongoose_packet_handler:t()) -> ok | {error, any()}.
register_components(Domains, Node, Handler) ->
    register_components(Domains, Node, Handler, false).

-spec register_components([Domain :: domain()],
                          Node :: node(),
                          Handler :: mongoose_packet_handler:t(),
                          AreHidden :: boolean()) -> ok | {error, any()}.
register_components(Domains, Node, Handler, AreHidden) ->
    LDomains = [{jid:nameprep(Domain), Domain} || Domain <- Domains],
    F = fun() ->
            [do_register_component(LDomain, Handler, Node, AreHidden) || LDomain <- LDomains],
            ok
    end,
    case mnesia:transaction(F) of
        {atomic, ok}      -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc
%% components are registered in two places: external_components table as local components
%% and external_components_global as globals. Registration should be done by register_component/1
%% or register_components/1, which registers them for current node; the arity 2 funcs are
%% here for testing.
-spec register_component(Domain :: domain(),
                         Handler :: mongoose_packet_handler:t()) -> ok | {error, any()}.
register_component(Domain, Handler) ->
    register_component(Domain, node(), Handler).

-spec register_component(Domain :: domain(),
                         Node :: node(),
                         Handler :: mongoose_packet_handler:t()) -> ok | {error, any()}.
register_component(Domain, Node, Handler) ->
    register_component(Domain, Node, Handler, false).

-spec register_component(Domain :: domain(),
                         Node :: node(),
                         Handler :: mongoose_packet_handler:t(),
                         IsHidden :: boolean()) -> ok | {error, any()}.
register_component(Domain, Node, Handler, IsHidden) ->
    register_components([Domain], Node, Handler, IsHidden).

do_register_component({error, Domain}, _Handler, _Node, _IsHidden) ->
    error({invalid_domain, Domain});
do_register_component({LDomain, _}, Handler, Node, IsHidden) ->
    case check_component(LDomain, Node) of
        ok ->
            ComponentGlobal = #external_component{domain = LDomain, handler = Handler,
                                                  node = Node, is_hidden = IsHidden},
            mnesia:write(external_component_global, ComponentGlobal, write),
            NDomain = {LDomain, Node},
            Component = #external_component{domain = NDomain, handler = Handler,
                                            node = Node, is_hidden = IsHidden},
            mnesia:write(Component),
            mongoose_hooks:register_subhost(LDomain, IsHidden);
        _ -> mnesia:abort(route_already_exists)
    end.

%% @doc Check if the component/route is already registered somewhere; ok means it is not, so we are
%% ok to proceed, anything else means the domain/node pair is already serviced.
%% true and false are there because that's how orelse works.
-spec check_component(binary(), Node :: node()) -> ok | error.
check_component(LDomain, Node) ->
    case check_dynamic_domains(LDomain)
        orelse check_component_route(LDomain)
        orelse check_component_local(LDomain, Node)
        orelse check_component_global(LDomain, Node) of
        true -> error;
        false -> ok
    end.

check_dynamic_domains(LDomain)->
    {error, not_found} =/= mongoose_domain_api:get_host_type(LDomain).

%% check that route for this domain is not already registered
check_component_route(LDomain) ->
    no_route =/= mongoose_router:lookup_route(LDomain).

%% check that there is no local component for domain:node pair
check_component_local(LDomain, Node) ->
    NDomain = {LDomain, Node},
    [] =/= mnesia:read(external_component, NDomain).

%% check that there is no component registered globally for this node
check_component_global(LDomain, Node) ->
    undefined =/= get_global_component(LDomain, Node).

%% Find a component registered globally for this node (internal use)
get_global_component([], _) ->
    undefined;
get_global_component([Comp|Tail], Node) ->
    case Comp of
        #external_component{node = Node} ->
            Comp;
        _ ->
            get_global_component(Tail, Node)
    end;
get_global_component(LDomain, Node) ->
    get_global_component(mnesia:read(external_component_global, LDomain), Node).


-spec unregister_components([Domains :: domain()]) -> {atomic, ok}.
unregister_components(Domains) ->
    unregister_components(Domains, node()).
-spec unregister_components([Domains :: domain()], Node :: node()) -> {atomic, ok}.
unregister_components(Domains, Node) ->
    LDomains = [{jid:nameprep(Domain), Domain} || Domain <- Domains],
    F = fun() ->
            [do_unregister_component(LDomain, Node) || LDomain <- LDomains],
            ok
    end,
    {atomic, ok} = mnesia:transaction(F).

do_unregister_component({error, Domain}, _Node) ->
    error({invalid_domain, Domain});
do_unregister_component({LDomain, _}, Node) ->
    case get_global_component(LDomain, Node) of
        undefined ->
            ok;
        Comp ->
            ok = mnesia:delete_object(external_component_global, Comp, write)
    end,
    ok = mnesia:delete({external_component, {LDomain, Node}}),
    mongoose_hooks:unregister_subhost(LDomain),
    ok.

-spec unregister_component(Domain :: domain()) -> {atomic, ok}.
unregister_component(Domain) ->
    unregister_components([Domain]).

-spec unregister_component(Domain :: domain(), Node :: node()) -> {atomic, ok}.
unregister_component(Domain, Node) ->
    unregister_components([Domain], Node).

%% @doc Returns a list of components registered for this domain by any node,
%% the choice is yours.
-spec lookup_component(Domain :: jid:lserver()) -> [external_component()].
lookup_component(Domain) ->
    mnesia:dirty_read(external_component_global, Domain).

%% @doc Returns a list of components registered for this domain at the given node.
%% (must be only one, or nothing)
-spec lookup_component(Domain :: jid:lserver(), Node :: node()) -> [external_component()].
lookup_component(Domain, Node) ->
    mnesia:dirty_read(external_component, {Domain, Node}).

-spec dirty_get_all_components(return_hidden()) -> [jid:lserver()].
dirty_get_all_components(all) ->
    mnesia:dirty_all_keys(external_component_global);
dirty_get_all_components(only_public) ->
    MatchNonHidden = {#external_component{ domain = '$1', is_hidden = false, _ = '_' }, [], ['$1']},
    mnesia:dirty_select(external_component_global, [MatchNonHidden]).

-spec is_component_dirty(jid:lserver()) -> boolean().
is_component_dirty(Domain) ->
    [] =/= lookup_component(Domain).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    update_tables(),

    %% add distributed service_component routes
    mnesia:create_table(external_component,
                        [{attributes, record_info(fields, external_component)},
                         {local_content, true}]),
    mnesia:add_table_copy(external_component, node(), ram_copies),
    mnesia:create_table(external_component_global,
                        [{attributes, record_info(fields, external_component)},
                         {type, bag},
                         {record_name, external_component}]),
    mnesia:add_table_copy(external_component_global, node(), ram_copies),
    mongoose_metrics:ensure_metric(global, routingErrors, spiral),
    gen_hook:add_handlers(hooks()),

    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    gen_hook:delete_handlers(hooks()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec hooks() -> [gen_hook:hook_tuple()].
hooks() ->
    [{node_cleanup, global, fun ?MODULE:routes_cleanup_on_nodedown/3, #{}, 90}].

-spec route(From   :: jid:jid(),
            To     :: jid:jid(),
            Acc    :: mongoose_acc:t(),
            Packet :: exml:element(),
            [xmpp_router:t()]) -> mongoose_acc:t().
route(_From, _To, Acc, _Packet, []) ->
    ?LOG_ERROR(#{what => no_more_routing_modules, acc => Acc}),
    mongoose_metrics:update(global, routingErrors, 1),
    mongoose_acc:append(router, result, {error, out_of_modules}, Acc);
route(OrigFrom, OrigTo, Acc0, OrigPacket, [M|Tail]) ->
    try xmpp_router:call_filter(M, OrigFrom, OrigTo, Acc0, OrigPacket) of
        drop ->
            mongoose_acc:append(router, result, {drop, M}, Acc0);
        {OrigFrom, OrigTo, Acc1, OrigPacketFiltered} ->
            try xmpp_router:call_route(M, OrigFrom, OrigTo, Acc1, OrigPacketFiltered) of
                {done, Acc2} ->
                    mongoose_acc:append(router, result, {done, M}, Acc2);
                {From, To, NAcc1, Packet} ->
                    route(From, To, NAcc1, Packet, Tail)
                catch Class:Reason:Stacktrace ->
                    ?LOG_WARNING(#{what => routing_failed,
                                   router_module => M, acc => Acc1,
                                   class => Class, reason => Reason, stacktrace => Stacktrace}),
                    mongoose_acc:append(router, result, {error, {M, Reason}}, Acc1)
            end
        catch Class:Reason:Stacktrace ->
            ?LOG_WARNING(#{what => route_filter_failed,
                           text => <<"Error when filtering packet in router">>,
                           router_module => M, acc => Acc0,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
            mongoose_acc:append(router, result, {error, {M, Reason}}, Acc0)
    end.

update_tables() ->
    case catch mnesia:table_info(external_component, attributes) of
        [domain, handler, node] ->
            mnesia:delete_table(external_component);
        [domain, handler, node, is_hidden] ->
            ok;
        {'EXIT', _} ->
            ok
    end,
    case catch mnesia:table_info(external_component_global, attributes) of
        [domain, handler, node] ->
            UpdateFun = fun({external_component, Domain, Handler, Node}) ->
                                {external_component, Domain, Handler, Node, false}
                        end,
            mnesia:transform_table(external_component_global, UpdateFun,
                                   [domain, handler, node, is_hidden]);
        [domain, handler, node, is_hidden] ->
            ok;
        {'EXIT', _} ->
            ok
    end.

-spec routes_cleanup_on_nodedown(map(), map(), map()) -> {ok, map()}.
routes_cleanup_on_nodedown(Acc, #{node := Node}, _) ->
    Entries = mnesia:dirty_match_object(external_component_global,
                                        #external_component{node = Node, _ = '_'}),
    [mnesia:dirty_delete_object(external_component_global, Entry) || Entry <- Entries],
    {ok, maps:put(?MODULE, ok, Acc)}.
