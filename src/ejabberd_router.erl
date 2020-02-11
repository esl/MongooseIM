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
         register_route/2,
         register_routes/2,
         unregister_route/1,
         unregister_routes/1,
         dirty_get_all_routes/0,
         dirty_get_all_domains/0,
         dirty_get_all_routes/1,
         dirty_get_all_domains/1,
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
-export([routes_cleanup_on_nodedown/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% debug exports for tests
-export([update_tables/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("route.hrl").
-include("external_component.hrl").

-record(state, {}).

-type domain() :: binary().

-type external_component() :: #external_component{domain :: domain(),
                                                  handler :: mongoose_packet_handler:t(),
                                                  is_hidden :: boolean()}.

% Not simple boolean() because is probably going to support third value in the future: only_hidden.
% Besides, it increases readability.
-type return_hidden() :: only_public | all.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Starts the server
%%--------------------------------------------------------------------


-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc The main routing function. It puts the message through a chain
%% of filtering/routing modules, as defined in config 'routing_modules'
%% setting (default is hardcoded in make_routing_module_source function
%% of this module). Each of those modules should use xmpp_router behaviour
%% and implement two functions:
%% filter/3 - should return either 'drop' atom or its args
%% route/3, which should either:
%%   - deliver the message locally by calling mongoose_local_delivery:do_local_route/3
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
    % ?ERROR_MSG("Deprecated - it should be Acc: ~p", [Packet]),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => From#jid.lserver,
                              element => Packet,
                              from_jid => From,
                              to_jid => To }),
    % (called by broadcasting)
    route(From, To, Acc);
route(From, To, Acc) ->
    ?DEBUG("route~n\tfrom ~p~n\tto ~p~n\tpacket ~p~n", [From, To, Acc]),
    El = mongoose_acc:element(Acc),
    RoutingModules = routing_modules_list(),
    NewAcc = route(From, To, Acc, El, RoutingModules),
    ?DEBUG("Routing using modules ~p resulted in ~p",
           [RoutingModules, mongoose_acc:get(router, result, {drop, undefined}, NewAcc)]),
    NewAcc.

-spec route(From   :: jid:jid(),
            To     :: jid:jid(),
            Acc :: mongoose_acc:t(),
            El :: exml:element() | {error, term()}) -> mongoose_acc:t().
route(From, To, Acc, {error, Reason} = Err) ->
    ?INFO_MSG("event=cannot_route_stanza,from=~p,to=~p,reason=~p,acc=~p", [From, To, Reason, Acc]),
    mongoose_acc:append(router, result, Err, Acc);
route(From, To, Acc, El) ->
    Acc1 = mongoose_acc:update_stanza(#{ from_jid => From,
                                         to_jid => To,
                                         element => El }, Acc),
    ?DEBUG("route~n\tfrom ~p~n\tto ~p~n\tpacket ~p~n", [From, To, Acc1]),
    RoutingModules = routing_modules_list(),
    NewAcc = route(From, To, Acc1, El, RoutingModules),
    ?DEBUG("Routing using modules ~p resulted in ~p",
           [RoutingModules, mongoose_acc:get(router, result, {drop, undefined}, NewAcc)]),
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
            ejabberd_hooks:run(register_subhost, [LDomain, IsHidden]);
        _ -> mnesia:abort(route_already_exists)
    end.

%% @doc Check if the component/route is already registered somewhere; ok means it is not, so we are
%% ok to proceed, anything else means the domain/node pair is already serviced.
%% true and false are there because that's how orelse works.
-spec check_component(binary(), Node :: node()) -> ok | true | false.
check_component(LDomain, Node) ->
    check_component_route(LDomain)
        orelse check_component_local(LDomain, Node)
        orelse check_component_global(LDomain, Node).


check_component_route(LDomain) ->
    %% check that route for this domain is not already registered
    case mnesia:read(route, LDomain) of
        [] ->
            false;
        _ ->
            true
    end.

check_component_local(LDomain, Node) ->
    %% check that there is no local component for domain:node pair
    NDomain = {LDomain, Node},
    case mnesia:read(external_component, NDomain) of
        [] ->
            false;
        _ ->
            true
    end.

check_component_global(LDomain, Node) ->
    %% check that there is no component registered globally for this node
    case get_global_component(LDomain, Node) of
        undefined ->
            ok;
        _ ->
            false
    end.

get_global_component([], _) ->
    %% Find a component registered globally for this node (internal use)
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
    ejabberd_hooks:run(unregister_subhost, [LDomain]),
    ok.

-spec unregister_component(Domain :: domain()) -> {atomic, ok}.
unregister_component(Domain) ->
    unregister_components([Domain]).

-spec unregister_component(Domain :: domain(), Node :: node()) -> {atomic, ok}.
unregister_component(Domain, Node) ->
    unregister_components([Domain], Node).

%% @doc Returns a list of components registered for this domain by any node,
%% the choice is yours.
-spec lookup_component(Domain :: domain()) -> [external_component()].
lookup_component(Domain) ->
    mnesia:dirty_read(external_component_global, Domain).

%% @doc Returns a list of components registered for this domain at the given node.
%% (must be only one, or nothing)
-spec lookup_component(Domain :: domain(), Node :: node()) -> [external_component()].
lookup_component(Domain, Node) ->
    mnesia:dirty_read(external_component, {Domain, Node}).

-spec register_route(Domain :: domain(),
                     Handler :: mongoose_packet_handler:t()) -> any().
register_route(Domain, Handler) ->
    register_route_to_ldomain(jid:nameprep(Domain), Domain, Handler).

-spec register_routes([domain()], mongoose_packet_handler:t()) -> 'ok'.
register_routes(Domains, Handler) ->
    lists:foreach(fun(Domain) ->
                      register_route(Domain, Handler)
                  end,
                  Domains).

-spec register_route_to_ldomain(binary(), domain(), mongoose_packet_handler:t()) -> any().
register_route_to_ldomain(error, Domain, _) ->
    erlang:error({invalid_domain, Domain});
register_route_to_ldomain(LDomain, _, Handler) ->
    mnesia:dirty_write(#route{domain = LDomain, handler = Handler}),
    % No support for hidden routes yet
    ejabberd_hooks:run(register_subhost, [LDomain, false]).

unregister_route(Domain) ->
    case jid:nameprep(Domain) of
        error ->
            erlang:error({invalid_domain, Domain});
        LDomain ->
            mnesia:dirty_delete(route, LDomain),
            ejabberd_hooks:run(unregister_subhost, [LDomain])
    end.

unregister_routes(Domains) ->
    lists:foreach(fun(Domain) ->
                      unregister_route(Domain)
                  end,
                  Domains).


-spec dirty_get_all_routes() -> [jid:lserver()].
dirty_get_all_routes() ->
    dirty_get_all_routes(all).

-spec dirty_get_all_domains() -> [jid:lserver()].
dirty_get_all_domains() ->
    dirty_get_all_domains(all).

-spec dirty_get_all_routes(return_hidden()) -> [jid:lserver()].
dirty_get_all_routes(ReturnHidden) ->
    lists:usort(all_routes(ReturnHidden)) -- ?MYHOSTS.

-spec dirty_get_all_domains(return_hidden()) -> [jid:lserver()].
dirty_get_all_domains(ReturnHidden) ->
    lists:usort(all_routes(ReturnHidden)).

-spec all_routes(return_hidden()) -> [jid:lserver()].
all_routes(all) ->
    mnesia:dirty_all_keys(route) ++ mnesia:dirty_all_keys(external_component_global);
all_routes(only_public) ->
    MatchNonHidden = {#external_component{ domain = '$1', is_hidden = false, _ = '_' }, [], ['$1']},
    mnesia:dirty_all_keys(route)
    ++
    mnesia:dirty_select(external_component_global, [MatchNonHidden]).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    update_tables(),
    mnesia:create_table(route,
                        [{attributes, record_info(fields, route)},
                         {local_content, true}]),
    mnesia:add_table_copy(route, node(), ram_copies),

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
    compile_routing_module(),
    mongoose_metrics:ensure_metric(global, routingErrors, spiral),
    ejabberd_hooks:add(node_cleanup, global, ?MODULE, routes_cleanup_on_nodedown, 90),

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State)
%%              -> {reply, Reply, State} |
%%                 {reply, Reply, State, Timeout} |
%%                 {noreply, State} |
%%                 {noreply, State, Timeout} |
%%                 {stop, Reason, Reply, State} |
%%                 {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet}, State) ->
    route(From, To, Packet),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ejabberd_hooks:delete(node_cleanup, global, ?MODULE, routes_cleanup_on_nodedown, 90),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

routing_modules_list() ->
    %% this is going to be compiled on startup from settings
    mod_routing_machine:get_routing_module_list().

compile_routing_module() ->
    Mods = case ejabberd_config:get_local_option(routing_modules) of
               undefined -> default_routing_modules();
               Defined -> Defined
           end,
    CodeStr = make_routing_module_source(Mods),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mod_routing_machine.erl", Code).

default_routing_modules() ->
    [mongoose_router_global,
     mongoose_router_localdomain,
     mongoose_router_external_localnode,
     mongoose_router_external,
     ejabberd_s2s].

make_routing_module_source(Mods) ->
    binary_to_list(iolist_to_binary(io_lib:format(
        "-module(mod_routing_machine).~n"
        "-compile(export_all).~n"
        "get_routing_module_list() -> ~p.~n",
        [Mods]))).

-spec route(From   :: jid:jid(),
            To     :: jid:jid(),
            Acc    :: mongoose_acc:t(),
            Packet :: exml:element(),
            [atom()]) -> mongoose_acc:t().
route(From, To, Acc, Packet, []) ->
    ?ERROR_MSG("event=no_more_routing_modules,from=~ts,to=~ts,packet=~ts",
               [jid:to_binary(From), jid:to_binary(To), exml:to_binary(Packet)]),
    mongoose_metrics:update(global, routingErrors, 1),
    mongoose_acc:append(router, result, {error, out_of_modules}, Acc);
route(OrigFrom, OrigTo, Acc, OrigPacket, [M|Tail]) ->
    case (catch xmpp_router:call_filter(M, OrigFrom, OrigTo, Acc, OrigPacket)) of
        {'EXIT', {Reason, StackTrace}} ->
            ?WARNING_MSG("event=filtering_error,from=~ts,to=~ts,module=~p~n~nreason=~p~n~n"
                         "packet=~ts~n~nstack_trace=~p~n",
                         [jid:to_binary(OrigFrom), jid:to_binary(OrigTo),
                          M, Reason, exml:to_binary(OrigPacket),
                          StackTrace]),
            mongoose_acc:append(router, result, {error, {M, Reason}}, Acc);
        drop ->
            mongoose_acc:append(router, result, {drop, M}, Acc);
        {OrigFrom, OrigTo, NAcc, OrigPacketFiltered} ->
            case catch(xmpp_router:call_route(M, OrigFrom, OrigTo, NAcc, OrigPacketFiltered)) of
                {'EXIT', {Reason, StackTrace}} ->
                    ?WARNING_MSG("event=routing_error,from=~ts,to=~ts,module=~p~n~nreason=~p~n~n"
                                 "packet=~ts~n~nstack_trace=~p~n",
                                 [jid:to_binary(OrigFrom), jid:to_binary(OrigTo),
                                  M, Reason, exml:to_binary(OrigPacketFiltered),
                                  StackTrace]),
                    mongoose_acc:append(router, result, {error, {M, Reason}}, NAcc);
                done ->
                    mongoose_acc:append(router, result, {done, M}, NAcc);
                {From, To, NAcc1, Packet} ->
                    route(From, To, NAcc1, Packet, Tail)
            end
    end.

update_tables() ->
    case catch mnesia:table_info(route, attributes) of
        [domain, node, pid] ->
            mnesia:delete_table(route);
        [domain, pid] ->
            mnesia:delete_table(route);
        [domain, pid, local_hint] ->
            mnesia:delete_table(route);
        [domain, handler] ->
            ok;
        {'EXIT', _} ->
            ok
    end,
    case lists:member(local_route, mnesia:system_info(tables)) of
        true ->
            mnesia:delete_table(local_route);
        false ->
            ok
    end,
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

-spec routes_cleanup_on_nodedown(map(), node()) -> map().
routes_cleanup_on_nodedown(Acc, Node) ->
    Entries = mnesia:dirty_match_object(external_component_global,
                                        #external_component{node = Node, _ = '_'}),
    [mnesia:dirty_delete_object(external_component_global, Entry) || Entry <- Entries],
    maps:put(?MODULE, ok, Acc).
