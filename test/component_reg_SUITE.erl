-module(component_reg_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("mongoose.hrl").
-include("external_component.hrl").

all() ->
    [ registering, registering_with_local ].

init_per_suite(C) ->
    {ok, _} = application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mongoose_config:set_opts(opts()),
    meck:new(mongoose_domain_api, [no_link]),
    meck:expect(mongoose_domain_api, get_host_type,
                fun(_) -> {error, not_found} end),
    application:ensure_all_started(exometer_core),
    mongooseim_helper:start_link_loaded_hooks(),
    ejabberd_router:start_link(),
    C.

init_per_testcase(_, C) ->
    mongoose_router:start(),
    mongooseim_helper:start_link_loaded_hooks(),
    C.

end_per_suite(_C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    meck:unload(),
    mongoose_config:erase_opts().

opts() ->
    #{all_metrics_are_global => false,
      component_backend => mnesia,
      routing_modules => [xmpp_router_a, xmpp_router_b, xmpp_router_c]}.

registering(_C) ->
    Dom = <<"aaa.bbb.com">>,
    {ok, Comps} = mongoose_component:register_components([Dom], node(), mongoose_packet_handler:new(?MODULE), false),
    Lookup = mongoose_component:lookup_component(Dom),
    ?assertMatch([#external_component{}], Lookup),
    mongoose_component:unregister_components(Comps),
    ?assertMatch([], mongoose_component:lookup_component(Dom)),
    ok.

registering_with_local(_C) ->
    mongooseim_helper:start_link_loaded_hooks(),
    Dom = <<"aaa.bbb.com">>,
    ThisNode = node(),
    AnotherNode = 'another@nohost',
    Handler = mongoose_packet_handler:new(?MODULE), %% This handler is only for testing!
    {ok, Comps} = mongoose_component:register_components([Dom], node(), Handler, false),
    %% we can find it globally
    ?assertMatch([#external_component{node = ThisNode}], mongoose_component:lookup_component(Dom)),
    %% and for this node
    ?assertMatch([#external_component{node = ThisNode}],
                 mongoose_component:lookup_component(Dom, ThisNode)),
    %% but not for another node
    ?assertMatch([], mongoose_component:lookup_component(Dom, AnotherNode)),
    %% once we unregister it is not available
    mongoose_component:unregister_components(Comps),
    ?assertMatch([], mongoose_component:lookup_component(Dom)),
    ?assertMatch([], mongoose_component:lookup_component(Dom, ThisNode)),
    ?assertMatch([], mongoose_component:lookup_component(Dom, AnotherNode)),
    %% we can register from both nodes
    {ok, Comps2} = mongoose_component:register_components([Dom], ThisNode, Handler, false),
    %% passing node here is only for testing
    {ok, _Comps3} = mongoose_component:register_components([Dom], AnotherNode, Handler, false),
    %% both are reachable locally
    ?assertMatch([#external_component{node = ThisNode}],
                 mongoose_component:lookup_component(Dom, ThisNode)),
    ?assertMatch([#external_component{node = AnotherNode}],
                 mongoose_component:lookup_component(Dom, AnotherNode)),
    %% if we try global lookup we get two handlers
    ?assertMatch([_, _], mongoose_component:lookup_component(Dom)),
    %% we unregister one and the result is:
    mongoose_component:unregister_components(Comps2),
    ?assertMatch([], mongoose_component:lookup_component(Dom, ThisNode)),
    ?assertMatch([#external_component{node = AnotherNode}],
                 mongoose_component:lookup_component(Dom)),
    ?assertMatch([#external_component{node = AnotherNode}],
                 mongoose_component:lookup_component(Dom, AnotherNode)),
    ok.

process_packet(_From, _To, _Packet, _Extra) ->
    exit(process_packet_called).
