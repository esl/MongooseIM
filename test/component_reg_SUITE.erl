-module(component_reg_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("mongoose.hrl").
-include("external_component.hrl").

all() ->
    [ registering, registering_with_local ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mongoose_config:set_opts(opts()),
    meck:new(mongoose_domain_api, [no_link]),
    meck:expect(mongoose_domain_api, get_host_type, fun(_) -> {error, not_found} end),
    application:ensure_all_started(exometer_core),
    async_helper:start(Config, [{mongoose_instrument, start_link, []},
                                {mongooseim_helper, start_link_loaded_hooks, []},
                                {ejabberd_router, start_link, []}]).

init_per_testcase(_, Config) ->
    mongoose_router:start(),
    Config.

end_per_suite(Config) ->
    async_helper:stop_all(Config),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    meck:unload(),
    mongoose_config:erase_opts().

opts() ->
    #{hosts => [],
      host_types => [],   
      component_backend => mnesia,
      routing_modules => [xmpp_router_a, xmpp_router_b, xmpp_router_c],
      instrumentation => config_parser_helper:default_config([instrumentation])}.

registering(_C) ->
    Dom = <<"aaa.bbb.com">>,
    {ok, Comp} = mongoose_component:register_component(
                    Dom, node(), mongoose_packet_handler:new(?MODULE), false, false),
    Lookup = mongoose_component:lookup_component(Dom),
    ?assertMatch([#external_component{}], Lookup),
    mongoose_component:unregister_component(Comp),
    ?assertMatch([], mongoose_component:lookup_component(Dom)),
    ok.

registering_with_local(_C) ->
    mongooseim_helper:start_link_loaded_hooks(),
    Dom = <<"aaa.bbb.com">>,
    ThisNode = node(),
    AnotherNode = 'another@nohost',
    Handler = mongoose_packet_handler:new(?MODULE), %% This handler is only for testing!
    {ok, Comp} = mongoose_component:register_component(Dom, node(), Handler, false, false),
    %% we can find it globally
    ?assertMatch([#external_component{node = ThisNode}], mongoose_component:lookup_component(Dom)),
    %% and for this node
    ?assertMatch([#external_component{node = ThisNode}],
                 mongoose_component:lookup_component(Dom, ThisNode)),
    %% but not for another node
    ?assertMatch([], mongoose_component:lookup_component(Dom, AnotherNode)),
    %% once we unregister it is not available
    mongoose_component:unregister_component(Comp),
    ?assertMatch([], mongoose_component:lookup_component(Dom)),
    ?assertMatch([], mongoose_component:lookup_component(Dom, ThisNode)),
    ?assertMatch([], mongoose_component:lookup_component(Dom, AnotherNode)),
    %% we can register from both nodes
    {ok, Comps2} = mongoose_component:register_component(Dom, ThisNode, Handler, false, false),
    %% passing node here is only for testing
    {ok, _Comps3} = mongoose_component:register_component(Dom, AnotherNode, Handler, false, false),
    %% both are reachable locally
    ?assertMatch([#external_component{node = ThisNode}],
                 mongoose_component:lookup_component(Dom, ThisNode)),
    ?assertMatch([#external_component{node = AnotherNode}],
                 mongoose_component:lookup_component(Dom, AnotherNode)),
    %% if we try global lookup we get two handlers
    ?assertMatch([_, _], mongoose_component:lookup_component(Dom)),
    %% we unregister one and the result is:
    mongoose_component:unregister_component(Comps2),
    ?assertMatch([], mongoose_component:lookup_component(Dom, ThisNode)),
    ?assertMatch([#external_component{node = AnotherNode}],
                 mongoose_component:lookup_component(Dom)),
    ?assertMatch([#external_component{node = AnotherNode}],
                 mongoose_component:lookup_component(Dom, AnotherNode)),
    ok.

process_packet(_From, _To, _Packet, _Extra) ->
    exit(process_packet_called).
