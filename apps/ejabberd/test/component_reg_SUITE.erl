-module(component_reg_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/ejabberd.hrl").


all() ->
    [ registering, registering_with_local ].

init_per_suite(C) ->
    application:ensure_all_started(stringprep),
    application:ensure_all_started(lager),
    mnesia:start(),
    mnesia:create_schema([node()]),
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option,
        fun(routing_modules) ->
            [xmpp_router_a, xmpp_router_b, xmpp_router_c]
        end),
    application:ensure_all_started(exometer),
    ejabberd_router:start_link(),
    C.

end_per_suite(_C) ->
    ok.

registering(_C) ->
    Dom = <<"aaa.bbb.com">>,
    ejabberd_router:register_component(Dom),
    Lookup = ejabberd_router:lookup_component(Dom),
    ?assertMatch([#external_component{}], Lookup),
    ejabberd_router:unregister_component(Dom),
    ?assertMatch([], ejabberd_router:lookup_component(Dom)),
    ok.

registering_with_local(_C) ->
    Dom = <<"aaa.bbb.com">>,
    ThisNode = node(),
    AnotherNode = 'another@nohost',
    ejabberd_router:register_component(Dom),
    %% we can find it globally
    ?assertMatch([#external_component{node = ThisNode}], ejabberd_router:lookup_component(Dom)),
    %% and for this node
    ?assertMatch([#external_component{node = ThisNode}], ejabberd_router:lookup_component(Dom, ThisNode)),
    %% but not for another node
    ?assertMatch([], ejabberd_router:lookup_component(Dom, AnotherNode)),
    %% once we unregister it is not available
    ejabberd_router:unregister_component(Dom),
    ?assertMatch([], ejabberd_router:lookup_component(Dom)),
    ?assertMatch([], ejabberd_router:lookup_component(Dom, ThisNode)),
    ?assertMatch([], ejabberd_router:lookup_component(Dom, AnotherNode)),
    %% we can register from both nodes
    ejabberd_router:register_component(Dom, ThisNode),
    ejabberd_router:register_component(Dom, AnotherNode), %% passing node here is only for testing
    %% both are reachable locally
    ?assertMatch([#external_component{node = ThisNode}], ejabberd_router:lookup_component(Dom, ThisNode)),
    ?assertMatch([#external_component{node = AnotherNode}], ejabberd_router:lookup_component(Dom, AnotherNode)),
    %% if we try global lookup we get two handlers
    ?assertMatch([_, _], ejabberd_router:lookup_component(Dom)),
    %% we unregister one and the result is:
    ejabberd_router:unregister_component(Dom),
    ?assertMatch([], ejabberd_router:lookup_component(Dom, ThisNode)),
    ?assertMatch([#external_component{node = AnotherNode}], ejabberd_router:lookup_component(Dom)),
    ?assertMatch([#external_component{node = AnotherNode}], ejabberd_router:lookup_component(Dom, AnotherNode)),
    ok.


