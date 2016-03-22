-module(xmpp_route_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    [ basic_routing ].

init_per_suite(C) ->
    application:start(stringprep),
    application:ensure_all_started(lager),
    mnesia:start(),
    mnesia:create_schema([node()]),
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option,
        fun(routing_modules) ->
            [xmpp_router_a, xmpp_router_b, xmpp_router_c]
        end),
    ejabberd_router:start_link(),
    C.

end_per_suite(_C) ->
    ok.

basic_routing(_C) ->
    %% module 'a' drops message 1, routes message 2, passes on everything else
    Self = self(),
    meck:new(xmpp_router_a, [non_strict]),
    meck:expect(xmpp_router_a, filter,
                fun(From, To, Packet) ->
                    case Packet of
                        1 -> drop;
                        _ -> {From, To, Packet}
                    end
                end),
    meck:expect(xmpp_router_a, route,
        fun(From, To, Packet) ->
            case Packet of
                2 ->
                    Self ! {a, Packet},
                    done;
                _ -> {From, To, Packet}
            end
        end),
    %% module 'b' drops message 3, routes message 4, passes on everything else
    meck:new(xmpp_router_b, [non_strict]),
    meck:expect(xmpp_router_b, filter,
        fun(From, To, Packet) ->
            case Packet of
                3 -> drop;
                _ -> {From, To, Packet}
            end
        end),
    meck:expect(xmpp_router_b, route,
        fun(From, To, Packet) ->
            case Packet of
                4 ->
                    Self ! {b, Packet},
                    done;
                _ -> {From, To, Packet}
            end
        end),
    %% module 'c' routes everything
    meck:new(xmpp_router_c, [non_strict]),
    meck:expect(xmpp_router_c, filter,
        fun(From, To, Packet) ->
            {From, To, Packet}
        end),
    meck:expect(xmpp_router_c, route,
        fun(_From, _To, Packet) ->
            Self ! {c, Packet},
            done
        end),
    %% send messages from 1 to 5
    lists:map(fun(I) -> route(I) end, [1,2,3,4,5]),
    meck:validate(xmpp_router_a),
    meck:unload(xmpp_router_a),
    meck:validate(xmpp_router_b),
    meck:unload(xmpp_router_b),
    meck:validate(xmpp_router_c),
    meck:unload(xmpp_router_c),
    %% we know that 1 and 3 should be dropped, and 2, 4 and 5 handled by a, b and c respectively
    verify([{a, 2}, {b, 4}, {c, 5}]),
    ok.

route(I) ->
    ok = ejabberd_router:route(jid:from_binary(<<"ala@localhost">>),
        jid:from_binary(<<"bob@localhost">>),
        I).

verify(L) ->
    receive
        X ->
            ?assert(lists:member(X, L)),
            verify(lists:delete(X, L))
    after 1000 ->
        ?assertEqual(L, []),
        ct:pal("all messages routed correctly")
    end.

