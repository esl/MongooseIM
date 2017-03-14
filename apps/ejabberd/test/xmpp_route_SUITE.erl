-module(xmpp_route_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    [ basic_routing ].

init_per_suite(C) ->
    ok = stringprep:start(),
    application:ensure_all_started(lager),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option,
        fun(routing_modules) ->
            [xmpp_router_a, xmpp_router_b, xmpp_router_c];
           (_) ->
            undefined
        end),
    {ok, _} = application:ensure_all_started(exometer),
    ejabberd_router:start_link(),
    C.

end_per_suite(_C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    application:stop(exometer),
    application:stop(exometer_core),
    ok.

basic_routing(_C) ->
    %% module 'a' drops message 1, routes message 2, passes on everything else
    setup_routing_module(xmpp_router_a, 1, 2),
    %% module 'b' drops message 3, routes message 4, passes on everything else
    setup_routing_module(xmpp_router_b, 3, 4),
    %% module 'c' routes everything
    setup_routing_module(xmpp_router_c, none, all),
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

setup_routing_module(Name, PacketToDrop, PacketToRoute) ->
    meck:new(Name, [non_strict]),
    meck:expect(Name, filter,
        fun(From, To, Acc) ->
            Packet = mongoose_acc:get(element, Acc),
            case Packet of
                PacketToDrop -> drop;
                _ -> {From, To, Acc}
            end
        end),
    meck:expect(Name, route,
        make_routing_fun(Name, PacketToRoute)),
    ok.

make_routing_fun(Name, all) ->
    Self = self(),
    Marker = list_to_atom([lists:last(atom_to_list(Name))]),
    fun(_From, _To, Acc) ->
        Packet = mongoose_acc:get(element, Acc),
        Self ! {Marker, Packet},
        done
    end;
make_routing_fun(Name, PacketToRoute) ->
    Self = self(),
    Marker = list_to_atom([lists:last(atom_to_list(Name))]),
    fun(From, To, Acc) ->
        Packet = mongoose_acc:get(element, Acc),
        case Packet of
            PacketToRoute ->
                Self ! {Marker, Packet},
                done;
            _ -> {From, To, Acc}
        end
    end.

route(I) ->
    Acc = mongoose_acc:from_kv(element, I),
    #{} = ejabberd_router:route(jid:from_binary(<<"ala@localhost">>),
                               jid:from_binary(<<"bob@localhost">>),
                               Acc).

verify(L) ->
    receive
        X ->
            ct:pal("{X, L}: ~p", [{X, L}]),
            ?assert(lists:member(X, L)),
            verify(lists:delete(X, L))
    after 1000 ->
        ?assertEqual(L, []),
        ct:pal("all messages routed correctly")
    end.

