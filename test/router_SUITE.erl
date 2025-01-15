-module(router_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("mongoose.hrl").

%% ---------------------------------------------------------------
%% Common Test callbacks
%% ---------------------------------------------------------------

all() ->
    [
     {group, routing}
    ].

groups() ->
    [
     {routing, [], [
                    basic_routing,
                    do_not_reroute_errors,
                    routing_error_out_of_modules
                   ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mongoose_config:set_opts(opts()),
    async_helper:start(Config, [{mongoose_instrument, start_link, []},
                                {mongooseim_helper, start_link_loaded_hooks, []},
                                {mongoose_router, start, []},
                                {ejabberd_router, start_link, []}]).

end_per_suite(Config) ->
    meck:unload(),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    async_helper:stop_all(Config),
    mongoose_config:erase_opts().

%% ---------------------------------------------------------------
%% Test cases
%% ---------------------------------------------------------------

basic_routing(_C) ->
    %% module 'a' drops message 1, routes message 2, passes on everything else
    setup_routing_module(xmpp_router_a, 1, 2),
    %% module 'b' drops message 3, routes message 4, passes on everything else
    setup_routing_module(xmpp_router_b, 3, 4),
    %% module 'c' routes everything
    setup_routing_module(xmpp_router_c, none, all),
    %% send messages from 1 to 5
    lists:map(fun(I) -> #{} = route(msg(I)) end, [1,2,3,4,5]),
    meck:validate(xmpp_router_a),
    meck:unload(xmpp_router_a),
    meck:validate(xmpp_router_b),
    meck:unload(xmpp_router_b),
    meck:validate(xmpp_router_c),
    meck:unload(xmpp_router_c),
    %% we know that 1 and 3 should be dropped, and 2, 4 and 5 handled by a, b and c respectively
    verify([{a, 2}, {b, 4}, {c, 5}]),
    ok.

%% This test makes sure that if we try to respond to an error message by routing error message
%% we do not enter an infinite loop; it has been fixed in d3941e33453c95ca78561144182712cc4f1d6c72
%% without the fix this tests gets stuck in a loop.
do_not_reroute_errors(_) ->
    From = <<"ja@localhost">>,
    To = <<"ty@localhost">>,
    Stanza = #xmlel{name = <<"iq">>,
                   attrs = #{<<"from">> => From,
                             <<"to">> => To,
                             <<"type">> => <<"get">>}
    },
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              host_type => <<"localhost">>,
                              element => Stanza }),
    meck:new(xmpp_router_a, [non_strict]),
    meck:expect(xmpp_router_a, filter,
                fun(From0, To0, Acc0, Packet0) -> {From0, To0, Acc0, Packet0} end),
    meck:expect(xmpp_router_a, route, fun resend_as_error/4),
    ejabberd_router:route(From, To, Acc, Stanza),
    meck:validate(xmpp_router_a),
    meck:unload(xmpp_router_a).

routing_error_out_of_modules(_C) ->
    %% all modules don't route anything for this test
    setup_routing_module(xmpp_router_a, none, 2),
    setup_routing_module(xmpp_router_b, none, 2),
    setup_routing_module(xmpp_router_c, none, 2),

    %% router should return an error
    Res = route(msg(1)),
    [{error, out_of_modules}] = mongoose_acc:get(router, result, Res).


%% ---------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------

setup_routing_module(Name, PacketToDrop, PacketToRoute) ->
    meck:new(Name, [non_strict]),
    meck:expect(Name, filter,
        fun(From, To, Acc, Packet) ->
            case msg_to_id(Packet) of
                PacketToDrop -> drop;
                _ -> {From, To, Acc, Packet}
            end
        end),
    meck:expect(Name, route,
        make_routing_fun(Name, PacketToRoute)),
    ok.

make_routing_fun(Name, all) ->
    Self = self(),
    Marker = list_to_atom([lists:last(atom_to_list(Name))]),
    fun(_From, _To, Acc, Packet) ->
        Self ! {Marker, Packet},
        {done, Acc}
    end;
make_routing_fun(Name, PacketToRoute) ->
    Self = self(),
    Marker = list_to_atom([lists:last(atom_to_list(Name))]),
    fun(From, To, Acc, Packet) ->
        case msg_to_id(Packet) of
            PacketToRoute ->
                Self ! {Marker, Packet},
                {done, Acc};
            _ -> {From, To, Acc, Packet}
        end
    end.

msg(I) ->
    IBin = integer_to_binary(I),
    #xmlel{ name = <<"message">>,
            children = [
                        #xmlel{ name = <<"body">>,
                                children = [#xmlcdata{ content = IBin }] }
                       ] }.

msg_to_id(Msg) ->
    binary_to_integer(exml_query:path(Msg, [{element, <<"body">>}, cdata])).

route(I) ->
    FromJID = jid:from_binary(<<"ala@localhost">>),
    ToJID = jid:from_binary(<<"bob@localhost">>),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              host_type => <<"localhost">>,
                              element => I,
                              from_jid => FromJID,
                              to_jid => ToJID }),
    ejabberd_router:route(FromJID, ToJID, Acc, I).

verify(L) ->
    receive
        {RouterID, XML} ->
            X = msg_to_id(XML),
            ct:pal("{RouterID, X, L}: ~p", [{RouterID, X, L}]),
            Item = {RouterID, X},
            ?assert(lists:member(Item, L)),
            verify(lists:delete(Item, L))
    after 1000 ->
        ?assertEqual(L, []),
        ct:pal("all messages routed correctly")
    end.

resend_as_error(From0, To0, Acc0, Packet0) ->
    {Acc1, Packet1} = jlib:make_error_reply(Acc0, Packet0, #xmlel{}),
    Acc2 = ejabberd_router:route(To0, From0, Acc1, Packet1),
    {done, Acc2}.

opts() ->
    RoutingModules = [xmpp_router_a, xmpp_router_b, xmpp_router_c],
    #{hosts => [<<"localhost">>],
      host_types => [],
      instrumentation => config_parser_helper:default_config([instrumentation]),
      component_backend => mnesia,
      routing_modules => xmpp_router:expand_routing_modules(RoutingModules)}.
