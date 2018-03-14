-module(router_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------
%% Common Test callbacks
%% ---------------------------------------------------------------

all() ->
    [
     {group, routing},
     {group, schema}
    ].

groups() ->
    [
     {routing, [], [
                    basic_routing
                   ]},
     {schema, [], [
                   update_tables_hidden_components,
                   update_tables_hidden_components_idempotent
                  ]}
    ].

init_per_suite(C) ->
    ok = stringprep:start(),
    application:ensure_all_started(lager),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(), 
    {ok, _} = application:ensure_all_started(exometer_core),
    C.

end_per_suite(_C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    application:stop(exometer_core),
    ok.

init_per_group(routing, Config) ->
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option,
        fun(routing_modules) ->
            [xmpp_router_a, xmpp_router_b, xmpp_router_c];
           (_) ->
            undefined
        end),
    ejabberd_hooks:start_link(),
    ejabberd_router:start_link(),
    Config;
init_per_group(schema, Config) ->
    remove_component_tables(),
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(HiddenComponent, _Config)
  when HiddenComponent == update_tables_hidden_components;
       HiddenComponent == update_tables_hidden_components_idempotent ->
    remove_component_tables();
end_per_testcase(_CaseName, _Config) ->
    ok.

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
     
update_tables_hidden_components(_C) ->
    %% Tables as of b076e4a62a8b21188245f13c42f9cfd93e06e6b7
    create_component_tables([domain, handler, node]),

    ejabberd_router:update_tables(),

    %% Local table is removed and distributed one has new list of attributes
    false = lists:member(external_component, mnesia:system_info(tables)),
    [domain, handler, node, is_hidden] = mnesia:table_info(external_component_global, attributes).

update_tables_hidden_components_idempotent(_C) ->
    AttrsWithHidden = [domain, handler, node, is_hidden],
    create_component_tables(AttrsWithHidden),

    ejabberd_router:update_tables(),

    %% Local table is not removed and attribute list of distributed one is not changed
    true = lists:member(external_component, mnesia:system_info(tables)),
    AttrsWithHidden = mnesia:table_info(external_component_global, attributes).

%% ---------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------

setup_routing_module(Name, PacketToDrop, PacketToRoute) ->
    meck:new(Name, [non_strict]),
    meck:expect(Name, filter,
        fun(From, To, Acc, Packet) ->
            case Packet of
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
    fun(_From, _To, _Acc, Packet) ->
        Self ! {Marker, Packet},
        done
    end;
make_routing_fun(Name, PacketToRoute) ->
    Self = self(),
    Marker = list_to_atom([lists:last(atom_to_list(Name))]),
    fun(From, To, Acc, Packet) ->
        case Packet of
            PacketToRoute ->
                Self ! {Marker, Packet},
                done;
            _ -> {From, To, Acc, Packet}
        end
    end.

route(I) ->
    Acc = mongoose_acc:from_kv(element, I),
    #{} = ejabberd_router:route(jid:from_binary(<<"ala@localhost">>),
                               jid:from_binary(<<"bob@localhost">>),
                               Acc, I).

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

create_component_tables(AttrList) ->
    {atomic, ok} =
    mnesia:create_table(external_component,
                        [{attributes, AttrList},
                         {local_content, true}]),
    {atomic, ok} =
    mnesia:create_table(external_component_global,
                        [{attributes, AttrList},
                         {type, bag},
                         {record_name, external_component}]).

remove_component_tables() ->
    mnesia:delete_table(external_component),
    mnesia:delete_table(external_component_global).

