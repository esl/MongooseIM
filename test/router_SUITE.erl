-module(router_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("mongoose.hrl").

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
                    basic_routing,
                    do_not_reroute_errors
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
    lists:map(fun(I) -> route(msg(I)) end, [1,2,3,4,5]),
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
        attrs = [{<<"from">>, From}, {<<"to">>, To}, {<<"type">>, <<"get">>} ]
    },
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              element => Stanza }),
    meck:new(xmpp_router_a, [non_strict]),
    meck:expect(xmpp_router_a, filter,
                fun(From0, To0, Acc0, Packet0) -> {From0, To0, Acc0, Packet0} end),
    meck:expect(xmpp_router_a, route, fun resend_as_error/4),
    ejabberd_router:route(From, To, Acc, Stanza),
    ok.
     
update_tables_hidden_components(_C) ->
    %% Tables as of b076e4a62a8b21188245f13c42f9cfd93e06e6b7
    create_component_tables([domain, handler, node]),

    ejabberd_router:update_tables(),

    %% Local table is removed and the distributed one has a new list of attributes
    false = lists:member(external_component, mnesia:system_info(tables)),
    [domain, handler, node, is_hidden] = mnesia:table_info(external_component_global, attributes).

update_tables_hidden_components_idempotent(_C) ->
    AttrsWithHidden = [domain, handler, node, is_hidden],
    create_component_tables(AttrsWithHidden),

    ejabberd_router:update_tables(),

    %% Local table is not removed and the attribute list of the distributed one is not changed
    true = lists:member(external_component, mnesia:system_info(tables)),
    AttrsWithHidden = mnesia:table_info(external_component_global, attributes).

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
    fun(_From, _To, _Acc, Packet) ->
        Self ! {Marker, Packet},
        done
    end;
make_routing_fun(Name, PacketToRoute) ->
    Self = self(),
    Marker = list_to_atom([lists:last(atom_to_list(Name))]),
    fun(From, To, Acc, Packet) ->
        case msg_to_id(Packet) of
            PacketToRoute ->
                Self ! {Marker, Packet},
                done;
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
                              element => I,
                              from_jid => FromJID,
                              to_jid => ToJID }),
    #{} = ejabberd_router:route(FromJID, ToJID, Acc, I).

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

resend_as_error(From0, To0, Acc0, Packet0) ->
    {Acc1, Packet1} = jlib:make_error_reply(Acc0, Packet0, #xmlel{}),
    ejabberd_router:route(To0, From0, Acc1, Packet1),
    done.
