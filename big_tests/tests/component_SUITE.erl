%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(component_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(reload_helper, [backup_ejabberd_config_file/2,
                        restore_ejabberd_config_file/2,
                        reload_through_ctl/2,
                        restart_ejabberd_node/1]).

-import(distributed_helper, [add_node_to_cluster/1,
                             mim/0,
                             remove_node_from_cluster/1,
                             require_rpc_nodes/1,
                             start_node/2,
                             stop_node/2]).

-import(component_helper, [connect_component/1,
                           connect_component/2,
                           disconnect_component/2,
                           disconnect_components/2,
                           connect_component_subdomain/1,
                           spec/2,
                           common/1,
                           common/2,
                           name/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, xep0114_tcp},
     {group, xep0114_ws},
     {group, subdomain},
     {group, hidden_components},
     {group, distributed}
    ].

groups() ->
    G = [{xep0114_tcp, [], xep0114_tests()},
         {xep0114_ws, [], xep0114_tests()},
         {subdomain, [], [register_subdomain]},
         {hidden_components, [], [disco_with_hidden_component]},
         {distributed, [], [register_in_cluster,
                            register_same_on_both
                            %clear_on_node_down TODO: Breaks cover
                           ]}],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

xep0114_tests() ->
    [register_one_component,
     dirty_disconnect,
     register_two_components,
     try_registering_with_wrong_password,
     try_registering_component_twice,
     try_registering_existing_host,
     disco_components,
     kick_old_component_on_conflict
     ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    ejabberd_node_utils:init(Config1).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(xep0114_tcp, Config) ->
    Config1 = get_components(common(Config), Config),
    escalus:create_users(Config1, escalus:get_users([alice, bob]));
init_per_group(xep0114_ws, Config) ->
    WSOpts = [{transport, escalus_ws},
              {wspath, <<"/ws-xmpp">>},
              {wslegacy, true} | common(Config, ct:get_config({hosts, mim, cowboy_port}))],
    Config1 = get_components(WSOpts, Config),
    escalus:create_users(Config1, escalus:get_users([alice, bob]));
init_per_group(subdomain, Config) ->
    Config1 = get_components(common(Config), Config),
    add_domain(Config1),
    escalus:create_users(Config1, escalus:get_users([alice, astrid]));
init_per_group(hidden_components, Config) ->
    Config1 = get_components(common(Config), Config),
    escalus:create_users(Config1, escalus:get_users([alice, bob]));
init_per_group(distributed, Config) ->
    Config1 = get_components(common(Config), Config),
    Config2 = add_node_to_cluster(Config1),
    escalus:create_users(Config2, escalus:get_users([alice, clusterguy]));
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(subdomain, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, astrid])),
    restore_domain(Config);
end_per_group(distributed, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, clusterguy])),
    remove_node_from_cluster(Config);
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
dirty_disconnect(Config) ->
    %% Given one connected component, kill the connection and reconnect
    CompOpts = ?config(component1, Config),
    {Component, Addr, _} = connect_component(CompOpts),
    disconnect_component(Component, Addr),
    {Component1, Addr, _} = connect_component(CompOpts),
    disconnect_component(Component1, Addr).

register_one_component(Config) ->
    %% Given one connected component
    CompOpts = ?config(component1, Config),
    {Component, ComponentAddr, _} = connect_component(CompOpts),
    verify_component(Config, Component, ComponentAddr),
    disconnect_component(Component, ComponentAddr).

verify_component(Config, Component, ComponentAddr) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                %% When Alice sends a message to the component
                Msg1 = escalus_stanza:chat_to(ComponentAddr, <<"Hi!">>),
                escalus:send(Alice, Msg1),
                %% Then component receives it
                Reply1 = escalus:wait_for_stanza(Component),
                escalus:assert(is_chat_message, [<<"Hi!">>], Reply1),

                %% When component sends a reply
                Msg2 = escalus_stanza:chat_to(Alice, <<"Oh hi!">>),
                escalus:send(Component, escalus_stanza:from(Msg2, ComponentAddr)),

                %% Then Alice receives it
                Reply2 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_chat_message, [<<"Oh hi!">>], Reply2),
                escalus:assert(is_stanza_from, [ComponentAddr], Reply2)
        end).

register_two_components(Config) ->
    %% Given two connected components
    CompOpts1 = ?config(component1, Config),
    CompOpts2 = ?config(component2, Config),
    {Comp1, CompAddr1, _} = connect_component(CompOpts1),
    {Comp2, CompAddr2, _} = connect_component(CompOpts2),

    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
                %% When Alice sends a message to the first component
                Msg1 = escalus_stanza:chat_to(Alice, <<"abc">>),
                escalus:send(Comp1, escalus_stanza:from(Msg1, CompAddr1)),
                %% Then component receives it
                Reply1 = escalus:wait_for_stanza(Alice),
                escalus:assert(is_chat_message, [<<"abc">>], Reply1),
                escalus:assert(is_stanza_from, [CompAddr1], Reply1),

                %% When Bob sends a message to the second component
                Msg2 = escalus_stanza:chat_to(Bob, <<"def">>),
                escalus:send(Comp2, escalus_stanza:from(Msg2, CompAddr2)),
                %% Then it also receives it
                Reply2 = escalus:wait_for_stanza(Bob),
                escalus:assert(is_chat_message, [<<"def">>], Reply2),
                escalus:assert(is_stanza_from, [CompAddr2], Reply2),

                %% When the second component sends a reply to Bob
                Msg3 = escalus_stanza:chat_to(CompAddr2, <<"ghi">>),
                escalus:send(Bob, Msg3),
                %% Then he receives it
                Reply3 = escalus:wait_for_stanza(Comp2),
                escalus:assert(is_chat_message, [<<"ghi">>], Reply3),

                %% WHen the first component sends a reply to Alice
                Msg4 = escalus_stanza:chat_to(CompAddr1, <<"jkl">>),
                escalus:send(Alice, Msg4),
                %% Then she receives it
                Reply4 = escalus:wait_for_stanza(Comp1),
                escalus:assert(is_chat_message, [<<"jkl">>], Reply4)
        end),

    disconnect_component(Comp1, CompAddr1),
    disconnect_component(Comp2, CompAddr2).

try_registering_with_wrong_password(Config) ->
    %% Given a component with a wrong password
    CompOpts1 = ?config(component1, Config),
    CompOpts2 = lists:keyreplace(password, 1, CompOpts1,
                                 {password, <<"wrong_one">>}),
    try
        %% When trying to connect it
        {Comp, Addr, _} = connect_component(CompOpts2),
        disconnect_component(Comp, Addr),
        ct:fail("component connected successfully with wrong password")
    catch {stream_error, _E} ->
        %% Then it should fail to do so
        ok
    end.

try_registering_component_twice(Config) ->
    %% Given two components with the same name
    CompOpts1 = ?config(component1, Config),
    {Comp1, Addr, _} = connect_component(CompOpts1),

    try
        %% When trying to connect the second one
        {Comp2, Addr, _} = connect_component(CompOpts1),
        disconnect_component(Comp2, Addr),
        ct:fail("second component connected successfully")
    catch {stream_error, _} ->
        %% Then it should fail to do so
        ok
    end,

    disconnect_component(Comp1, Addr).

try_registering_existing_host(Config) ->
    %% Given a external vjud component
    Component = ?config(vjud_component, Config),

    try
        %% When trying to connect it to the server
        {Comp, Addr, _} = connect_component(Component),
        disconnect_component(Comp, Addr),
        ct:fail("vjud component connected successfully")
    catch {stream_error, _} ->
        %% Then it should fail since vjud service already exists on the server
        ok
    end.

%% When conflict_behaviour is kick_old, then:
%% - stop old connections by sending stream:error with reason "conflict"
kick_old_component_on_conflict(Config) ->
    CompOpts1 = spec(kicking_component, Config),
    {Comp1, Addr, _} = connect_component(CompOpts1),

    %% When trying to connect the second one
    {Comp2, Addr, _} = connect_component(CompOpts1),

    %% First connection is disconnected
    Stanza = escalus:wait_for_stanza(Comp1),
    escalus:assert(is_stream_error, [<<"conflict">>, <<"">>], Stanza),

    %% New connection is usable
    verify_component(Config, Comp2, Addr),

    disconnect_component(Comp2, Addr).

disco_components(Config) ->
    %% Given two connected components
    CompOpts1 = ?config(component1, Config),
    CompOpts2 = ?config(component2, Config),
    {Comp1, Addr1, _} = connect_component(CompOpts1),
    {Comp2, Addr2, _} = connect_component(CompOpts2),

    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                %% When server asked for the disco features
                Server = escalus_client:server(Alice),
                Disco = escalus_stanza:service_discovery(Server),
                escalus:send(Alice, Disco),

                %% Then it contains hosts of 2 components
                DiscoReply = escalus:wait_for_stanza(Alice),
                escalus:assert(has_service, [Addr1], DiscoReply),
                escalus:assert(has_service, [Addr2], DiscoReply)
        end),

    disconnect_component(Comp1, Addr1),
    disconnect_component(Comp2, Addr2).

%% Verifies that a component connected to the "hidden components" endpoint
%% is not discoverable.
%% Assumes mod_disco with `{users_can_see_hidden_services, false}` option
disco_with_hidden_component(Config) ->
    %% Given two connected components
    CompOpts1 = ?config(component1, Config),
    HCompOpts = spec(hidden_component, Config),
    {Comp1, Addr1, _} = connect_component(CompOpts1),
    {HComp, HAddr, _} = connect_component(HCompOpts),

    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                %% When server asked for the disco features
                Server = escalus_client:server(Alice),
                Disco = escalus_stanza:service_discovery(Server),
                escalus:send(Alice, Disco),

                %% Then it contains hosts of component1 and hidden_component is missing
                DiscoReply = escalus:wait_for_stanza(Alice),
                escalus:assert(has_service, [Addr1], DiscoReply),
                escalus:assert(fun(Stanza) ->
                                       not escalus_pred:has_service(HAddr, Stanza)
                               end, DiscoReply)
        end),

    disconnect_component(Comp1, Addr1),
    disconnect_component(HComp, HAddr).

register_subdomain(Config) ->
    %% Given one connected component
    CompOpts1 = ?config(component1, Config),
    {Comp, Addr, Name} = connect_component_subdomain(CompOpts1),

    escalus:story(Config, [{alice, 1}, {astrid, 1}], fun(Alice, Astrid) ->
                %% When Alice asks for service discovery on the server
                Server1 = escalus_client:server(Alice),
                Disco1 = escalus_stanza:service_discovery(Server1),
                escalus:send(Alice, Disco1),

                %% Then it contains the registered route
                DiscoReply1 = escalus:wait_for_stanza(Alice),
                ComponentHost1 = <<Name/binary, ".", Server1/binary>>,
                escalus:assert(has_service, [ComponentHost1], DiscoReply1),

                %% When Astrid ask for service discovery on her server
                Server2 = escalus_client:server(Astrid),
                false = (Server1 =:= Server2),
                Disco2 = escalus_stanza:service_discovery(Server2),
                escalus:send(Astrid, Disco2),

                %% Then it also contains the registered route
                DiscoReply2 = escalus:wait_for_stanza(Astrid),
                ComponentHost2 = <<Name/binary, ".", Server2/binary>>,
                escalus:assert(has_service, [ComponentHost2], DiscoReply2)

        end),

    disconnect_component(Comp, Addr).


register_in_cluster(Config) ->
    %% Given one component connected to the cluster
    CompOpts1 = ?config(component1, Config),
    Component1 = connect_component(CompOpts1),
    {Comp1, Addr1, _} = Component1,
    CompOpts2 = ?config(component2, Config),
    Component2 = connect_component(CompOpts2),
    {Comp2, Addr2, _} = Component2,
    CompOpts_on_2 = spec(component_on_2, Config),
    Component_on_2 = connect_component(CompOpts_on_2),
    {Comp_on_2, Addr_on_2, _} = Component_on_2,

    escalus:story(Config, [{alice, 1}, {clusterguy, 1}], fun(Alice, ClusterGuy) ->
                do_chat_with_component(Alice, ClusterGuy, Component1),
                do_chat_with_component(Alice, ClusterGuy, Component2),
                do_chat_with_component(Alice, ClusterGuy, Component_on_2)
        end),

    disconnect_component(Comp1, Addr1),
    disconnect_component(Comp2, Addr2),
    disconnect_component(Comp_on_2, Addr_on_2),
    ok.

clear_on_node_down(Config) ->
    CompOpts = ?config(component1, Config),
    ?assertMatch({_, _, _}, connect_component(CompOpts)),
    ?assertThrow({stream_error, _}, connect_component(CompOpts)),

    stop_node(mim(), Config),
    start_node(mim(), Config),

    {Comp, Addr, _} = connect_component(CompOpts),
    disconnect_component(Comp, Addr).

do_chat_with_component(Alice, ClusterGuy, Component1) ->
    {Comp, Addr, Name} = Component1,

    %% When Alice sends a message to the component
    Msg1 = escalus_stanza:chat_to(Addr, <<"Hi!">>),
    escalus:send(Alice, Msg1),
    %% Then component receives it
    Reply1 = escalus:wait_for_stanza(Comp),
    escalus:assert(is_chat_message, [<<"Hi!">>], Reply1),

    %% When components sends a reply
    Msg2 = escalus_stanza:chat_to(Alice, <<"Oh hi!">>),
    escalus:send(Comp, escalus_stanza:from(Msg2, Addr)),

    %% Then Alice receives it
    Reply2 = escalus:wait_for_stanza(Alice),
    escalus:assert(is_chat_message, [<<"Oh hi!">>], Reply2),
    escalus:assert(is_stanza_from, [Addr], Reply2),

    %% When ClusterGuy (connected to the other node than component)
    %% sends a message
    Msg3 = escalus_stanza:chat_to(Addr, <<"Hello!">>),
    escalus:send(ClusterGuy, Msg3),
    %% Then component receives it
    Reply3 = escalus:wait_for_stanza(Comp),
    escalus:assert(is_chat_message, [<<"Hello!">>], Reply3),

    %% When components sends a reply
    Msg4 = escalus_stanza:chat_to(ClusterGuy, <<"Hola!">>),
    escalus:send(Comp, escalus_stanza:from(Msg4, Addr)),

    %% Then ClusterGuy receives it
    Reply4 = escalus:wait_for_stanza(ClusterGuy),
    escalus:assert(is_chat_message, [<<"Hola!">>], Reply4),
    escalus:assert(is_stanza_from, [Addr], Reply4),

    %% When Alice asks for the disco features
    Server1 = escalus_client:server(Alice),
    Disco1 = escalus_stanza:service_discovery(Server1),
    escalus:send(Alice, Disco1),

    %% Then it contains host of the service
    DiscoReply1 = escalus:wait_for_stanza(Alice),
    escalus:assert(has_service, [Addr], DiscoReply1),

    %% When ClusterGuy asks for the disco features on her server
    Server2 = escalus_client:server(ClusterGuy),
    Disco2 = escalus_stanza:service_discovery(Server2),
    escalus:send(ClusterGuy, Disco2),

    %% Then it also contains the service (with the other address though)
    DiscoReply2 = escalus:wait_for_stanza(ClusterGuy),
    DistributedAddr = <<Name/binary, ".", Server2/binary>>,
    escalus:assert(has_service, [DistributedAddr], DiscoReply2).


register_same_on_both(Config) ->
    %% Given two components with the same name
    %% but not on the same host
    %% we should be able to register
    %% and we get two components having the same name and address
    CompOpts2 = ?config(component2, Config),
    Component2 = connect_component(CompOpts2),
    {Comp2, Addr, Name} = Component2,
    CompOpts_d = spec(component_duplicate, Config),
    Component_d = connect_component(CompOpts_d),
    {Comp_d, Addr, Name} = Component_d,

    escalus:story(Config, [{alice, 1}, {clusterguy, 1}], fun(Alice, ClusterGuy) ->
        %% When Alice sends a message to the component
        Msg1 = escalus_stanza:chat_to(Addr, <<"Hi!">>),
        escalus:send(Alice, Msg1),
        %% Then component receives it (on the same node)
        Reply1 = escalus:wait_for_stanza(Comp2),
        escalus:assert(is_chat_message, [<<"Hi!">>], Reply1),

        %% When components sends a reply
        Msg2 = escalus_stanza:chat_to(Alice, <<"Oh hi!">>),
        escalus:send(Comp2, escalus_stanza:from(Msg2, Addr)),

        %% Then Alice receives it
        Reply2 = escalus:wait_for_stanza(Alice),
        escalus:assert(is_chat_message, [<<"Oh hi!">>], Reply2),
        escalus:assert(is_stanza_from, [Addr], Reply2),

        %% When ClusterGuy (connected to the other node than component)
        %% sends a message
        Msg3 = escalus_stanza:chat_to(Addr, <<"Hello!">>),
        escalus:send(ClusterGuy, Msg3),
        %% Then component on his node receives it
        Reply3 = escalus:wait_for_stanza(Comp_d),
        escalus:assert(is_chat_message, [<<"Hello!">>], Reply3),

        %% When components sends a reply
        Msg4 = escalus_stanza:chat_to(ClusterGuy, <<"Hola!">>),
        escalus:send(Comp_d, escalus_stanza:from(Msg4, Addr)),

        %% Then ClusterGuy receives it
        Reply4 = escalus:wait_for_stanza(ClusterGuy),
        escalus:assert(is_chat_message, [<<"Hola!">>], Reply4),
        escalus:assert(is_stanza_from, [Addr], Reply4),

        %% When Alice asks for the disco features
        Server1 = escalus_client:server(Alice),
        Disco1 = escalus_stanza:service_discovery(Server1),
        escalus:send(Alice, Disco1),

        %% Then it contains host of the service
        DiscoReply1 = escalus:wait_for_stanza(Alice),
        escalus:assert(has_service, [Addr], DiscoReply1),

        %% When ClusterGuy asks for the disco features on her server
        Server2 = escalus_client:server(ClusterGuy),
        Disco2 = escalus_stanza:service_discovery(Server2),
        escalus:send(ClusterGuy, Disco2),

        %% Then it also contains the same service
        DiscoReply2 = escalus:wait_for_stanza(ClusterGuy),
        escalus:assert(has_service, [Addr], DiscoReply2)

    end),
    disconnect_components([Comp2, Comp_d], Addr),
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_components(Opts, Config) ->
    Components = [component1, component2, vjud_component],
    [ {C, Opts ++ spec(C, Config)} || C <- Components ] ++ Config.

add_domain(Config) ->
    Node = default_node(Config),
    Hosts = {hosts, "[\"localhost\", \"sogndal\"]"},
    backup_ejabberd_config_file(Node, Config),
    ejabberd_node_utils:modify_config_file([Hosts], Config),
    reload_through_ctl(Node, Config),
    ok.

restore_domain(Config) ->
    Node = default_node(Config),
    restore_ejabberd_config_file(Node, Config),
    restart_ejabberd_node(Node),
    Config.


%%--------------------------------------------------------------------
%% Stanzas
%%--------------------------------------------------------------------


cluster_users() ->
    AllUsers = ct:get_config(escalus_users),
    [proplists:lookup(alice, AllUsers), proplists:lookup(clusterguy, AllUsers)].

default_node(Config) ->
    Node = ct:get_config({hosts, mim, node}),
    Node == undefined andalso error(node_undefined, [Config]),
    Node.
