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
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, xep0114},
     {group, subdomain},
     {group, hidden_components},
     {group, distributed}
    ].

groups() ->
    [{xep0114, [parallel], xep0114_tests()},
     {subdomain, [], [register_subdomain]},
     {hidden_components, [], [disco_with_hidden_component]},
     {distributed, [], [register_in_cluster,
                        register_same_on_both
                        %clear_on_node_down TODO: Breaks cover
                       ]}].

suite() ->
    distributed_helper:require_rpc_nodes([mim]) ++ escalus:suite().

xep0114_tests() ->
    [register_one_component,
     register_one_component_tls,
     dirty_disconnect,
     register_two_components,
     intercomponent_communication,
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

init_per_group(xep0114, Config) ->
    instrument_helper:start(events()),
    Config;
init_per_group(subdomain, Config) ->
    add_domain(Config),
    Config;
init_per_group(distributed, Config) ->
    distributed_helper:add_node_to_cluster(Config);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(xep0114, _Config) ->
    instrument_helper:stop();
end_per_group(subdomain, Config) ->
    restore_domain(Config);
end_per_group(distributed, Config) ->
    distributed_helper:remove_node_from_cluster(Config);
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
register_one_component(Config) ->
    TS = instrument_helper:timestamp(),
    %% Given one connected component
    CompSpec = component_helper:spec(component1),
    {Component, ComponentAddr, _} = component_helper:connect_component(CompSpec),
    FullCheckF = fun(#{byte_size := S, lserver := LServer}) ->
                         S > 0 andalso LServer =:= ComponentAddr
                 end,
    CheckBytes = fun(#{byte_size := S}) -> S > 0 end,
    CheckServer = fun(#{lserver := S}) -> S =:= ComponentAddr end,

    %% Expect events for handshake, but not for 'start stream'.
    instrument_helper:assert(xmpp_element_out, ht_labels(), FullCheckF,
        #{expected_count => 1, min_timestamp => TS}),
    instrument_helper:assert(xmpp_element_in, ht_labels(), FullCheckF,
        #{expected_count => 1, min_timestamp => TS}),

    instrument_helper:assert(component_auth_failed, #{}, FullCheckF,
        #{expected_count => 0, min_timestamp => TS}),
    instrument_helper:assert(tcp_data_in, labels(), CheckBytes,
        #{min_timestamp => TS}),
    instrument_helper:assert(tcp_data_out, labels(), CheckBytes,
        #{min_timestamp => TS}),

    TS1 = instrument_helper:timestamp(),
    verify_component(Config, Component, ComponentAddr),

    % Message from Alice
    instrument_helper:assert(xmpp_element_out, ht_labels(), FullCheckF,
        #{expected_count => 1, min_timestamp => TS1}),
    % Reply to Alice
    instrument_helper:assert(xmpp_element_in, ht_labels(), FullCheckF,
        #{expected_count => 1, min_timestamp => TS1}),

    component_helper:disconnect_component(Component, ComponentAddr).

register_one_component_tls(Config) ->
    TS = instrument_helper:timestamp(),
    %% Given one connected component
    CompSpec = component_helper:spec(tls_component),
    {Component, ComponentAddr, _} = component_helper:connect_component(CompSpec),
    FullCheckF = fun(#{byte_size := S, lserver := LServer}) ->
                         S > 0 andalso LServer =:= ComponentAddr
                 end,
    CheckBytes = fun(#{byte_size := S}) -> S > 0 end,
    CheckServer = fun(#{lserver := S}) -> S =:= ComponentAddr end,
    instrument_helper:assert(tls_data_in, labels(), CheckBytes,
        #{min_timestamp => TS}),
    instrument_helper:assert(tls_data_out, labels(), CheckBytes,
        #{min_timestamp => TS}),

    TS1 = instrument_helper:timestamp(),
    verify_component(Config, Component, ComponentAddr),

    component_helper:disconnect_component(Component, ComponentAddr).

dirty_disconnect(Config) ->
    %% Given one connected component, kill the connection and reconnect
    CompSpec = component_helper:spec(component1),
    {Component, Addr, _} = component_helper:connect_component(CompSpec),
    component_helper:disconnect_component(Component, Addr),
    {Component1, Addr, _} = component_helper:connect_component(CompSpec),
    component_helper:disconnect_component(Component1, Addr).

intercomponent_communication(Config) ->
    %% Given two connected components
    CompSpec1 = component_helper:spec(component1),
    CompSpec2 = component_helper:spec(component2),
    {Comp1, CompAddr1, _} = component_helper:connect_component(CompSpec1),
    {Comp2, CompAddr2, _} = component_helper:connect_component(CompSpec2),

    TS = instrument_helper:timestamp(),
    %% When the first component sends a message the second component
    Msg0 = escalus_stanza:chat_to(CompAddr2, <<"intercomponent msg">>),
    escalus:send(Comp1, escalus_stanza:from(Msg0, CompAddr1)),
    %% Then the second component receives it
    Reply0 = escalus:wait_for_stanza(Comp2),
    escalus:assert(is_chat_message, [<<"intercomponent msg">>], Reply0),

    FullCheckF = fun(#{byte_size := S, lserver := LServer}) ->
                         S > 0 andalso LServer =:= CompAddr1 orelse LServer =:= CompAddr2
                 end,
    instrument_helper:assert(xmpp_element_out, ht_labels(), FullCheckF,
        #{expected_count => 1, min_timestamp => TS}),
    instrument_helper:assert(xmpp_element_in, ht_labels(), FullCheckF,
        #{expected_count => 1, min_timestamp => TS}),

    component_helper:disconnect_component(Comp1, CompAddr1),
    component_helper:disconnect_component(Comp2, CompAddr2).


register_two_components(Config) ->
    %% Given two connected components
    CompSpec1 = component_helper:spec(component1),
    CompSpec2 = component_helper:spec(component2),
    {Comp1, CompAddr1, _} = component_helper:connect_component(CompSpec1),
    {Comp2, CompAddr2, _} = component_helper:connect_component(CompSpec2),
    TS = instrument_helper:timestamp(),

    escalus:fresh_story(Config,
                  [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
            %% When the first component sends a message to Alice
            Msg1 = escalus_stanza:chat_to(Alice, <<"Comp1-2-Alice msg">>),
            escalus:send(Comp1, escalus_stanza:from(Msg1, CompAddr1)),
            %% Then she receives it
            Reply1 = escalus:wait_for_stanza(Alice),
            escalus:assert(is_chat_message, [<<"Comp1-2-Alice msg">>], Reply1),
            escalus:assert(is_stanza_from, [CompAddr1], Reply1),

            %% When the second component sends a message to Bob
            Msg2 = escalus_stanza:chat_to(Bob, <<"Comp2-2-Bob msg">>),
            escalus:send(Comp2, escalus_stanza:from(Msg2, CompAddr2)),
            %% Then he receives it
            Reply2 = escalus:wait_for_stanza(Bob),
            escalus:assert(is_chat_message, [<<"Comp2-2-Bob msg">>], Reply2),
            escalus:assert(is_stanza_from, [CompAddr2], Reply2),

            %% When Bob sends a reply to the second component
            Msg3 = escalus_stanza:chat_to(CompAddr2, <<"Bob-2-Comp2 msg">>),
            escalus:send(Bob, Msg3),
            %% Then the second component receives it
            Reply3 = escalus:wait_for_stanza(Comp2),
            escalus:assert(is_chat_message, [<<"Bob-2-Comp2 msg">>], Reply3),

            %% When Alice sends a reply to the first component
            Msg4 = escalus_stanza:chat_to(CompAddr1, <<"Alice-2-Comp1 msg">>),
            escalus:send(Alice, Msg4),
            %% Then the first component receives it
            Reply4 = escalus:wait_for_stanza(Comp1),
            escalus:assert(is_chat_message, [<<"Alice-2-Comp1 msg">>], Reply4)
        end),

    FullCheckF = fun(#{byte_size := S, lserver := LServer}) ->
                    S > 0 andalso LServer =:= CompAddr1 orelse LServer =:= CompAddr2
             end,
    % Msg to Alice, msg to Bob
    instrument_helper:assert(xmpp_element_in, ht_labels(), FullCheckF,
        #{expected_count => 2, min_timestamp => TS}),
    % Msg from Bob, msg from Alice
    instrument_helper:assert(xmpp_element_out, ht_labels(), FullCheckF,
        #{expected_count => 2, min_timestamp => TS}),

    component_helper:disconnect_component(Comp1, CompAddr1),
    component_helper:disconnect_component(Comp2, CompAddr2).

try_registering_with_wrong_password(Config) ->
    %% Given a component with a wrong password
    TS = instrument_helper:timestamp(),
    CompSpec1 = component_helper:spec(component1),
    CompSpec2 = lists:keyreplace(password, 1, CompSpec1, {password, <<"wrong_one">>}),
    try
        %% When trying to connect it
        {Comp, Addr, _} = component_helper:connect_component(CompSpec2),
        component_helper:disconnect_component(Comp, Addr),
        ct:fail("component connected successfully with wrong password")
    catch {stream_error, _E} ->
        %% Then it should fail to do so
        instrument_helper:assert(component_auth_failed, #{}, fun(_) -> true end,
                                 #{expected_count => 1, min_timestamp => TS}),
        ok
    end.

try_registering_component_twice(Config) ->
    %% Given two components with the same name
    CompSpec1 = component_helper:spec(component1),
    {Comp1, Addr, _} = component_helper:connect_component(CompSpec1),

    try
        %% When trying to connect the second one
        {Comp2, Addr, _} = component_helper:connect_component(CompSpec1),
        component_helper:disconnect_component(Comp2, Addr),
        ct:fail("second component connected successfully")
    catch {stream_error, _} ->
        %% Then it should fail to do so
        ok
    end,

    component_helper:disconnect_component(Comp1, Addr).

try_registering_existing_host(Config) ->
    %% Given a external vjud component
    Component = component_helper:spec(vjud_component),

    try
        %% When trying to connect it to the server
        {Comp, Addr, _} = component_helper:connect_component(Component),
        component_helper:disconnect_component(Comp, Addr),
        ct:fail("vjud component connected successfully")
    catch {stream_error, _} ->
        %% Then it should fail since vjud service already exists on the server
        ok
    end.

%% When conflict_behaviour is kick_old, then:
%% - stop old connections by sending stream:error with reason "conflict"
kick_old_component_on_conflict(Config) ->
    CompSpec1 = component_helper:spec(kicking_component),
    {Comp1, Addr, _} = component_helper:connect_component(CompSpec1),

    %% When trying to connect the second one
    {Comp2, Addr, _} = component_helper:connect_component(CompSpec1),

    %% First connection is disconnected
    Stanza = escalus:wait_for_stanza(Comp1),
    escalus:assert(is_stream_error, [<<"conflict">>, <<>>], Stanza),

    %% New connection is usable
    verify_component(Config, Comp2, Addr),

    component_helper:disconnect_component(Comp2, Addr).

disco_components(Config) ->
    %% Given two connected components
    CompSpec1 = component_helper:spec(component1),
    CompSpec2 = component_helper:spec(component2),
    {Comp1, Addr1, _} = component_helper:connect_component(CompSpec1),
    {Comp2, Addr2, _} = component_helper:connect_component(CompSpec2),

    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
                %% When server asked for the disco features
                Server = escalus_client:server(Alice),
                Disco = escalus_stanza:service_discovery(Server),
                escalus:send(Alice, Disco),

                %% Then it contains hosts of 2 components
                DiscoReply = escalus:wait_for_stanza(Alice),
                escalus:assert(has_service, [Addr1], DiscoReply),
                escalus:assert(has_service, [Addr2], DiscoReply)
        end),

    component_helper:disconnect_component(Comp1, Addr1),
    component_helper:disconnect_component(Comp2, Addr2).

%% Verifies that a component connected to the "hidden components" endpoint
%% is not discoverable.
%% Assumes mod_disco with `{users_can_see_hidden_services, false}` option
disco_with_hidden_component(Config) ->
    %% Given two connected components
    CompSpec1 = component_helper:spec(component1),
    HCompOpts = component_helper:spec(hidden_component),
    {Comp1, Addr1, _} = component_helper:connect_component(CompSpec1),
    {HComp, HAddr, _} = component_helper:connect_component(HCompOpts),

    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
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

    component_helper:disconnect_component(Comp1, Addr1),
    component_helper:disconnect_component(HComp, HAddr).

register_subdomain(Config) ->
    %% Given one connected component
    CompSpec1 = component_helper:spec(component1),
    {Comp, Addr, Name} = component_helper:connect_component_subdomain(CompSpec1),

    escalus:fresh_story(Config, [{alice, 1}, {astrid, 1}], fun(Alice, Astrid) ->
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

    component_helper:disconnect_component(Comp, Addr).


register_in_cluster(Config) ->
    %% Given one component connected to the cluster
    CompSpec1 = component_helper:spec(component1),
    Component1 = component_helper:connect_component(CompSpec1),
    {Comp1, Addr1, _} = Component1,
    CompSpec2 = component_helper:spec(component2),
    Component2 = component_helper:connect_component(CompSpec2),
    {Comp2, Addr2, _} = Component2,
    CompSpec_on_2 = component_helper:spec(component_on_2),
    Component_on_2 = component_helper:connect_component(CompSpec_on_2),
    {Comp_on_2, Addr_on_2, _} = Component_on_2,

    escalus:fresh_story(Config, [{alice, 1}, {clusterguy, 1}], fun(Alice, ClusterGuy) ->
                do_chat_with_component(Alice, ClusterGuy, Component1),
                do_chat_with_component(Alice, ClusterGuy, Component2),
                do_chat_with_component(Alice, ClusterGuy, Component_on_2)
        end),

    component_helper:disconnect_component(Comp1, Addr1),
    component_helper:disconnect_component(Comp2, Addr2),
    component_helper:disconnect_component(Comp_on_2, Addr_on_2).

clear_on_node_down(Config) ->
    CompSpec = component_helper:spec(component1),
    ?assertMatch({_, _, _}, component_helper:connect_component(CompSpec)),
    ?assertThrow({stream_error, _}, component_helper:connect_component(CompSpec)),

    distributed_helper:stop_node(mim(), Config),
    distributed_helper:start_node(mim(), Config),

    {Comp, Addr, _} = component_helper:connect_component(CompSpec),
    component_helper:disconnect_component(Comp, Addr).

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
    CompSpec2 = component_helper:spec(component2),
    Component2 = component_helper:connect_component(CompSpec2),
    {Comp2, Addr, Name} = Component2,
    CompSpec_d = component_helper:second_port(CompSpec2),
    Component_d = component_helper:connect_component(CompSpec_d),
    {Comp_d, Addr, Name} = Component_d,

    escalus:fresh_story(Config, [{alice, 1}, {clusterguy, 1}], fun(Alice, ClusterGuy) ->
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
    component_helper:disconnect_components([Comp2, Comp_d], Addr),
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
verify_component(Config, Component, ComponentAddr) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
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

add_domain(Config) ->
    Hosts = {hosts, "\"localhost\", \"sogndal\""},
    ejabberd_node_utils:backup_config_file(Config),
    ejabberd_node_utils:modify_config_file([Hosts], Config),
    ejabberd_node_utils:restart_application(mongooseim),
    ok.

restore_domain(Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(mongooseim),
    Config.

events() ->
    instrument_helper:declared_events(mongoose_component_listener, [#{}]).

%% Data metrics have only the connection_type label
labels() ->
    #{connection_type => component}.

%% XMPP element metric labels include host_type, but components don't have host types
ht_labels() ->
    (labels())#{host_type => <<>>}.

%%--------------------------------------------------------------------
%% Stanzas
%%--------------------------------------------------------------------

cluster_users() ->
    AllUsers = ct:get_config(escalus_users),
    [proplists:lookup(alice, AllUsers), proplists:lookup(clusterguy, AllUsers)].
