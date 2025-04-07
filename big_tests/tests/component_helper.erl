-module(component_helper).

% API
-export([connect_component/1,
         connect_component/2,
         disconnect_component/2,
         disconnect_components/2,
         connect_component_subdomain/1,
         get_components/1,
         second_port/1,
         spec/1,
         host/1
        ]).

-export([component_start_stream/2,
         component_stream_start/2,
         component_handshake/2,
         component_start_stream_subdomain/2
        ]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml_stream.hrl").

connect_component(Component) ->
    connect_component(Component, component_start_stream).

connect_component(ComponentOpts, StartStep) ->
    Res = escalus_connection:start(ComponentOpts,
                                   [{?MODULE, StartStep},
                                    {?MODULE, component_handshake}]),
    case Res of
        {ok, Component, _} ->
            {component, ComponentName} = lists:keyfind(component, 1, ComponentOpts),
            {server, ComponentServer} = lists:keyfind(server, 1, ComponentOpts),
            ComponentAddr = <<ComponentName/binary, ".", ComponentServer/binary>>,
            {Component, ComponentAddr, ComponentName};
        {error, E} ->
            throw(cook_connection_step_error(E))
    end.

disconnect_component(Component, Addr) ->
    disconnect_components([Component], Addr).

disconnect_components(Components, Addr) ->
    [escalus_connection:stop(Component) || Component <- Components],
    wait_helper:wait_until(fun() -> rpc(mongoose_component, lookup_component, [Addr]) =:= [] end, true,
                           #{name => rpc}).

rpc(M, F, A) ->
    Node = ct:get_config({hosts, mim, node}),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    escalus_rpc:call(Node, M, F, A, 10000, Cookie).

cook_connection_step_error(E) ->
    {connection_step_failed, Step, Reason} = E,
    {StepDef, _, _} = Step,
    {EDef, _} = Reason,
    {EDef, StepDef}.

component_start_stream(Conn = #client{props = Props}, []) ->
    {server, Server} = lists:keyfind(server, 1, Props),
    {component, Component} = lists:keyfind(component, 1, Props),

    ComponentHost = <<Component/binary, ".", Server/binary>>,
    StreamStart = component_stream_start(ComponentHost, false),
    ct:log("sent~n~p~n", [StreamStart]),
    ok = escalus_connection:send(Conn, StreamStart),
    StreamStartRep = escalus_connection:get_stanza(Conn, wait_for_stream),
    ct:log("received~n~p~n", [StreamStartRep]),

    #xmlstreamstart{attrs = Attrs} = StreamStartRep,
    Id = maps:get(<<"id">>, Attrs, undefined),
    From = maps:get(<<"from">>, Attrs, undefined),
    From =:= ComponentHost orelse throw({from_does_not_correspond_to_connection, Server, From}),

    {Conn#client{props = [{sid, Id} | Props]}, []}.

component_stream_start(Component, IsSubdomain) ->
    Attrs1 = #{<<"to">> => Component,
               <<"xmlns">> => <<"jabber:component:accept">>,
               <<"xmlns:stream">> => <<"http://etherx.jabber.org/streams">>},
    Attrs2 = case IsSubdomain of
                 false ->
                     Attrs1;
                 true ->
                    Attrs1#{<<"is_subdomain">> => <<"true">>}
             end,
    #xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs2}.

component_handshake(Conn = #client{props = Props}, []) ->
    {password, Password} = lists:keyfind(password, 1, Props),
    {sid, SID} = lists:keyfind(sid, 1, Props),

    Handshake = component_handshake_el(SID, Password),
    ct:log("sent~n~p~n", [Handshake]),
    ok = escalus_connection:send(Conn, Handshake),
    HandshakeRep = escalus_connection:get_stanza(Conn, handshake),
    ct:log("received~n~p~n", [HandshakeRep]),
    case HandshakeRep of
        #xmlel{name = <<"handshake">>, children = []} ->
            {Conn, []};
        #xmlel{name = <<"stream:error">>} ->
            throw({stream_error, HandshakeRep})
    end.

component_handshake_el(SID, Password) ->
    Handshake = crypto:hash(sha, <<SID/binary, Password/binary>>),
    #xmlel{name = <<"handshake">>,
           children = [#xmlcdata{content = binary:encode_hex(Handshake, lowercase)}]}.

component_start_stream_subdomain(Conn = #client{props = Props}, []) ->
    {component, Component} = lists:keyfind(component, 1, Props),

    StreamStart = component_stream_start(Component, true),
    ct:log("sent~n~p~n", [StreamStart]),
    ok = escalus_connection:send(Conn, StreamStart),
    StreamStartRep = escalus_connection:get_stanza(Conn, wait_for_stream),
    ct:log("received~n~p~n", [StreamStartRep]),

    #xmlstreamstart{attrs = Attrs} = StreamStartRep,
    Id = maps:get(<<"id">>, Attrs, undefined),

    {Conn#client{props = [{sid, Id}|Props]}, []}.

connect_component_subdomain(Component) ->
    connect_component(Component, component_start_stream_subdomain).

host(CompSpec) ->
    proplists:get_value(component, CompSpec).

second_port(Spec) ->
    lists:keyreplace(port, 1, Spec, {port, mim2_component_port()}).

spec(vjud_component) ->
    [{component, <<"vjud">>} | common(mim2_component_port())];
spec(component_on_2) ->
    [{component, <<"yet_another_component">>} | common(mim2_component_port())];
spec(component_on_2) ->
    [{component, <<"yet_another_component">>} | common(mim2_component_port())];
spec(hidden_component) ->
    [{component, <<"hidden_component">>} | common(hidden_component_port())];
spec(tls_component) ->
    [{component, <<"tls_component">>}, {ssl, true} | common(tls_component_port())];
spec(kicking_component) ->
    [{component, <<"kicking_component">>} | common(kicking_component_port())];
spec(Other) ->
    Prefix = integer_to_binary(erlang:unique_integer([monotonic, positive])),
    Name = <<Prefix/binary, "_", (atom_to_binary(Other))/binary>>,
    [{component, Name} |  common(component_port())].

component_port() ->
    ct:get_config({hosts, mim, component_port}).

get_components(Config) ->
    Opts = common(component_port()),
    Components = [component1, component2, vjud_component],
    [ {C, Opts ++ spec(C)} || C <- Components ] ++ Config.

kicking_component_port() ->
    ct:get_config({hosts, mim, kicking_component_port}).

hidden_component_port() ->
    ct:get_config({hosts, mim, hidden_component_port}).

tls_component_port() ->
    ct:get_config({hosts, mim, tls_component_port}).

mim2_component_port() ->
    ct:get_config({hosts, mim2, component_port}).

common(Port) ->
    [{server, ct:get_config({hosts, mim, domain})},
     {host, <<"localhost">>},
     {password, <<"secret">>},
     {port, Port}].
