-module(traffic_monitor_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("jid/include/jid.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------


all() ->
    [
        {group, regular}
    ].

groups() ->
    [{regular, [], [get_page, tracing_from_websocket]}].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    mongoose_helper:inject_module(?MODULE),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) when GroupName =:= regular; GroupName =:= async_pools ->
    HostType = domain_helper:host_type(),
    SecHostType = domain_helper:secondary_host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    Config2 = dynamic_modules:save_modules(SecHostType, Config1),
    ok = dynamic_modules:ensure_modules(HostType, [{mongoose_traffic, #{}}]),
    init_listeners(),
    Config2.

init_per_testcase(get_page, Config) ->
    {ok, Client} = fusco:start("http://localhost:5111", []),
    init_per_testcase(generic, [{http_client, Client} | Config]);
init_per_testcase(tracing_from_websocket, Config) ->
    {ok, Wsc} = gun:open("localhost", 5111, #{transport => tcp, protocols => [http], retry => 1}),
    StreamRef = gun:ws_upgrade(Wsc, "/ws-traffic", [], #{}),
    receive
        {gun_upgrade, Wsc, StreamRef, [<<"websocket">>], _} -> ok
    after 1000 ->
        ct:fail("gun did not fire")
    end,
    init_per_testcase(generic, [{ws_client, {Wsc, StreamRef}} | Config]);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(get_page, Config) ->
    C = proplists:get_value(http_client, Config),
    fusco:disconnect(C),
    end_per_testcase(generic, Config);
end_per_testcase(tracing_from_websocket, Config) ->
    {C, _} = proplists:get_value(ws_client, Config),
    gun:close(C),
    end_per_testcase(generic, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

end_per_group(GroupName, Config) when GroupName =:= regular ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config).

get_page(Config) ->
    % just to make sure listener works
    C = proplists:get_value(http_client, Config),
    {ok, Res} = fusco:request(C, <<"/traffic">>, <<"GET">>, [], [], 5000),
    {{<<"200">>, <<"OK">>}, _, _, _, _} = Res,
    ok.

tracing_from_websocket(Config) ->
    C = proplists:get_value(ws_client, Config),
    send(C, <<"get_status">>, #{}),
    receive_status(false),
    send(C, <<"trace_flag">>, #{<<"value">>  => true}),
    receive_status(true),
    send(C, <<"get_status">>, #{}),
    receive_status(true),
    escalus:fresh_story(Config, [{carol, 1}], fun(Alice) ->
            {<<"new_trace">>, #{<<"bare_jid">> := <<>>}} = receive_msg(),
            {<<"new_trace">>, #{<<"full_jid">> := <<>>, <<"pid">> := BPid}} = receive_msg(),
            send(C, <<"get_trace">>, #{<<"pid">>  => BPid}),
            {<<"get_trace">>, Trace} = receive_msg(),
            [_ | _] = maps:get(<<"trace">>, Trace),
            Server = escalus_client:server(Alice),
            escalus:send(Alice, escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_INFO, []), Server)),
            {<<"message">>, _} = receive_msg(),
            ok
        end),
    ok.

init_listeners() ->
    Opts = #{connection_type => undefined,
             handlers =>
                 [#{host => '_',module => mongoose_traffic, path => "/traffic/[...]"},
                  #{host => '_',module => mongoose_traffic_channel, path => "/ws-traffic"}],
             ip_address => "0",
             ip_tuple => {0,0,0,0},
             ip_version => 4,
             module => ejabberd_cowboy,
             port => 5111,
             proto => tcp,
             protocol => #{compress => false},
             transport => #{max_connections => 1024,num_acceptors => 2}},
    distributed_helper:rpc(distributed_helper:mim(), ejabberd_cowboy,
        start_listener, [Opts]),
    ok.

receive_status(Expected) ->
    {Evt, Data} = receive_msg(),
    ?assertEqual(<<"status">>, Evt),
    ?assertEqual(Expected, maps:get(<<"trace_flag">>, Data, missing)).

receive_msg() ->
    receive
        {gun_ws, _, _, {text, Bin}} ->
            {Data} = jiffy:decode(Bin),
            Event = proplists:get_value(<<"event">>, Data),
            {Payload} = proplists:get_value(<<"payload">>, Data),
            {Event, maps:from_list(Payload)}
    after 100 ->
        ct:fail("message not received")
    end.

send({C, Stream}, Event, Payload) ->
    Data = #{event => Event, payload => Payload},
    gun:ws_send(C, Stream, {text, jiffy:encode(Data)}).