-module(mod_websockets_SUITE).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-define(HANDSHAKE_TIMEOUT, 3000).
-define(eq(E, I), ?assertEqual(E, I)).
-define(PORT, 5280).
-define(IP, {127, 0, 0, 1}).
-define(FAST_PING_RATE, 500).
-define(NEW_TIMEOUT, 1200).


all() -> [ping_test,
          set_ping_test,
          disable_ping_test,
          disable_and_set].

init_per_testcase(_, C) ->
    setup(),
    C.

end_per_testcase(_, C) ->
    teardown(),
    C.

setup() ->
    meck:unload(),
    application:ensure_all_started(cowboy),
    meck:new(supervisor, [unstick, passthrough, no_link]),
    meck:new(gen_mod,[unstick, passthrough, no_link]),
    %% Set ping rate
    meck:expect(gen_mod,get_opt, fun(ping_rate, _, none) -> ?FAST_PING_RATE;
                                    (A, B, C) -> meck:passthrough([A, B, C]) end),
    meck:expect(supervisor, start_child,
                fun(ejabberd_listeners, {_, {_, start_link, [_]}, transient,
                                         infinity, worker, [_]}) -> {ok, self()};
                   (A,B) -> meck:passthrough([A,B])
                end),
    %% Start websocket cowboy listening

    Opts = [{num_acceptors, 10},
            {max_connections, 1024},
            {modules, [{"_", "/http-bind", mod_bosh},
                       {"_", "/ws-xmpp", mod_websockets,
                        [{timeout, 600000}, {ping_rate, ?FAST_PING_RATE}]}]}],
    ejabberd_cowboy:start_listener({?PORT, ?IP, tcp}, Opts).


teardown() ->
    meck:unload(),
    cowboy:stop_listener(ejabberd_cowboy:ref({?PORT, ?IP, tcp})),
    application:stop(cowboy),
    ok.

ping_test(_Config) ->
    timer:sleep(500),
    {ok, Socket1, _} = ws_handshake("localhost", ?PORT),
    %% When
    Resp = wait_for_ping(Socket1, 0, 5000),
    %% then
    ?eq(Resp, ok).

set_ping_test(_Config) ->
    {ok, Socket1, InternalSocket} = ws_handshake("localhost", ?PORT),
    %% When
    mod_websockets:set_ping(InternalSocket, ?NEW_TIMEOUT),
    ok = wait_for_ping(Socket1, 0 , ?NEW_TIMEOUT + 1000),
    %% Im waiting too less time!
    ErrorTimeout = wait_for_ping(Socket1, 0, 700),
    ok = wait_for_ping(Socket1, 0, ?NEW_TIMEOUT + 1000),
    %% now I wait enough time
    Resp1 = wait_for_ping(Socket1, 0, ?NEW_TIMEOUT + 200),
    %% then
    ?eq(ok, Resp1),
    ?eq({error, timeout}, ErrorTimeout).

disable_ping_test(_Config) ->
    {ok, Socket1, InternalSocket} = ws_handshake("localhost", ?PORT),
    %% When
    mod_websockets:disable_ping(InternalSocket),
    %% Should not receive any packets
    ErrorTimeout = wait_for_ping(Socket1, 0, ?FAST_PING_RATE),
    %% then
    ?eq(ErrorTimeout, {error, timeout}).

disable_and_set(_Config) ->
    {ok, Socket1, InternalSocket} = ws_handshake("localhost", ?PORT),
    %% When
    mod_websockets:disable_ping(InternalSocket),
    %% Should not receive any packets
    ErrorTimeout = wait_for_ping(Socket1, 0, ?FAST_PING_RATE),
    mod_websockets:set_ping(InternalSocket, ?NEW_TIMEOUT),
    Resp1 = wait_for_ping(Socket1, 0, ?NEW_TIMEOUT + 100),
    %% then
    ?eq(ErrorTimeout, {error, timeout}),
    ?eq(Resp1, ok).

%% Client side
ws_handshake(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, raw},
                                                {active, false}]),
    ok = gen_tcp:send(Socket,
                      ["GET /ws-xmpp HTTP/1.1\r\n"
                       "Host: localhost:5280\r\n"
                       "Connection: upgrade\r\n"
                       "Origin: http://localhost\r\n"
                       "Sec-WebSocket-Key: NT1P6NvEFQyDDKuTyEN+1Q==\r\n"
                       "Sec-WebSocket-Version: 13\r\n"
                       "Upgrade: websocket\r\n"
                       "\r\n"]),
    {ok, Handshake} = gen_tcp:recv(Socket, 0, 5000),
    Packet = erlang:decode_packet(http, Handshake, []),
    {ok, {http_response, {1,1}, 101, "Switching Protocols"}, _Rest} = Packet,
    InternalSocket = get_websocket(),
    {ok, Socket, InternalSocket}.

wait_for_ping(_, Try, _) when Try > 10 ->
    {error, no_ping_packet};
wait_for_ping(Socket, Try, Timeout) ->
    {Reply, Content} = gen_tcp:recv(Socket, 0, Timeout),
    case Reply of
        error ->
            {error, Content};
        ok ->
            Ping = ws_rx_frame(<<"">>, 9),
            case Content of
                Ping ->
                    ok;
                _ ->
                    wait_for_ping(Socket, Try + 1, Timeout)
            end
    end.

%% Helpers
ws_rx_frame(Payload, Opcode) ->
    Length = byte_size(Payload),
    <<1:1, 0:3, Opcode:4, 0:1, Length:7, Payload/binary>>.

get_websocket() ->
    %% Assumption: there's only one ranch protocol process running and
    %% it's the one which started due to our gen_tcp:connect in ws_handshake/2.
    [{cowboy_clear, Pid}] = get_ranch_connections(),
    %% This is a record! See mod_websockets: #websocket{}.
    {websocket, Pid, fake_peername}.

get_child_by_mod(Sup, Mod) ->
    Kids = supervisor:which_children(Sup),
    case lists:keyfind([Mod], 4, Kids) of
        false -> error(not_found, [Sup, Mod]);
        {_, KidPid, _, _} -> KidPid
    end.

get_ranch_connections() ->
    LSup = get_child_by_mod(ranch_sup, ranch_listener_sup),
    CSup = get_child_by_mod(LSup, ranch_conns_sup),
    [ {Mod, Pid} || {_, Pid, _, [Mod]} <- supervisor:which_children(CSup) ].
