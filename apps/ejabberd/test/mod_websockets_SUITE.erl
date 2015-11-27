-module(mod_websockets_SUITE).
-export([all/0, setup/0, teardown/1, ping_test/1]).
-include_lib("eunit/include/eunit.hrl").
-define(HANDSHAKE_TIMEOUT, 3000).
-define(eq(E, I), ?assertEqual(E, I)).
-define(PORT, 5280).
-define(IP, {127, 0, 0, 1}).
-define(FAST_PING_RATE, 1000).


all() -> [ ping_test ].

init_per_suite(C) ->
    C.

setup() ->
    application:ensure_all_started(cowboy),
    meck:new(supervisor, [unstick, passthrough]),
    meck:new(ejabberd_c2s, [unstick, passthrough]),
    meck:new(gen_mod,[unstick, passthrough]),
    %% Set ping rate to 1 sec
    meck:expect(gen_mod,get_opt, fun(ping_rate, _, none) -> ?FAST_PING_RATE;
                                    (A, B, C) -> meck:passthrough([A, B, C]) end),
    %% mock ejabberd_c2s
    meck:expect(ejabberd_c2s, start, fun(_,_) -> {ok, mocked_pid} end),
    meck:expect(supervisor, start_child,
                fun(ejabberd_sup, {_, {_, start_link, [_]}, permanent,
                                                            infinity, worker, [_]}) -> ok;
                   (A,B) ->meck:passthrough([A,B]) end),
    %% Start websocket cowboy listening
    spawn(fun() ->
    Opts = [{num_acceptors, 10},
            {max_connections, 1024},
            {modules, [{"_", "/http-bind", mod_bosh},
                       {"_", "/ws-xmpp", mod_websockets,
                        [{timeout, 600000}, {ping_rate, 2000}]}]}],
        ejabberd_cowboy:start_listener({?PORT, ?IP, tcp}, Opts)
          end).


teardown(Pid) ->
    cowboy:stop_listener(http_listener),
    meck:unload(supervisor),
    meck:unload(ejabberd_c2s),
    meck:unload(gen_mod),
    exit(Pid, normal),
    ok.

ping_test(_Config) ->
    %% Given
    Pid = setup(),
    timer:sleep(500),
    {ok, Socket1} = ws_handshake("localhost", ?PORT),
    %% When
    Resp = wait_for_ping(Socket1, 0),
    %% then
    ?eq(Resp, ok),
    teardown(Pid).


%% Client side
ws_handshake(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, raw},
                                                {active, false}]),
    ok = gen_tcp:send(Socket, [
        "GET /ws-xmpp HTTP/1.1\r\n"
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
    {ok, Socket}.

wait_for_ping(_, Try) when Try > 10 ->
    {error, no_ping_packet};
wait_for_ping(Socket, Try) ->
    {Reply, Content} = gen_tcp:recv(Socket, 0, 5000),
    case Reply of
        error ->
            {error, Content};
        ok ->
            Ping = ws_rx_frame(<<"">>, 9),
            case Content of
                Ping ->
                    ok;
                _ ->
                    wait_for_ping(Socket, Try + 1)
            end
    end.

%% Helpers
ws_rx_frame(Payload, Opcode) ->
    Length = byte_size(Payload),
    <<1:1, 0:3, Opcode:4, 0:1, Length:7, Payload/binary>>.



