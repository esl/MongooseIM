-module(mod_websockets_SUITE).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-define(HANDSHAKE_TIMEOUT, 3000).
-define(eq(E, I), ?assertEqual(E, I)).
-define(PORT, 5280).
-define(IP, {127, 0, 0, 1}).
-define(FAST_PING_RATE, 500).
-define(NEW_TIMEOUT, 1200).
%The timeout is long enough to pass all test cases for ping interval settings
%using NEW_TIMEOUT value. In these tests we wait for at most 2 pings.
%The 300ms is just an additional overhead
-define(IDLE_TIMEOUT, ?NEW_TIMEOUT * 2 + 300).


all() ->
    ping_tests() ++ subprotocol_header_tests() ++ timeout_tests().

ping_tests() ->
    [ping_test,
     set_ping_test,
     disable_ping_test,
     disable_and_set].

subprotocol_header_tests() ->
    [agree_to_xmpp_subprotocol,
     agree_to_xmpp_subprotocol_case_insensitive,
     agree_to_xmpp_subprotocol_from_many,
     do_not_agree_to_missing_subprotocol,
     do_not_agree_to_other_subprotocol].

timeout_tests() ->
    [connection_is_closed_after_idle_timeout,
     client_ping_frame_resets_idle_timeout].

init_per_suite(C) ->
    setup(),
    C.

end_per_suite(_) ->
    teardown(),
    ok.

init_per_testcase(_, C) ->
    C.

end_per_testcase(_, C) ->
    C.

setup() ->
    meck:unload(),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(stringprep),
    application:ensure_all_started(lager),
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
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),
    %% Start websocket cowboy listening

    Opts = [{num_acceptors, 10},
            {max_connections, 1024},
            {modules, [{"_", "/http-bind", mod_bosh},
                       {"_", "/ws-xmpp", mod_websockets,
                        [{timeout, ?IDLE_TIMEOUT},
                         {ping_rate, ?FAST_PING_RATE}]}]}],
    ejabberd_cowboy:start_listener({?PORT, ?IP, tcp}, Opts).


teardown() ->
    meck:unload(),
    cowboy:stop_listener(ejabberd_cowboy:ref({?PORT, ?IP, tcp})),
    application:stop(cowboy),
    application:stop(lager),
    %% Do not stop stringprep, Erlang 21 does not like to reload nifs
    ok.

ping_test(_Config) ->
    timer:sleep(500),
    #{socket := Socket1} = ws_handshake(),
    %% When
    Resp = wait_for_ping(Socket1, 0, 5000),
    %% then
    ?eq(Resp, ok).

set_ping_test(_Config) ->
    #{socket := Socket1, internal_socket := InternalSocket} = ws_handshake(),
    %% When
    mod_websockets:set_ping(InternalSocket, ?NEW_TIMEOUT),
    WaitMargin = 300,
    ok = wait_for_ping(Socket1, 0 , ?NEW_TIMEOUT + WaitMargin),
    %% Im waiting too less time!
    TooShort = 700,
    ErrorTimeout = wait_for_ping(Socket1, 0, TooShort),
    ?eq({error, timeout}, ErrorTimeout),
    %% now I'm wait the remaining time (and some margin)
    ok = wait_for_ping(Socket1, 0, ?NEW_TIMEOUT - TooShort + WaitMargin).

disable_ping_test(_Config) ->
    #{socket := Socket1, internal_socket := InternalSocket} = ws_handshake(),
    %% When
    mod_websockets:disable_ping(InternalSocket),
    %% Should not receive any packets
    ErrorTimeout = wait_for_ping(Socket1, 0, ?FAST_PING_RATE),
    %% then
    ?eq(ErrorTimeout, {error, timeout}).

disable_and_set(_Config) ->
    #{socket := Socket1, internal_socket := InternalSocket} = ws_handshake(),
    %% When
    mod_websockets:disable_ping(InternalSocket),
    %% Should not receive any packets
    ErrorTimeout = wait_for_ping(Socket1, 0, ?FAST_PING_RATE),
    mod_websockets:set_ping(InternalSocket, ?NEW_TIMEOUT),
    Resp1 = wait_for_ping(Socket1, 0, ?NEW_TIMEOUT + 100),
    %% then
    ?eq(ErrorTimeout, {error, timeout}),
    ?eq(Resp1, ok).

connection_is_closed_after_idle_timeout(_Config) ->
    #{socket := Socket} = ws_handshake(),
    inet:setopts(Socket, [{active, true}]),
    Closed = wait_for_close(Socket),
    ?eq(Closed, ok).

client_ping_frame_resets_idle_timeout(_Config) ->
    #{socket := Socket} = ws_handshake(#{extra_headers => [<<"sec-websocket-protocol: xmpp\r\n">>]}),
    Now = os:system_time(millisecond),
    inet:setopts(Socket, [{active, true}]),
    WaitBeforePingFrame = (?IDLE_TIMEOUT) div 2,
    timer:sleep(WaitBeforePingFrame),
    %%Masked ping frame
    Ping = << 1:1, 0:3, 9:4, 1:1, 0:39 >>,
    ok = gen_tcp:send(Socket, Ping),
    Closed = wait_for_close(Socket),
    ?eq(Closed, ok),
    End = os:system_time(millisecond),
    %%Below we check if the time difference between now and the moment
    %%the WebSocket was established is bigger then the the ?IDLE_TIMEOUT plus initial wait time
    %%This shows that the connection was not killed after the first ?IDLE_TIMEOUT
    ?assert(End - Now > ?IDLE_TIMEOUT + WaitBeforePingFrame).

wait_for_close(Socket) ->
    receive
        {tcp_closed, Socket} ->
            ok
    after ?IDLE_TIMEOUT + 500 ->
              timeout
    end.


ws_handshake() ->
    ws_handshake(#{}).

%% Client side
%% Gun is too high level for subprotocol_header_tests checks
ws_handshake(Opts) ->
    Host = "localhost",
    Port = ?PORT,
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
                       ++ maps:get(extra_headers, Opts, ""),
                       "\r\n"]),
    {ok, Handshake} = gen_tcp:recv(Socket, 0, 5000),
    Packet = erlang:decode_packet(http, Handshake, []),
    {ok, {http_response, {1,1}, 101, "Switching Protocols"}, Rest} = Packet,
    {Headers, _} = consume_headers(Rest, []),
    InternalSocket = get_websocket(),
    #{socket => Socket, internal_socket => InternalSocket, headers => Headers}.

consume_headers(Data, Headers) ->
    case erlang:decode_packet(httph, Data, []) of
        {ok, http_eoh, Rest} ->
            {lists:reverse(Headers), Rest};
        {ok, {http_header,_,Name,_,Value}, Rest} ->
            consume_headers(Rest, [{Name, Value}|Headers])
    end.

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
    %% it's the one which started due to our gen_tcp:connect in ws_handshake/1
    [{cowboy_clear, Pid}] = get_ranch_connections(),
    %% This is a record! See mod_websockets: #websocket{}.
    {websocket, Pid, fake_peername, undefined}.

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

wait_for_no_ranch_connections(Times) ->
    case get_ranch_connections() of
        [] ->
            ok;
        _ when Times > 0 ->
            timer:sleep(100),
            wait_for_no_ranch_connections(Times - 1);
       Connections ->
            error(#{reason => wait_for_no_ranch_connections_failed,
                    connections => Connections})
    end.

%% ---------------------------------------------------------------------
%% subprotocol_header_tests test functions
%% ---------------------------------------------------------------------

%% From RFC 6455:
%%   The |Sec-WebSocket-Protocol| header field is used in the WebSocket
%%   opening handshake.  It is sent from the client to the server and back
%%   from the server to the client to confirm the subprotocol of the
%%   connection.
agree_to_xmpp_subprotocol(_) ->
    check_subprotocol("Proper client behaviour", ["xmpp"], "xmpp").

agree_to_xmpp_subprotocol_case_insensitive(_) ->
    %% The value must conform to the requirements
    %% given in item 10 of Section 4.1 of this specification -- namely,
    %% the value must be a token as defined by RFC 2616 [RFC2616].
    %% ...
    %% 10.  The request MAY include a header field with the name
    %%      |Sec-WebSocket-Protocol|.  If present, this value indicates one
    %%      or more comma-separated subprotocol the client wishes to speak,
    %%      ordered by preference.  The elements that comprise this value
    %%      MUST be non-empty strings with characters in the range U+0021 to
    %%      U+007E not including separator characters as defined in
    %%      [RFC2616] and MUST all be unique strings.  The ABNF for the
    %%      value of this header field is 1#token, where the definitions of
    %%      constructs and rules are as given in [RFC2616].
    %% ...
    check_subprotocol("Case insensitive", ["XMPP"], "XMPP").

%% From RFC 6455:
%%   The |Sec-WebSocket-Protocol| header field MAY appear multiple times
%%   in an HTTP request (which is logically the same as a single
%%   |Sec-WebSocket-Protocol| header field that contains all values).
%%   However, the |Sec-WebSocket-Protocol| header field MUST NOT appear
%%   more than once in an HTTP response.
agree_to_xmpp_subprotocol_from_many(_) ->
    check_subprotocol("Two protocols in one header", ["xmpp, other"], "xmpp"),
    check_subprotocol("Two protocols in one header", ["other, xmpp"], "xmpp"),
    check_subprotocol("Two protocols in two headers", ["other", "xmpp"], "xmpp").

%% Do not set a Sec-Websocket-Protocol header in response if it's missing in a request.
%%
%% From RFC 6455:
%%   if the server does not wish to agree to one of the suggested
%%   subprotocols, it MUST NOT send back a |Sec-WebSocket-Protocol|
%%   header field in its response.
do_not_agree_to_missing_subprotocol(_) ->
    check_subprotocol("Subprotocol header is missing", [], undefined).

%% Do not set a Sec-Websocket-Protocol header in response if it's provided, but not xmpp.
do_not_agree_to_other_subprotocol(_) ->
    check_subprotocol("Subprotocol is not xmpp", ["other"], undefined).


%% ---------------------------------------------------------------------
%% subprotocol_header_tests helper functions
%% ---------------------------------------------------------------------

check_subprotocol(Comment, ProtoList, ExpectedProtocol) ->
    ReqHeaders = lists:append(["Sec-Websocket-Protocol: " ++ Proto ++ "\r\n" || Proto <- ProtoList]),
    Info = #{reason => check_subprotocol_failed,
             comment => Comment,
             expected_protocol => ExpectedProtocol,
             request_headers => ReqHeaders},
    #{headers := RespHeaders, socket := Socket} = ws_handshake(#{extra_headers => ReqHeaders}),
    %% get_websocket/0 does not support more than one open connection
    gen_tcp:close(Socket),
    wait_for_no_ranch_connections(10),
    RespProtocol = proplists:get_value("Sec-Websocket-Protocol", RespHeaders),
    case RespProtocol of
        ExpectedProtocol ->
            ok;
        _ ->
            Info2 = Info#{response_headers => RespHeaders,
                          response_protocol => RespProtocol},
            ct:fail(Info2)
    end.
