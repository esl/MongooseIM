-module(mod_websockets_SUITE).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

-define(eq(E, I), ?assertEqual(E, I)).
-define(PORT, 5280).
-define(IP, {127, 0, 0, 1}).
-define(FAST_PING_RATE, 500).
-define(IDLE_TIMEOUT, 1200 * 2 + 300).

-import(config_parser_helper, [config/2, default_config/1]).

all() ->
    [ping_test | subprotocol_header_tests() ++ timeout_tests()].

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
    setup(C),
    C.

end_per_suite(C) ->
    teardown(C).

init_per_testcase(_, C) ->
    C.

end_per_testcase(_, C) ->
    C.

setup(Config) ->
    meck:unload(),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jid),
    meck:new(supervisor, [unstick, passthrough, no_link]),
    meck:new(gen_mod, [unstick, passthrough, no_link]),
    %% Set ping rate
    meck:expect(gen_mod, get_opt, fun(ping_rate, _, none) -> ?FAST_PING_RATE;
                                     (A, B, C) -> meck:passthrough([A, B, C]) end),
    mongoose_config:set_opts(#{default_server_name => <<"localhost">>}),
    %% Start websocket cowboy listening
    Handlers = [config([listen, http, handlers, mod_bosh],
                       #{host => '_', path => "/http-bind"}),
                config([listen, http, handlers, mod_websockets],
                       #{host => '_', path => "/ws-xmpp",
                         timeout => ?IDLE_TIMEOUT, ping_rate => ?FAST_PING_RATE})],
    Opts = #{module => ejabberd_cowboy,
             port => ?PORT,
             ip_tuple => ?IP,
             ip_address => "127.0.0.1",
             ip_version => inet,
             proto => tcp,
             handlers => Handlers,
             transport => default_config([listen, http, transport]),
             protocol => default_config([listen, http, protocol])},
    #{start := {M, F, A}} = ejabberd_cowboy:listener_spec(Opts),
    async_helper:start(Config, M, F, A).

teardown(Config) ->
    async_helper:stop_all(Config),
    meck:unload(),
    cowboy:stop_listener(ejabberd_cowboy:ref({?PORT, ?IP, tcp})),
    mongoose_config:erase_opts(),
    application:stop(cowboy),
    ok.

ping_test(_Config) ->
    timer:sleep(500),
    #{socket := Socket1} = ws_handshake(),
    %% When
    Resp = wait_for_ping(Socket1, 0, 5000),
    %% then
    ?eq(Resp, ok).

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
    CSup = get_child_by_mod(LSup, ranch_conns_sup_sup),
    [{Mod, Pid} || {_, Sup, _, [ranch_conns_sup]} <- supervisor:which_children(CSup),
                   {_, Pid, _, [Mod]} <- supervisor:which_children(Sup)].

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
