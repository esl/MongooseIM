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

-module(cowboy_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(SERVER, "http://localhost:8080").

-import(ejabberd_helper, [start_ejabberd/1,
                          stop_ejabberd/0,
                          use_config_file/2,
                          start_ejabberd_with_config/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, routing},
     start_cowboy_returns_error_eaddrinuse,
     conf_reload].

groups() ->
    [{routing, [sequence], [http_requests,
                            ws_request_bad_protocol,
                            ws_requests_xmpp,
                            ws_requests_other,
                            mixed_requests]}].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

-define(APPS, [crypto, ssl, fusco, ranch, cowlib, cowboy]).

init_per_suite(Config) ->
    [application:start(App) || App <- ?APPS],
    {ok, Pid} = create_handlers(),
    [{meck_pid, Pid}|Config].

end_per_suite(Config) ->
    remove_handlers(Config),
    Config.

init_per_group(routing, Config) ->
    start_cowboy(),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(routing, Config) ->
    stop_cowboy(),
    Config;
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, Config) ->
    reset_history(),
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
http_requests(_Config) ->
    %% Given
    Host = ?SERVER,
    Path = <<"/">>,
    Method = "GET",
    Headers = [],
    Body = [],

    %% When
    Codes = [begin
                Response = execute_request(Host, Path, Method, Headers, Body),
                is_status_code(Response, 200)
            end || _ <- lists:seq(1, 50)],

    %% Then
    Codes = lists:duplicate(50, true),
    50 = meck:num_calls(dummy_http_handler, init, '_'),
    50 = meck:num_calls(dummy_http_handler, handle, '_'),
    50 = meck:num_calls(dummy_http_handler, terminate, '_').

ws_request_bad_protocol(_Config) ->
    %% Given
    Host = ?SERVER,
    Path = <<"/">>,
    Method = "GET",
    Headers = ws_headers(<<"unknown-protocol">>),
    Body = [],

    %% When
    Response = execute_request(Host, Path, Method, Headers, Body),

    %% Then
    true = is_status_code(Response, 404).

ws_requests_xmpp(_Config) ->
    %% Given
    Host = "localhost",
    Port = 8080,
    Protocol = <<"xmpp">>,
    BinaryPing = ws_tx_frame(<<"ping">>, 2),
    BinaryPong = ws_rx_frame(<<"pong">>, 2),

    %% When
    {ok, Socket} = ws_handshake(Host, Port, Protocol),
    Responses = [begin
                ok = ws_send(Socket, BinaryPing),
                ws_recv(Socket)
            end || _ <- lists:seq(1, 50)],
    ok = gen_tcp:close(Socket),

    %% Then
    %% dummy_ws1_handler:init/3 is not called since mod_cowboy takes over
    Responses = lists:duplicate(50, BinaryPong),
    1 = meck:num_calls(dummy_ws1_handler, websocket_init, '_'),
    50 = meck:num_calls(dummy_ws1_handler, websocket_handle, '_'),
    ok = meck:wait(dummy_ws1_handler, websocket_terminate, '_', 1000).

ws_requests_other(_Config) ->
    %% Given
    Host = "localhost",
    Port = 8080,
    Protocol = <<"other">>,
    TextPing = ws_tx_frame(<<"ping">>, 1),
    TextPong = ws_rx_frame(<<"pong">>, 1),

    %% When
    {ok, Socket} = ws_handshake(Host, Port, Protocol),
    Responses = [begin
            ok = ws_send(Socket, TextPing),
            ws_recv(Socket)
        end || _ <- lists:seq(1, 50)],
    ok = gen_tcp:close(Socket),

    %% Then
    Responses = lists:duplicate(50, TextPong),
    1 = meck:num_calls(dummy_ws2_handler, websocket_init, '_'),
    50 = meck:num_calls(dummy_ws2_handler, websocket_handle, '_'),
    ok = meck:wait(dummy_ws2_handler, websocket_terminate, '_', 1000).

mixed_requests(_Config) ->
    %% Given
    Protocol1 = <<"xmpp">>,
    Protocol2 = <<"other">>,
    Protocol3 = <<"non-existent">>,

    TextPing = ws_tx_frame(<<"ping">>, 1),
    TextPong = ws_rx_frame(<<"pong">>, 1),

    Host = "localhost",
    Port = 8080,

    HTTPHost = ?SERVER,
    Path = <<"/">>,
    Method = "GET",
    Headers3 = ws_headers(Protocol3),
    Headers4 = [],
    Body = [],

    %% When
    {ok, Socket1} = ws_handshake(Host, Port, Protocol1),
    {ok, Socket2} = ws_handshake(Host, Port, Protocol2),

    Responses = [begin
                ok = ws_send(Socket1, TextPing),
                Resp1 = ws_recv(Socket1),

                Resp2 = execute_request(HTTPHost, Path, Method, Headers4, Body),
                Status2 = is_status_code(Resp2, 200),

                ok = ws_send(Socket2, TextPing),
                Resp3 = ws_recv(Socket2),

                Resp4 = execute_request(HTTPHost, Path, Method, Headers3, Body),
                Status4 = is_status_code(Resp4, 404),

                {Resp1, Status2, Resp3, Status4}
            end || _ <- lists:seq(1, 50)],

    %% Then
    Responses = lists:duplicate(50, {TextPong, true, TextPong, true}).

start_cowboy_returns_error_eaddrinuse(_C) ->
    Opts = [{port, 8088}, {ip, {127, 0, 0, 1}}, {modules, []}, {retires, {2, 10}}],
    {ok, _Pid} = ejabberd_cowboy:start_cowboy(a_ref, Opts),
    Result = ejabberd_cowboy:start_cowboy(a_ref_2, Opts),
    {error, eaddrinuse} = Result.

conf_reload(Config) ->
    %% Given initial configuration
    HTTPHost = "http://localhost:5280",
    Path = <<"/">>,
    Method = "GET",
    Headers1 = [],
    Headers2 = ws_headers(<<"xmpp">>),
    Body = [],

    copy(data(Config, "ejabberd.onlyhttp.cfg"), data(Config, "ejabberd.cfg")),
    start_ejabberd_with_config(Config, "ejabberd.cfg"),

    %% When making requests for http and ws
    Response1 = execute_request(HTTPHost, Path, Method, Headers1, Body),
    Response2 = execute_request(HTTPHost, Path, Method, Headers2, Body),

    %% Then http returns 200 and ws returns 404
    true = is_status_code(Response1, 200),
    true = is_status_code(Response2, 404),

    %% Given new configuration
    copy(data(Config, "ejabberd.onlyws.cfg"), data(Config, "ejabberd.cfg")),
    ejabberd_config:reload_local(),

    %% When making requests for http and ws
    Response3 = execute_request(HTTPHost, Path, Method, Headers1, Body),

    %% Then http returns 404 and ws works fine
    true = is_status_code(Response3, 404),

    ok = stop_ejabberd().

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
copy(Src, Dst) ->
    {ok, _} = file:copy(Src, Dst).

data(Config, Path) ->
    Dir = proplists:get_value(data_dir, Config),
    filename:join([Dir, Path]).

start_cowboy() ->
    Dispatch = cowboy_router:compile([
                {'_',
                 [{"/[...]", mod_cowboy,
                   [{http, dummy_http_handler},
                    {ws, xmpp, dummy_ws1_handler},
                    {ws, other, dummy_ws2_handler}
                   ]}]
                }]),
    {ok, _Pid} = cowboy:start_http(http_listener, 20, [{port, 8080}],
                                   [{env, [{dispatch, Dispatch}]}]).

stop_cowboy() ->
    ok = cowboy:stop_listener(http_listener).

execute_request(Host, Path, Method, Headers, Body) ->
    {ok, Pid} = fusco:start_link(Host, []),
    Response = fusco:request(Pid, Path, Method, Headers, Body, 5000),
    fusco:disconnect(Pid),
    Response.

is_status_code({ok, {{CodeBin, _}, _, _, _, _}}, Code) ->
    case binary_to_integer(CodeBin) of
        Code -> true;
        _    -> false
    end.

ws_send(Socket, Frame) ->
    ok = gen_tcp:send(Socket, Frame).

ws_recv(Socket) ->
    {ok, Packet} = gen_tcp:recv(Socket, 0, 5000),
    Packet.

ws_handshake(Host, Port, Protocol) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, raw},
                                                {active, false}]),
    ok = gen_tcp:send(Socket, [
                "GET / HTTP/1.1\r\n"
                "Host: localhost\r\n"
                "Connection: upgrade\r\n"
                "Origin: http://localhost\r\n"
                "Sec-WebSocket-Key: x3JJHMbDL1EzLkh9GBhXDw==\r\n"
                "Sec-WebSocket-Protocol: ", Protocol, "\r\n"
                "Sec-WebSocket-Version: 13\r\n"
                "Upgrade: websocket\r\n"
                "\r\n"]),
    {ok, Handshake} = gen_tcp:recv(Socket, 0, 5000),
    Packet = erlang:decode_packet(http, Handshake, []),
    {ok, {http_response, {1,1}, 101, "Switching Protocols"}, _Rest} = Packet,
    {ok, Socket}.

ws_headers(Protocol) ->
    [{<<"upgrade">>, <<"websocket">>},
     {<<"connection">>, <<"upgrade">>},
     {<<"sec-websocket-key">>, <<"x3JJHMbDL1EzLkh9GBhXDw==">>},
     {<<"sec-websocket-protocol">>, Protocol},
     {<<"sec-websocket-version">>, <<"13">>}].

ws_tx_frame(Payload, Opcode) ->
    Mask = 16#ffffffff,
    Length = byte_size(Payload),
    MaskedPayload = << <<(Byte bxor 16#ff):8>> || <<Byte:8>> <= Payload >>,
    <<1:1, 0:3, Opcode:4, 1:1, Length:7, Mask:32, MaskedPayload/binary>>.

ws_rx_frame(Payload, Opcode) ->
    Length = byte_size(Payload),
    <<1:1, 0:3, Opcode:4, 0:1, Length:7, Payload/binary>>.

%%--------------------------------------------------------------------
%% http/ws handlers mock
%%--------------------------------------------------------------------
create_handlers() ->
    Owner = self(),
    F = fun() ->
            [create_handler(Handler) || Handler <- handlers()],
            Owner ! ok,
            timer:sleep(infinity)
    end,
    Pid = spawn(F),
    receive
        ok ->
            {ok, Pid}
    after 5000 ->
            {error, timeout}
    end.

handlers() ->
    WSFuns = [{init, fun ws_init/3},
              {websocket_init, fun ws_websocket_init/3},
              {websocket_handle, fun ws_websocket_handle/3},
              {websocket_info, fun ws_websocket_info/3},
              {websocket_terminate, fun ws_websocket_terminate/3}],
    [{dummy_http_handler, [{init, fun handler_init/3},
                           {handle, fun handler_handle/2},
                           {terminate, fun handler_terminate/3}]},
     {dummy_ws1_handler, WSFuns},
     {dummy_ws2_handler, WSFuns}].

create_handler({Name, Functions}) ->
    ok = meck:new(Name, [non_strict]),
    [ok = meck:expect(Name, Function, Fun) || {Function, Fun} <- Functions].

remove_handlers(Config) ->
    [ok = meck:unload(Handler) || {Handler, _} <- handlers()],
    exit(?config(meck_pid, Config), kill).

reset_history() ->
    [ok = meck:reset(Handler) || {Handler, _} <- handlers()].

%% cowboy_http_handler
handler_init(_Type, Req, _Opts) ->
    {ok, Req, no_state}.

handler_handle(Req, State) ->
    {ok, Req1} = cowboy_req:reply(200, Req),
    {ok, Req1, State}.

handler_terminate(_Reason, _Req, _State) ->
    ok.

%% cowboy_websocket_handler
ws_init(_Type, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

ws_websocket_init(_Transport, Req, _Opts) ->
    {ok, Req, no_state}.

ws_websocket_handle({text,<<"ping">>}, Req, State) ->
    {reply, {text, <<"pong">>}, Req, State};
ws_websocket_handle({binary, <<"ping">>}, Req, State) ->
    {reply, {binary, <<"pong">>}, Req, State};
ws_websocket_handle(_Other, Req, State) ->
    {ok, Req, State}.

ws_websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

ws_websocket_terminate(_Reason, _Req, _State) ->
    ok.
