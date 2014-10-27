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

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, routing}].

groups() ->
    [{routing, [sequence], [http_requests,
                            ws_request_bad_protocol,
                            ws_requests_xmpp]}].

suite() ->
    [].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

-define(APPS, [crypto, ssl, fusco, ranch, cowlib, cowboy]).

init_per_suite(Config) ->
    [application:start(App) || App <- ?APPS],
    {ok, Pid} = create_handlers(),
    start_cowboy(),
    [{meck_pid, Pid}|Config].

end_per_suite(Config) ->
    remove_handlers(Config),
    stop_cowboy(),
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, Config) ->
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
    Host = ?SERVER,
    Path = <<"/">>,
    Method = "GET",
    Headers = ws_headers(<<"xmpp">>),
    Body = [],

    %% When
    Response = execute_request(Host, Path, Method, Headers, Body),

    %% Then
    true = is_status_code(Response, 101).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
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

ws_headers(Protocol) ->
    [{<<"upgrade">>, <<"websocket">>},
     {<<"connection">>, <<"upgrade">>},
     {<<"sec-websocket-key">>, <<"x3JJHMbDL1EzLkh9GBhXDw==">>},
     {<<"sec-websocket-protocol">>, Protocol},
     {<<"sec-websocket-version">>, <<"13">>}].

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
    exit(?config(meck_pid, Config), kill),
    [meck:unload(Handler) || {Handler, _} <- handlers()].

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

ws_websocket_handle(<<"ping">>, Req, State) ->
    {reply, <<"pong">>, Req, State};
ws_websocket_handle(_Other, Req, State) ->
    {ok, Req, State}.

ws_websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

ws_websocket_terminate(_Reason, _Req, _State) ->
    ok.
