%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
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

-module(tcp_listener_SUITE).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [s2s_inet_sockname_returns_error,
     service_inet_sockname_returns_error].

suite() ->
    require_rpc_nodes([mim]).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    setup_meck(),
    Config.

end_per_suite(_Config) ->
    teardown_meck().

setup_meck() ->
    {Mod, Code} = rpc(mim(), dynamic_compile, from_string, [tcp_listener_helper_code()]),
    rpc(mim(), code, load_binary, [Mod, "tcp_listener_helper.erl", Code]),
    ok = rpc(mim(), meck, new, [mongoose_tcp_listener, [passthrough, no_link]]),
    ok = rpc(mim(), tcp_listener_helper, setup_meck, []).

teardown_meck() ->
    rpc(mim(), meck, unload, []).

tcp_listener_helper_code() ->
    "-module(tcp_listener_helper).\n"
    "-compile([export_all, nowarn_export_all]).\n"
    "setup_meck() ->\n"
    "    meck:expect(mongoose_tcp_listener, read_connection_details, fun read_connection_details/2).\n\n"
    "read_connection_details(Socket, Opts) ->\n"
    "    persistent_term:put(tcp_listener_helper_info, {Socket, self()}),\n"
    "    gen_tcp:close(Socket),\n"
    "    Result = meck:passthrough([Socket, Opts]),\n"
    "    persistent_term:put(tcp_listener_helper_result, Result),\n"
    "    Result.\n".

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

s2s_inet_sockname_returns_error(_Config) ->
    inet_sockname_returns_error(incoming_s2s_port).

service_inet_sockname_returns_error(_Config) ->
    inet_sockname_returns_error(service_port).

%% Checks that the listener does not crash if inet:sockname/1 call returns an error
inet_sockname_returns_error(PortName) ->
    Port = ct:get_config({hosts, mim, PortName}),
    {ok, ClientSocket} = gen_tcp:connect(localhost, Port, []),
    receive {tcp_closed, ClientSocket} -> gen_tcp:close(ClientSocket)
    after 5000 -> ct:fail("Socket was not closed by the server")
    end,
    {_ServerSocket, Pid} = rpc(mim(), persistent_term, get, [tcp_listener_helper_info]),
    %% The listener process is still alive
    [_|_] = rpc:pinfo(Pid),
    {error, einval} = rpc(mim(), persistent_term, get, [tcp_listener_helper_result]),
    ok.
