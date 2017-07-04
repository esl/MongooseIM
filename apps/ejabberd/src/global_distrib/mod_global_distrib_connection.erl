%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
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

-module(mod_global_distrib_connection).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Server :: ejabberd:lserver()) -> {ok, pid()} | {error, any()}.
start_link(Server) ->
    gen_server:start_link(?MODULE, Server, []).

init(Server) ->
    {Addr, Port} = choose_endpoint(Server),
    try
        {ok, RawSocket} = gen_tcp:connect(Addr, Port, [binary, {active, false}]),
        {ok, Socket} = mod_global_distrib_transport:wrap(RawSocket, [connect | opt(tls_opts)]),
        mod_global_distrib_transport:setopts(Socket, [{active, once}]),
        {ok, Socket}
    catch
        error:{badmatch, Reason} ->
            lager:error("Connection to ~p failed: ~p", [{Addr, Port}, Reason]),
            {stop, normal}
    end.

handle_call(Msg, From, Socket) ->
    gen_server:reply(From, ok),
    handle_cast(Msg, Socket).

handle_cast({data, Stamp, Data}, Socket) ->
    QueueTimeNative = p1_time_compat:monotonic_time() - Stamp,
    QueueTimeUS = p1_time_compat:convert_time_unit(QueueTimeNative, native, micro_seconds),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_SEND_QUEUE_TIME, QueueTimeUS),
    ClockTime = p1_time_compat:system_time(micro_seconds),
    Annotated = <<(byte_size(Data) + 8):32, ClockTime:64, Data/binary>>,
    ok = mod_global_distrib_transport:send(Socket, Annotated),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MESSAGES_SENT, 1),
    {noreply, Socket}.

handle_info({tcp, _Socket, RawData}, Socket) ->
    ok = mod_global_distrib_transport:setopts(Socket, [{active, once}]),
    %% Feeding data to drive the TLS state machine (in case of TLS connection)
    {ok, _} = mod_global_distrib_transport:recv_data(Socket, RawData),
    {noreply, Socket};
handle_info({tcp_closed, _}, Socket) ->
    mod_global_distrib_transport:close(Socket),
    {stop, normal, Socket};
handle_info(_, Socket) ->
    {noreply, Socket}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ignore.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(mod_global_distrib_sender, Key).

-spec choose_endpoint(Server :: ejabberd:lserver()) -> mod_global_distrib_utils:endpoint().
choose_endpoint(Server) ->
    {ok, Endpoints} = endpoints(Server),
    N = random:uniform(length(Endpoints)),
    lists:nth(N, Endpoints).

-spec endpoints(Server :: ejabberd:lserver()) -> {ok, [mod_global_distrib_utils:endpoint()]}.
endpoints(Server) ->
    case ejabberd_config:get_local_option({global_distrib_addr, Server}) of
        undefined -> mod_global_distrib_mapping:endpoints(Server);
        Endpoints ->
            ResolvedEndpoints = mod_global_distrib_utils:resolve_endpoints(Endpoints),
            {ok, ResolvedEndpoints}
    end.
