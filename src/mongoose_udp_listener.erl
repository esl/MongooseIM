%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
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

-module(mongoose_udp_listener).
-author('konrad.zemek@erlang-solutions.com').

-include("mongoose.hrl").

-type udp_listen_option() :: gen_udp:option()
                           | {ip, _}
                           | {fd, pos_integer()}
                           | {ifaddr, _}
                           | inet:address_family()
                           | {port, inet:port_number()}.

-export([start_link/6, init/6]).

%% Internal
-export([recv_loop/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(PortIPProto :: ejabberd_listener:port_ip_proto(),
                 Module :: atom(),
                 Opts :: [any(), ...],
                 SockOpts :: [udp_listen_option()],
                 Port :: inet:port_number(),
                 IPS :: [any()]) -> any().
start_link(PortIP, Module, Opts, SockOpts, Port, IPS) ->
    proc_lib:start_link(?MODULE, init, [PortIP, Module, Opts, SockOpts, Port, IPS]).

-spec init(PortIPProto :: ejabberd_listener:port_ip_proto(),
           Module :: atom(),
           Opts :: [any(), ...],
           SockOpts :: [udp_listen_option()],
           Port :: inet:port_number(),
           IPS :: [any()]) -> no_return().
init(PortIP, Module, Opts, SockOpts, Port, IPS) ->
    case gen_udp:open(Port, [binary, {active, false}, {reuseaddr, true}
                             | SockOpts]) of
        {ok, Socket} ->
            %% Inform my parent that this port was opened succesfully
            proc_lib:init_ack({ok, self()}),
            recv_loop(Socket, Module, Opts);
        {error, Reason} ->
            ejabberd_listener:socket_error(Reason, PortIP, Module,
                                           SockOpts, Port, IPS)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec recv_loop(Socket :: port(),
                Module :: atom(),
                Opts :: [any(), ...]) -> no_return().
recv_loop(Socket, Module, Opts) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {Addr, Port, Packet}} ->
            case catch Module:udp_recv(Socket, Addr, Port, Packet, Opts) of
                {'EXIT', Reason} ->
                    ?ERROR_MSG("failed to process UDP packet:~n"
                               "** Source: {~p, ~p}~n"
                               "** Reason: ~p~n** Packet: ~p",
                               [Addr, Port, Reason, Packet]);
                _ ->
                    ok
            end,
            ?MODULE:recv_loop(Socket, Module, Opts);
        {error, Reason} ->
            ?ERROR_MSG("unexpected UDP error: ~s",
                       [ejabberd_listener:format_error(Reason)]),
            exit({error, Reason})
    end.
