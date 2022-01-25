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

-ignore_xref([start_link/6, init/6]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Id :: mongoose_listener_config:listener_id(),
                 Module :: atom(),
                 Opts :: [any(), ...],
                 SockOpts :: [udp_listen_option()],
                 Port :: inet:port_number(),
                 IPS :: [any()]) -> any().
start_link(Id, Module, Opts, SockOpts, Port, IPS) ->
    proc_lib:start_link(?MODULE, init, [Id, Module, Opts, SockOpts, Port, IPS]).

-spec init(Id :: mongoose_listener_config:listener_id(),
           Module :: atom(),
           Opts :: [any(), ...],
           SockOpts :: [udp_listen_option()],
           Port :: inet:port_number(),
           IPS :: [any()]) -> no_return().
init(Id, Module, Opts, SockOpts, Port, IPS) ->
    case gen_udp:open(Port, [binary, {active, false}, {reuseaddr, true}
                             | SockOpts]) of
        {ok, Socket} ->
            %% Inform my parent that this port was opened succesfully
            proc_lib:init_ack({ok, self()}),
            recv_loop(Socket, Module, Opts);
        {error, Reason} ->
            ejabberd_listener:socket_error(Reason, Id, Module, SockOpts, Port, IPS)
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
            try Module:udp_recv(Socket, Addr, Port, Packet, Opts)
            catch Class:Reason:Stacktrace ->
                      ?LOG_ERROR(#{what => udp_listener_recv_failed,
                                   text => <<"Failed to process UDP packet">>,
                                   socket => Socket, handler_module => Module,
                                   ip => Addr, port => Port,
                                   class => Class, reason => Reason,
                                   stacktrace => Stacktrace, udp_packet => Packet})
            end,
            ?MODULE:recv_loop(Socket, Module, Opts);
        {error, Reason} ->
            ?LOG_ERROR(#{what => udp_listener_recv_failed,
                         text => <<"Unexpected UDP error">>,
                         socket => Socket, handler_module => Module,
                         reason => ejabberd_listener:format_error(Reason)}),
            exit({error, Reason})
    end.
