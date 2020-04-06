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

-module(mod_global_distrib_transport).
-author('konrad.zemek@erlang-solutions.com').

-record(?MODULE, {
           transport :: fast_tls | gen_tcp,
           socket :: fast_tls:tls_socket() | gen_tcp:socket()
          }).

-type t() :: #?MODULE{}.

-export([wrap/2, setopts/2, recv_data/2, close/1, send/2, peername/1]).
-export_types([t/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec wrap(Socket :: gen_tcp:socket(), false | [connect | false] | proplists:proplist()) ->
                  {ok, t()} | {error, any()}.
wrap(Socket, [connect | false]) ->
    wrap(Socket, false);
wrap(Socket, false) ->
    {ok, #?MODULE{transport = gen_tcp, socket = Socket}};
wrap(Socket, Opts0) ->
    Opts1 = case proplists:get_value(ciphers, Opts0) of
                undefined -> [{ciphers, "TLSv1.2:TLSv1.3"} | Opts0];
                _ -> Opts0
            end,
    case fast_tls:tcp_to_tls(Socket, Opts1) of
        {ok, TLSSocket} -> {ok, #?MODULE{transport = fast_tls, socket = TLSSocket}};
        Error -> Error
    end.

-spec setopts(t(), Opts :: proplists:proplist()) -> ok | {error, term()}.
setopts(#?MODULE{transport = gen_tcp, socket = Socket}, Opts) ->
    inet:setopts(Socket, Opts);
setopts(#?MODULE{transport = fast_tls, socket = Socket}, Opts) ->
    fast_tls:setopts(Socket, Opts).

-spec recv_data(t(), Data :: binary()) -> {ok, binary()} | {error, any()}.
recv_data(#?MODULE{transport = gen_tcp}, Data) ->
    {ok, Data};
recv_data(#?MODULE{transport = fast_tls, socket = Socket}, Data) ->
    fast_tls:recv_data(Socket, Data).

-spec close(t()) -> ok | {error, any()}.
close(#?MODULE{transport = gen_tcp, socket = Socket}) ->
    gen_tcp:close(Socket);
close(#?MODULE{transport = fast_tls, socket = Socket}) ->
    fast_tls:close(Socket).

-spec send(t(), Data :: binary()) -> ok | {error, any()}.
send(#?MODULE{transport = gen_tcp, socket = Socket}, Data) ->
    gen_tcp:send(Socket, Data);
send(#?MODULE{transport = fast_tls, socket = Socket}, Data) ->
    fast_tls:send(Socket, Data).

-spec peername(t()) -> {inet:ip_address(), inet:port_number()} | unknown.
peername(#?MODULE{transport = gen_tcp, socket = Socket}) ->
    normalize_peername(inet:peername(Socket));
peername(#?MODULE{transport = fast_tls, socket = Socket}) ->
    normalize_peername(fast_tls:peername(Socket)).

-spec normalize_peername({ok, {inet:ip_address(), inet:port_number()}} | any()) ->
    {inet:ip_address(), inet:port_number()} | unknown.
normalize_peername({ok, {IP, Port}}) when is_tuple(IP), is_integer(Port) -> {IP, Port};
normalize_peername(_Other) -> unknown.

