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
           transport :: ssl | gen_tcp,
           socket :: ssl:sslsocket() | gen_tcp:socket()
          }).

-type t() :: #?MODULE{}.

-export([wrap/3, setopts/2, close/1, send/2, peername/1]).
-export_type([t/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec wrap(gen_tcp:socket(), #{tls := just_tls:options()}, client | server) ->
    {ok, t()} | {error, any()}.
wrap(Socket, #{tls := Opts}, ClientOrServer) ->
    inet:setopts(Socket, [{active, false}]),
    case just_tls:tcp_to_tls(Socket, Opts, ClientOrServer) of
        {ok, TLSSocket} -> {ok, #?MODULE{transport = ssl, socket = TLSSocket}};
        Error -> Error
    end;
wrap(Socket, #{}, _ExtraOpts) ->
    {ok, #?MODULE{transport = gen_tcp, socket = Socket}}.

-spec setopts(t(), Opts :: proplists:proplist()) -> ok | {error, term()}.
setopts(#?MODULE{transport = gen_tcp, socket = Socket}, Opts) ->
    inet:setopts(Socket, Opts);
setopts(#?MODULE{transport = ssl, socket = Socket}, Opts) ->
    ssl:setopts(Socket, Opts).

-spec close(t()) -> ok | {error, any()}.
close(#?MODULE{transport = gen_tcp, socket = Socket}) ->
    gen_tcp:close(Socket);
close(#?MODULE{transport = ssl, socket = Socket}) ->
    ssl:close(Socket).

-spec send(t(), Data :: binary()) -> ok | {error, any()}.
send(#?MODULE{transport = gen_tcp, socket = Socket}, Data) ->
    gen_tcp:send(Socket, Data);
send(#?MODULE{transport = ssl, socket = Socket}, Data) ->
    ssl:send(Socket, Data).

-spec peername(t()) -> mongoose_transport:peer() | unknown.
peername(#?MODULE{transport = gen_tcp, socket = Socket}) ->
    normalize_peername(inet:peername(Socket));
peername(#?MODULE{transport = ssl, socket = Socket}) ->
    normalize_peername(ssl:peername(Socket)).

-spec normalize_peername({ok, mongoose_transport:peer()} | any()) ->
    mongoose_transport:peer() | unknown.
normalize_peername({ok, {IP, Port}}) when is_tuple(IP), is_integer(Port) -> {IP, Port};
normalize_peername(_Other) -> unknown.
