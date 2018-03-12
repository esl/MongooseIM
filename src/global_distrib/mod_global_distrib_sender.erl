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
-module(mod_global_distrib_sender).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-export([start/2, stop/1, send/2, get_process_for/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------


-spec send(jid:lserver() | pid(), {jid:jid(), jid:jid(), mongoose_acc:t(), xmlel:packet()}) -> ok.
send(Server, Packet) when is_binary(Server) ->
    Worker = get_process_for(Server),
    send(Worker, Packet);
send(Worker, {From, _To, _Acc, _Packet} = FPacket) ->
    BinPacket = term_to_binary(FPacket),
    BinFrom = mod_global_distrib_utils:recipient_to_worker_key(From, opt(global_host)),
    Data = <<(byte_size(BinFrom)):16, BinFrom/binary, BinPacket/binary>>,
    Stamp = erlang:monotonic_time(),
    ok = mod_global_distrib_utils:cast_or_call(Worker, {data, Stamp, Data}).

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(Host :: jid:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts0) ->
    Opts = [{listen_port, 5555},
            {connections_per_endpoint, 1},
            {endpoint_refresh_interval, 60},
            {disabled_gc_interval, 60} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: jid:lserver()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec start() -> any().
start() ->
    opt(tls_opts). %% Check for required tls_opts

-spec stop() -> any().
stop() ->
    ok.

-spec get_process_for(jid:lserver()) -> pid().
get_process_for(Server) ->
    mod_global_distrib_outgoing_conns_sup:get_connection(Server).

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
