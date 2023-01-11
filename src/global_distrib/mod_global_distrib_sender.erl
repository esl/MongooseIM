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

-behaviour(mongoose_module_metrics).

-include("mongoose.hrl").

-export([send/2, get_process_for/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec send(jid:lserver() | pid(), {jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}) -> ok.
send(Server, {_,_, Acc, _} = Packet) when is_binary(Server) ->
    try get_process_for(Server) of
        Worker ->
           send(Worker, Packet)
    catch Class:Reason:Stacktrace ->
              ?LOG_ERROR(#{what => gd_get_process_for_failed, server => Server, acc => Acc,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
              erlang:raise(Class, Reason, Stacktrace)
    end;
send(Worker, {From, _To, _Acc, _Packet} = FPacket) ->
    BinPacket = term_to_binary(FPacket),
    BinFrom = mod_global_distrib_utils:recipient_to_worker_key(From, opt(global_host)),
    Data = <<(byte_size(BinFrom)):16, BinFrom/binary, BinPacket/binary>>,
    Stamp = erlang:monotonic_time(),
    ok = mod_global_distrib_utils:cast_or_call(Worker, {data, Stamp, Data}).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec get_process_for(jid:lserver()) -> pid().
get_process_for(Server) ->
    mod_global_distrib_outgoing_conns_sup:get_connection(Server).

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(mod_global_distrib, Key).
