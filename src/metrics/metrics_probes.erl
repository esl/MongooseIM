%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
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
-module(metrics_probes).

-include("mongoose_logger.hrl").

%% Samples
-export([
         do_sample_queue_lengths/0,
         do_sample_tcp_connections/0
        ]).

do_sample_queue_lengths() ->
    {FinalNormalQueueLen, FinalFsmQueueLen, FinalTotalQueueLen} =
        lists:foldl(
            fun(Pid, {NormalQueueLen, FsmQueueLen, TotalQueueLen}) ->
                 MsgQueue =
                    case erlang:process_info(Pid, message_queue_len) of
                        {_, Value} -> Value;
                        _ -> 0
                    end,
                Dict =
                    case erlang:process_info(Pid, dictionary) of
                        {_, Value0} -> Value0;
                        _ -> []
                    end,
                FsmInternal = proplists:get_value('$internal_queue_len', Dict, 0),
                {
                    NormalQueueLen + MsgQueue,
                    FsmQueueLen + FsmInternal,
                    TotalQueueLen + FsmInternal + MsgQueue
                }
            end, {0, 0, 0}, erlang:processes()),
    #{
        regular => FinalNormalQueueLen,
        fsm => FinalFsmQueueLen,
        total => FinalTotalQueueLen
    }.

do_sample_tcp_connections() ->
    OpenTcpPorts = length(port_list(name, "tcp_inet")),
    #{value => OpenTcpPorts}.

-spec port_list(Attr::atom(), term()) -> [port()].
port_list(Attr, Val) ->
    [Port || Port <- erlang:ports(),
             {Attr, Val} =:= erlang:port_info(Port, Attr)].
