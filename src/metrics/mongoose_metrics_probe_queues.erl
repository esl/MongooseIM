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
-module(mongoose_metrics_probe_queues).
-behaviour(mongoose_metrics_probe).

-export([sample/0, datapoints/0]).

datapoints() ->
    [regular, fsm, total].

sample() ->
    {FinalNormalQueueLen, FinalFsmQueueLen} =
        lists:foldl(
            fun(Pid, {NormalQueueLen, FsmQueueLen}) ->
                    {MsgQueueLen, Dict} =
                    case erlang:process_info(Pid, [message_queue_len, dictionary]) of
                        [{message_queue_len, Msgs}, {dictionary, D}] -> {Msgs, D};
                        _ -> {0, []}
                    end,
                    FsmInternal = proplists:get_value('$internal_queue_len', Dict, 0),
                    {
                        NormalQueueLen + MsgQueueLen,
                        FsmQueueLen + FsmInternal
                    }
            end, {0, 0}, erlang:processes()),
        #{
            regular => FinalNormalQueueLen,
            fsm => FinalFsmQueueLen,
            total => FinalNormalQueueLen + FinalFsmQueueLen
        }.
