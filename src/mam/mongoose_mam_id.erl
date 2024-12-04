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

-module(mongoose_mam_id).
-author('konrad.zemek@erlang-solutions.com').

-export([next_unique/1]).
-on_load(load/0).

-spec load() -> ok.
load() ->
    case persistent_term:get(?MODULE, undefined) of
        undefined ->
            Atomic = atomics:new(1, [{signed, false}]),
            persistent_term:put(?MODULE, Atomic);
        _ ->
            ok
    end.

-spec next_unique(integer()) -> integer().
next_unique(Candidate) when is_integer(Candidate), 0 < Candidate ->
    Atomic = persistent_term:get(?MODULE),
    next_unique(Candidate, Candidate - 1, Atomic).

-spec next_unique(integer(), integer(), atomics:atomics_ref()) -> integer().
next_unique(Candidate, Current, Atomic) ->
    case atomics:compare_exchange(Atomic, 1, Current, Candidate) of
        Int when is_integer(Int), Candidate =< Int ->
            next_unique(Int + 1, Int, Atomic);
        Int when is_integer(Int) ->
            next_unique(Candidate, Int, Atomic);
        ok ->
            Candidate
    end.
