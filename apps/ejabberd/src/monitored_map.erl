%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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

-module(monitored_map).
-author('konrad.zemek@erlang-solutions.com').

-export([new/0, put/4, remove/2, find/2, get/2, get/3, handle_info/2]).

-record(monitored_map, {
          map = #{} :: #{term() => {term(), pid(), reference()}},
          monitors = #{} :: #{reference() => term()}
         }).

-type t() :: #monitored_map{}.
-type t(KeyT, ValueT) :: #monitored_map{map :: #{KeyT => {ValueT, pid(), reference()}},
                                        monitors :: #{reference() => KeyT}}.

-export_type([t/0, t/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec new() -> t().
new() ->
    #monitored_map{}.

-spec put(Key :: term(), Value :: term(), Pid :: pid(), MonMap :: t()) -> t().
put(Key, Value, Pid, #monitored_map{map = Map, monitors = Monitors} = MonMap) ->
    case maps:get(Key, Map, {undefined, undefined, make_ref()}) of
        {_, Pid, Ref} ->
            NewMap = maps:put(Key, {Value, Pid, Ref}, Map),
            MonMap#monitored_map{map = NewMap};

        {_, _, OldRef} ->
            demonitor(OldRef),
            Ref = monitor(process, Pid),
            NewMonitors = maps:put(Ref, Key, maps:remove(Ref, Monitors)),
            NewMap = maps:put(Key, {Value, Pid, Ref}, Map),
            MonMap#monitored_map{map = NewMap, monitors = NewMonitors}
    end.

-spec remove(Key :: term(), MonMap :: t()) -> t().
remove(Key, #monitored_map{map = Map, monitors = Monitors} = MonMap) ->
    case maps:find(Key, Map) of
        {ok, {_Value, _Pid, Ref}} ->
            demonitor(Ref),
            NewMap = maps:remove(Key, Map),
            NewMonitors = maps:remove(Ref, Monitors),
            MonMap#monitored_map{map = NewMap, monitors = NewMonitors};
        _ ->
            MonMap
    end.

-spec find(Key :: term(), MonMap :: t()) -> {ok, term()} | error.
find(Key, #monitored_map{map = Map}) ->
    case maps:find(Key, Map) of
        {ok, {Value, _, _}} -> {ok, Value};
        Other -> Other
    end.

-spec get(Key :: term(), MonMap :: t()) -> term() | no_return().
get(Key, #monitored_map{map = Map}) ->
    {Value, _, _} = maps:get(Key, Map),
    Value.

-spec get(Key :: term(), MonMap :: t(), Default :: term()) -> term().
get(Key, #monitored_map{map = Map}, Default) ->
    {Value, _, _} = maps:get(Key, Map, {Default, undefined, undefined}),
    Value.

-spec handle_info(Message :: term(), MonMap :: t()) -> t().
handle_info({'DOWN', MonitorRef, _Type, _Object, _Info},
            #monitored_map{map = Map, monitors = Monitors} = MonMap) ->
    case maps:find(MonitorRef, Monitors) of
        {ok, Key} ->
            NewMap = maps:remove(Key, Map),
            NewMonitors = maps:remove(MonitorRef, Monitors),
            MonMap#monitored_map{map = NewMap, monitors = NewMonitors};
        error ->
            MonMap
    end;
handle_info(_, MonMap) ->
    MonMap.
