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
-module(monitored_map_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     no_key_test,
     put_monitors_test,
     get_test,
     remove_test,
     expire_entries_on_process_death_test,
     overwrite_entry_test,
     overwrite_entry_same_pid_test,
     find_test,
     get_default_test,
     remove_nonexistent_element_test,
     ignore_nonrelevant_info_test
    ].

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

no_key_test(_Config) ->
    Map = monitored_map:new(),
    ?assertError(_, monitored_map:get(key, Map)).

put_monitors_test(_Config) ->
    Map0 = monitored_map:new(),
    Pid = new_process(),
    _Map1 = monitored_map:put(key, value, Pid, Map0),
    assert_monitors([Pid]).

get_test(_Config) ->
    Map0 = monitored_map:new(),
    Map1 = monitored_map:put(key, value, self(), Map0),
    ?assertEqual(value, monitored_map:get(key, Map1)).

remove_test(_Config) ->
    Map0 = monitored_map:new(),
    Map1 = monitored_map:put(key, value, self(), Map0),
    Map2 = monitored_map:remove(key, Map1),
    ?assertError(_, monitored_map:get(key, Map2)),
    assert_monitors([]).

expire_entries_on_process_death_test(_Config) ->
    Map0 = monitored_map:new(),
    Pid1 = spawn(fun() -> ok end),
    Pid2 = spawn(fun() -> ok end),
    Pid3 = new_process(),

    Map1 = lists:foldl(fun({Key, Value, Pid}, M) -> monitored_map:put(Key, Value, Pid, M) end,
                       Map0, [{key1, value1, Pid1}, {key2, value2, Pid1},
                              {key3, value3, Pid2}, {key4, value4, Pid3}]),

    %% Receive 3 expiration messages: two from first process, one from second
    Map2 = lists:foldl(
             fun(_, Map) -> receive DownMsg1 -> monitored_map:handle_info(DownMsg1, Map) end end,
             Map1, lists:seq(1, 3)),

    ?assertError(_, monitored_map:get(key1, Map2)),
    ?assertError(_, monitored_map:get(key2, Map2)),
    ?assertError(_, monitored_map:get(key3, Map2)),
    ?assertEqual(value4, monitored_map:get(key4, Map2)),
    assert_monitors([Pid3]).

overwrite_entry_test(_Config) ->
    Map0 = monitored_map:new(),
    Pid = new_process(),
    Map1 = monitored_map:put(key, value1, self(), Map0),
    Map2 = monitored_map:put(key, value2, Pid, Map1),
    ?assertEqual(value2, monitored_map:get(key, Map2)),
    assert_monitors([Pid]).

overwrite_entry_same_pid_test(_Config) ->
    Map0 = monitored_map:new(),
    Map1 = monitored_map:put(key, value1, self(), Map0),
    Map2 = monitored_map:put(key, value2, self(), Map1),
    ?assertEqual(value2, monitored_map:get(key, Map2)).

find_test(_Config) ->
    Map0 = monitored_map:new(),
    Map1 = monitored_map:put(key, value, self(), Map0),
    ?assertEqual({ok, value}, monitored_map:find(key, Map1)),
    ?assertEqual(error, monitored_map:find(key2, Map1)).

get_default_test(_Config) ->
    Map0 = monitored_map:new(),
    Map1 = monitored_map:put(key, value, self(), Map0),
    ?assertEqual(value, monitored_map:get(key, Map1, default_value)),
    ?assertEqual(default_value, monitored_map:get(key2, Map1, default_value)).

remove_nonexistent_element_test(_Config) ->
    Map = monitored_map:new(),
    ?assertEqual(Map, monitored_map:remove(key, Map)).

ignore_nonrelevant_info_test(_Config) ->
    Map0 = monitored_map:new(),
    Map1 = monitored_map:put(key, value, self(), Map0),
    DownMsg = {'DOWN', make_ref(), process, self(), error},
    ?assertEqual(Map1, monitored_map:handle_info(DownMsg, Map1)),
    ?assertEqual(Map1, monitored_map:handle_info(any_other_msg, Map1)).

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

assert_monitors(Expected) ->
    {monitors, All} = process_info(self(), monitors),
    MonitoredPids = [Pid || {process, Pid} <- All],
    ?assertEqual(ordsets:from_list(Expected), ordsets:from_list(MonitoredPids)).

new_process() ->
    spawn_link(fun() -> receive Anything -> Anything end end).
