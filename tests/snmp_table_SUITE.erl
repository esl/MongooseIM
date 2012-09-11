%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

-module(snmp_table_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(WAIT_TIME, 500).

-import(snmp_helper, [assert_counter/2,
                      get_counter_value/1,
                      get_table_value/3,
                      get_next_table_value/3]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, router}].

groups() ->
    [{router, [sequence], [get_to,
                           get_num,
                           get_noexist,
                           get_next]}].

suite() ->
    [{require, ejabberd_node} | escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Router tests
%%--------------------------------------------------------------------

get_to(_Config) ->
    [{value, "localhost"}] = get_table_value([1], [1], routerRegisteredPathsTable).

get_num(_Config) ->
    [{value, 1}] = get_table_value([1], [2], routerRegisteredPathsTable).

get_noexist(_Config) ->
    Paths = escalus_ejabberd:rpc(ets, tab2list, [route]),
    N = length(Paths)+1,
    [{noValue, noSuchInstance}] = get_table_value([N], [1], routerRegisteredPathsTable),
    [{noValue, noSuchInstance}] = get_table_value([0], [1], routerRegisteredPathsTable).

get_next(_Config) ->
    Paths = escalus_ejabberd:rpc(ets, tab2list, [route]),
    Paths2 = lists:map(fun({route, Host, _}) ->
        binary_to_list(Host)
    end, Paths),
    Paths3 = collect_nexts(0, 0, []),
    [] = Paths2 -- Paths3.

collect_nexts(X, Y, Acc) ->
    case get_next_table_value([X], [Y], routerRegisteredPathsTable) of
        [{_, 1}] -> Acc;
        [{[Y1, X1], Host}] -> collect_nexts(X1, Y1, [Host | Acc])
    end.
