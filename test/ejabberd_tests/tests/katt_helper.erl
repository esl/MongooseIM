%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
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
-module(katt_helper).

%% Suite configuration
-export([init_per_suite/1,
         end_per_suite/1]).

%% API
-export([run/2,
         run/3]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration helpers
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    [{apps_started, start_apps([katt], [])} | Config].

end_per_suite(Config) ->
    [application:stop(App) || App <- ?config(apps_started, Config)],
    ok.

run(BlueprintName, Config) ->
    run(BlueprintName, Config, []).

run(BlueprintName, Config, Params) ->
    Blueprint = blueprint(BlueprintName, Config),
    Params1 = [{hostname, "localhost"},
               {port, ct:get_config(ejabberd_metrics_rest_port)}],
    Params2 = lists:ukeymerge(1, lists:keysort(1, Params), lists:keysort(1, Params1)),
    {TestResult, _, _, _, TransResults} = Result = katt:run(Blueprint, Params2),
    case TestResult of
        pass ->
            Result;
        fail ->
            FailedTrans = failed_transactions(TransResults),
            ct:pal("Failed transactions:~n~p~n", [FailedTrans]),
            ct:fail(katt_transactions_failed)
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
start_apps([], Acc) ->
    Acc;
start_apps([App|Tail]=All, Acc) ->
    case application:start(App) of
        ok ->
            start_apps(Tail, [App|Acc]);
        {error, {already_started, App}} ->
            start_apps(Tail, Acc);
        {error, {not_started, Dep}} ->
            start_apps([Dep|All], Acc)
    end.

blueprint(Name, Config) ->
    File = atom_to_list(Name) ++ ".apib",
    filename:join([?config(data_dir, Config), File]).

failed_transactions(Results) ->
    lists:filtermap(fun
            ({Description, _, _, _, {fail, Reason}}) ->
                {true, {Description, Reason}};
            (_) ->
                false
        end, Results).
