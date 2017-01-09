%%==============================================================================
%% Copyright 2013 Erlang Solutions Ltd.
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

-module(metrics_register_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(WAIT_TIME, 500).
-define(RT_WAIT_TIME, 60000).

-import(metrics_helper, [assert_counter/2,
                      get_counter_value/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, registration}].

groups() ->
    [{registration, [sequence], [register,
                                 unregister]}].

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
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(unregister, Config) ->
    Alice = escalus_users:get_user_by_name(alice),
    escalus_users:create_user(Config, Alice),
    Config;
init_per_testcase(registered_users, Config) ->
    XMPPDomain = ct:get_config({hosts, mim, domain}),
    case escalus_ejabberd:rpc(ejabberd_config, get_local_option,
                              [{auth_method, XMPPDomain}]) of
        external ->
            {skip, "counter not supported with ejabberd_auth_external"};
        anonymous ->
            {skip, "counter not supported with anonymous authentication"};
        _ ->
            Config
    end;
init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(unregister, _Config) ->
    ok;
end_per_testcase(_CaseName, Config) ->
    Alice = escalus_users:get_user_by_name(alice),
    escalus_users:delete_user(Config, Alice).

%%--------------------------------------------------------------------
%% Registration tests
%%--------------------------------------------------------------------

register(Config) ->
    {value, Registarations} = get_counter_value(modRegisterCount),

    Alice = escalus_users:get_user_by_name(alice),
    escalus_users:create_user(Config, Alice),

    assert_counter(Registarations + 1, modRegisterCount).


unregister(Config) ->
    {value, Deregistarations} = get_counter_value(modUnregisterCount),

    Alice = escalus_users:get_user_by_name(alice),
    escalus_users:delete_user(Config, Alice),

    assert_counter(Deregistarations + 1, modUnregisterCount).
