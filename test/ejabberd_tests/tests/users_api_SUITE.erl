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
-module(users_api_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, transaction},
     {group, negative},
     {group, utf}].   

groups() ->
    [{transaction, [{repeat_until_any_fail, 10}], [user_transaction ]},
     {negative, [], [negative_calls]},
     {utf, [], [user_utf]}
    ].

init_per_suite(Config) ->
    case is_external_auth() of
        true ->
            {skip, "users api not compatible with external authentication"};
        false ->
            katt_helper:init_per_suite(Config)
    end.

end_per_suite(Config) ->
    katt_helper:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% users_api tests
%%
%% NOTE:  In case all tests fail remove everything inside ebin/ directory
%%--------------------------------------------------------------------
user_transaction(Config) ->
    Params = [{host, ct:get_config(ejabberd_domain)},
              {username, "http_guy"}],
    Result = katt_helper:run(user_transaction, Config, Params),
    Vars = element(4, Result),
    UserCount1 = proplists:get_value("user_count_1", Vars),
    UserCount2 = proplists:get_value("user_count_2", Vars),
    
    ?assertEqual(UserCount1+1, UserCount2).

user_utf(Config) ->
    Params = [{host, ct:get_config(ejabberd_domain)},
              {username, <<"utfguy">>},
              {password, <<34,"\\ud835","\\udf15",34>>}], % <<utf16>>,
    Result = katt_helper:run(user_utf, Config, Params),
    Vars = element(4, Result),
    UserCount1 = proplists:get_value("user_count_1", Vars),
    UserCount2 = proplists:get_value("user_count_2", Vars),
    ?assertEqual(UserCount1+1, UserCount2).


negative_calls(Config) ->
    Params = [{host, ct:get_config(ejabberd_domain)}],
    katt_helper:run(user_negative, Config, Params).

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
is_external_auth() ->
    lists:member(ejabberd_auth_external, auth_modules()).

auth_modules() ->
    Hosts = escalus_ejabberd:rpc(ejabberd_config, get_global_option, [hosts]),
    lists:flatmap(
      fun(Host) ->
              escalus_ejabberd:rpc(ejabberd_auth, auth_modules, [Host])
      end, Hosts).
