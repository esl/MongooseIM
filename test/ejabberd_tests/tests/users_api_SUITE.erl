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
     {group, negative}].

groups() ->
    [{transaction, [{repeat_until_any_fail, 10}], [user_transaction]},
     {negative, [], [negative_calls]}
    ].

init_per_suite(Config) ->
    case is_external_auth() of
        true ->
            {skip, "users api not compatible with external authentication"};
        false ->
            [{riak_auth, is_riak_auth()} | katt_helper:init_per_suite(Config)]
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
%%--------------------------------------------------------------------
user_transaction(Config) ->
    Params = [{host, ct:get_config(ejabberd_domain)},
              {username, "http_guy"}],
    Result = katt_helper:run(user_transaction, Config, Params),
    Vars = element(4, Result),

    UserCount1 = proplists:get_value("user_count_1", Vars),
    UserCount2 = proplists:get_value("user_count_2", Vars),
    ?assertEqual(UserCount1+1, UserCount2),
    wait_for_user_removal(proplists:get_value(riak_auth, Config)).

negative_calls(Config) ->
    Params = [{host, ct:get_config(ejabberd_domain)}],
    katt_helper:run(user_negative, Config, Params).

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
is_external_auth() ->
    lists:member(ejabberd_auth_external, auth_modules()).

is_riak_auth() ->
    lists:member(ejabberd_auth_riak, auth_modules()).

auth_modules() ->
    Hosts = escalus_ejabberd:rpc(ejabberd_config, get_global_option, [hosts]),
    lists:flatmap(
      fun(Host) ->
              escalus_ejabberd:rpc(ejabberd_auth, auth_modules, [Host])
      end, Hosts).

wait_for_user_removal(false) ->
    ok;
wait_for_user_removal(_) ->
    do_wait_for_user_removal(10).

do_wait_for_user_removal(0) ->
    ok;
do_wait_for_user_removal(N) ->
    Domain = ct:get_config(ejabberd_domain),
    case escalus_ejabberd:rpc(ejabberd_auth_riak, get_vh_registered_users_number, [Domain]) of
        0 ->
            ok;
        _ ->
            timer:sleep(500),
            do_wait_for_user_removal(N-1)
    end.