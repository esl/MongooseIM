%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
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
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).
-import(rest_helper, [assert_status/2, simple_request/2, simple_request/3, simple_request/4]).
-import(domain_helper, [domain/0]).
-import(mongoose_helper, [auth_modules/0]).

-define(DOMAIN, (domain())).
-define(PORT, (ct:get_config({hosts, mim, metrics_rest_port}))).
-define(USERNAME, "http_guy").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, transaction},
     {group, negative}].

groups() ->
    G = [{transaction, [{repeat_until_any_fail, 10}], [user_transaction]},
         {negative, [], negative_calls_test_cases()}
        ],
    ct_helper:repeat_all_until_all_ok(G).

negative_calls_test_cases() ->
    [
        add_malformed_user,
        add_user_without_proper_fields,
        delete_non_existent_user
    ].

init_per_suite(_Config) ->
    case is_external_auth() of
        true ->
            {skip, "users api not compatible with external authentication"};
        false ->
            [{riak_auth, is_riak_auth()}]
    end.

end_per_suite(_Config) ->
    ok.

suite() ->
    require_rpc_nodes([mim]).

%%--------------------------------------------------------------------
%% users_api tests
%%--------------------------------------------------------------------

user_transaction(Config) ->
    Count1 = fetch_list_of_users(Config),
    add_user(?USERNAME, <<"my_http_password">>),
    Count2 = fetch_list_of_users(Config),
    % add user again = change their password
    % check idempotence
    add_user(?USERNAME, <<"some_other_password">>),
    Count2 = fetch_list_of_users(Config),
    add_user("http_guy2", <<"my_http_password">>),
    Count3 = fetch_list_of_users(Config),
    delete_user(?USERNAME),
    delete_user("http_guy2"),
    Count1 = fetch_list_of_users(Config),

    ?assertEqual(Count2, Count1+1),
    ?assertEqual(Count3, Count2+1),

    wait_for_user_removal(proplists:get_value(riak_auth, Config)).

add_malformed_user(_Config) ->
    Path = unicode:characters_to_list(["/users/host/", ?DOMAIN, "/username/" ?USERNAME]),
    % cannot use jiffy here, because the JSON is malformed
    Res = simple_request(<<"PUT">>, Path, ?PORT,
                         <<"{
                        \"user\": {
                            \"password\": \"my_http_password\"
                  }">>),
    assert_status(400, Res).

add_user_without_proper_fields(_Config) ->
    Path = unicode:characters_to_list(["/users/host/", ?DOMAIN, "/username/" ?USERNAME]),
    Body = jiffy:encode(#{<<"user">> => #{<<"pazzwourd">> => <<"my_http_password">>}}),
    Res = simple_request(<<"PUT">>, Path, ?PORT, Body),
    assert_status(422, Res).

delete_non_existent_user(_Config) ->
    Path = unicode:characters_to_list(["/users/host/", ?DOMAIN, "/username/i_don_exist"]),
    Res = simple_request(<<"DELETE">>, Path, ?PORT),
    assert_status(404, Res).

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------

fetch_list_of_users(_Config) ->
    Result = simple_request(<<"GET">>, unicode:characters_to_list(["/users/host/", ?DOMAIN]), ?PORT),
    assert_status(200, Result),
    {_S, H, B} = Result,
    ?assertEqual(<<"application/json">>, proplists:get_value(<<"content-type">>, H)),
    #{<<"count">> := Count, <<"users">> := _} = B,
    ?assertEqual(2, maps:size(B)),
    Count.

add_user(UserName, Password) ->
    Path = unicode:characters_to_list(["/users/host/", ?DOMAIN, "/username/", UserName]),
    Body = jiffy:encode(#{<<"user">> => #{<<"password">> => Password}}),
    Res = simple_request(<<"PUT">>, Path, ?PORT, Body),
    assert_status(204, Res),
    Res.

delete_user(UserName) ->
    Path = unicode:characters_to_list(["/users/host/", ?DOMAIN, "/username/", UserName]),
    Res = simple_request(<<"DELETE">>, Path, ?PORT),
    assert_status(204, Res),
    Res.

is_external_auth() ->
    lists:member(ejabberd_auth_external, auth_modules()).

is_riak_auth() ->
    lists:member(ejabberd_auth_riak, auth_modules()).

wait_for_user_removal(false) ->
    ok;
wait_for_user_removal(_) ->
    Domain = domain(),
    try mongoose_helper:wait_until(
            fun() ->
                rpc(mim(), ejabberd_auth_riak, get_vh_registered_users_number, [Domain])
            end,
            0,
            #{ time_sleep => 500, time_left => 5000, name => rpc})
    of
	    {ok, 0} ->
		    ok
    catch
	_Error:Reason ->
		ct:pal("~p", [Reason]),
		ok
    end.
