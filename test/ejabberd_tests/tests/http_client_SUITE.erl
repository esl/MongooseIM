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
-module(http_client_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [get_test,
     post_test,
     request_timeout_test,
     pool_timeout_test
    ].

init_per_suite(Config) ->
    ConfigWithModules = dynamic_modules:save_modules(domain(), Config),
    dynamic_modules:ensure_modules(domain(), required_modules()),
    http_helper:start(8080, '_', fun process_request/1),
    ConfigWithModules.

process_request(Req) ->
    {Sleep, Req1} = cowboy_req:qs_val(<<"sleep">>, Req),
    case Sleep of
        <<"true">> -> timer:sleep(100);
        _ -> ok
    end,
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"OK">>, Req1),
    Req2.

end_per_suite(Config) ->
    http_helper:stop(),
    dynamic_modules:restore_modules(domain(), Config).

init_per_testcase(request_timeout_test, Config) ->
    call(start_pool, [domain(), tmp_pool, [{host, "http://localhost:8080"},
                                           {request_timeout, 10}]]),
    Config;
init_per_testcase(pool_timeout_test, Config) ->
    call(start_pool, [domain(), tmp_pool, [{host, "http://localhost:8080"},
                                           {pool_size, 1},
                                           {max_overflow, 0},
                                           {pool_timeout, 10}]]),
    Config;
init_per_testcase(_TC, Config) ->
    call(start_pool, [domain(), tmp_pool, [{host, "http://localhost:8080"}]]),
    Config.

end_per_testcase(_TC, _Config) ->
    call(stop_pool, [domain(), tmp_pool]).

get_test(_Config) ->
    Pool = call(get_pool, [domain(), tmp_pool]),
    Result = call(get, [Pool, <<"/some/path">>, []]),
    {ok, {<<"200">>, <<"OK">>}} = Result.

post_test(_Config) ->
    Pool = call(get_pool, [domain(), tmp_pool]),
    Result = call(post, [Pool, <<"/some/path">>, [], <<"test request">>]),
    {ok, {<<"200">>, <<"OK">>}} = Result.

request_timeout_test(_Config) ->
    Pool = call(get_pool, [domain(), tmp_pool]),
    Result = call(get, [Pool, <<"/some/path?sleep=true">>, []]),
    {error, request_timeout} = Result.

pool_timeout_test(_Config) ->
    Pool = call(get_pool, [domain(), tmp_pool]),
    spawn_link(fun() -> call(get, [Pool, <<"/some/path?sleep=true">>, []]) end),
    timer:sleep(10), % wait for the only pool worker to start handling the request
    Result = call(get, [Pool, <<"/some/path">>, []]),
    {error, pool_timeout} = Result.

required_modules() ->
    [{mod_http_client, []}].

call(F, Args) ->
    escalus_ejabberd:rpc(mod_http_client, F, Args).

domain() ->
    ct:get_config({hosts, mim, domain}).




