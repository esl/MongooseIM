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

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, fusco},
     {group, gun_http1},
     {group, gun_http2}].

groups() ->
    [{fusco, [], [pool_timeout_test | tests()]},
     {gun_http1, [], tests()},
     {gun_http2, [], tests()}].

tests() ->
    [get_test,
     no_pool_test,
     post_test,
     request_timeout_test,
     multiple_requests_test
    ].

init_per_suite(Config) ->
    http_helper:start(8080, '_', fun process_request/1),
    Pid = self(),
    spawn(fun() ->
                  register(test_helper, self()),
                  mim_ct_sup:start_link(ejabberd_sup),
                  mongoose_wpool:ensure_started(),
                  Pid ! ready,
                  receive stop -> ok end
          end),
    receive ready -> ok end,
    Config.

process_request(Req) ->
    QS = cowboy_req:parse_qs(Req),
    case proplists:get_value(<<"sleep">>, QS) of
        <<"true">> -> timer:sleep(100);
        _ -> ok
    end,
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"OK">>, Req).

end_per_suite(_Config) ->
    http_helper:stop(),
    exit(whereis(ejabberd_sup), shutdown),
    whereis(test_helper) ! stop.

init_per_group(fusco, Config) ->
    [{connection_opts, [{server, {"http://localhost", 8080}}, {http_client, fusco}]} | Config];
init_per_group(gun_http1, Config) ->
    application:ensure_all_started(gun),
    [{connection_opts, [{server, {"127.0.0.1", 8080}}, {http_client, gun}]} | Config];
init_per_group(gun_http2, Config) ->
    application:ensure_all_started(gun),
    [{connection_opts, [{server, {"127.0.0.1", 8080}},
                        {http_client, gun},
                        {http_opts, #{protocols => [http2]}}]} | Config].

end_per_group(_, Config) ->
    Config.

init_per_testcase(request_timeout_test, Config) ->
    ConnOpts = proplists:get_value(connection_opts, Config),
    mongoose_wpool:start_configured_pools([{http, global, pool(), [],
                                            [{request_timeout, 10} | ConnOpts]}],
                                          [<<"a.com">>]),
    Config;
init_per_testcase(pool_timeout_test, Config) ->
    ConnOpts = proplists:get_value(connection_opts, Config),
    mongoose_wpool:start_configured_pools([{http, global, pool(),
                                            [{workers, 1},
                                             {max_overflow, 0},
                                             {strategy, available_worker},
                                             {call_timeout, 50}],
                                            ConnOpts}],
                                          [<<"a.com">>]),
    Config;
init_per_testcase(_TC, Config) ->
    ConnOpts = proplists:get_value(connection_opts, Config),
    mongoose_wpool:start_configured_pools([{http, global, pool(), [],
                                            ConnOpts}],
                                          [<<"a.com">>]),
    Config.

end_per_testcase(_TC, _Config) ->
    mongoose_wpool:stop(http, global, pool()).

get_test(_Config) ->
    Result = mongoose_http_client:get(global, pool(), <<"some/path">>, []),
    {ok, {<<"200">>, <<"OK">>}} = Result.

no_pool_test(_Config) ->
    Result = mongoose_http_client:get(global, non_existent_pool, <<"some/path">>, []),
    {error, pool_not_started} = Result.

post_test(_Config) ->
    Result = mongoose_http_client:post(global, pool(), <<"some/path">>, [], <<"test request">>),
    {ok, {<<"200">>, <<"OK">>}} = Result.

request_timeout_test(_Config) ->
    Result = mongoose_http_client:get(global, pool(), <<"some/path?sleep=true">>, []),
    {error, request_timeout} = Result.

pool_timeout_test(_Config) ->
    Pid = self(),
    spawn(fun() ->
                  mongoose_http_client:get(global, pool(), <<"/some/path?sleep=true">>, []),
                  Pid ! finished
          end),
    timer:sleep(10), % wait for the only pool worker to start handling the request
    Result = mongoose_http_client:get(global, pool(), <<"some/path">>, []),
    {error, pool_timeout} = Result,
    receive finished -> ok after 1000 -> error(no_finished_message) end.

multiple_requests_test(_Config) ->
    Pid = self(),
    N = 5,
    [spawn(fun() ->
        Result = mongoose_http_client:get(global, pool(), <<"some/path">>, []),
        Pid ! Result
        end) || _ <- lists:seq(1,N)],
    receive_results(N).

receive_results(0) ->
    ok;
receive_results(N) ->
    receive
        Result ->
            {ok, {<<"200">>, <<"OK">>}} = Result,
            receive_results(N-1)
    after 100 ->
        error(results_timeout)
    end.

pool() -> tmp_pool.
