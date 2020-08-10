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
-include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, gun_http1},
     {group, gun_http2}].

groups() ->
    [{gun_http1, [], tests()},
     {gun_http2, [], tests()},
     {gunworker, [], [terminating_correctly]}].

tests() ->
    [get_test,
     no_pool_test,
     post_test,
     request_timeout_test,
     multiple_requests_test,
     unstable_connection_test
    ].

init_per_suite(Config) ->
    application:ensure_all_started(gun),
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

init_per_group(gun_http1, Config) ->
    [{connection_opts, [{server, {"127.0.0.1", 8080}}]} | Config];
init_per_group(gun_http2, Config) ->
    [{connection_opts, [{server, {"127.0.0.1", 8080}},
                        {http_opts, #{protocols => [http2]}}]} | Config];
init_per_group(gunworker, Config) ->
    Server = {"127.0.0.1", 8080},
    Opts = #{protocols => [http2]},
    [{server, Server}, {connection_opts, Opts} | Config].

end_per_group(_, Config) ->
    Config.

init_per_testcase(request_timeout_test, Config) ->
    ConnOpts = proplists:get_value(connection_opts, Config),
    mongoose_wpool:start_configured_pools([{http, global, pool(), [],
                                            [{workers, 5}, {request_timeout, 10} | ConnOpts]}],
                                          [<<"a.com">>]),
    Config;
init_per_testcase(unstable_connection_test, Config) ->
    ConnOpts = proplists:get_value(connection_opts, Config, #{}),
    HttpOpts = proplists:get_value(http_opts, ConnOpts, #{}),

    NewConnOpts = [{http_opts, HttpOpts#{connect_timeout => 500,
                                         retry => 1,
                                         retry_timeout => 500}} | ConnOpts],
    mongoose_wpool:start_configured_pools([{http, global, pool(), [],
                                            NewConnOpts}],
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
init_per_testcase(terminating_correctly, Config) ->
    Config;
init_per_testcase(_TC, Config) ->
    ConnOpts = proplists:get_value(connection_opts, Config),
    mongoose_wpool:start_configured_pools([{http, global, pool(), [],
                                            ConnOpts}],
                                          [<<"a.com">>]),
    Config.

end_per_testcase(terminating_correctly, _Config) ->
    ok;
end_per_testcase(_TC, _Config) ->
    mongoose_wpool:stop(http, global, pool()).

terminating_correctly(Config) ->
    Server = ?config(server, Config),
    Opts = ?config(connection_opts, Config),
    gen_server:start({local, gun_worker}, mongoose_gun_worker, {Server, Opts}, []),
    Pid = self(),
    N = 5,
    [spawn(fun() ->
                   {ok, {{Code, _}, _, RespBody, _, _}} =
                       gen_server:call(gun_worker, {request, <<"/some/path">>, <<"GET">>, [], <<>>, 1, 5000}),
                   R = {ok, {Code, RespBody}},
                   Pid ! R
           end) || _ <- lists:seq(1, N)],
    timer:sleep(50),
    [spawn(fun() ->
            R = gen_server:call(
                  gun_worker, {request, <<"/some/path?sleep=true">>, <<"GET">>, [], <<>>, 1, 5000}),
            Pid ! R
           end) || _ <- lists:seq(1, N)],
    ?assertEqual(ok, gen_server:stop(gun_worker, shutdown, 5000)),
    receive_results(N, {ok, {<<"200">>, <<"OK">>}}),
    receive_results(N, {error, request_timeout}).

get_test(_Config) ->
    Result = mongoose_http_client:get(global, pool(), <<"some/path">>, []),
    ?assertEqual({ok, {<<"200">>, <<"OK">>}}, Result).

no_pool_test(_Config) ->
    Result = mongoose_http_client:get(global, non_existent_pool, <<"some/path">>, []),
    ?assertEqual({error, pool_not_started}, Result).

post_test(_Config) ->
    Result = mongoose_http_client:post(global, pool(), <<"some/path">>, [], <<"test request">>),
    ?assertEqual({ok, {<<"200">>, <<"OK">>}}, Result).

request_timeout_test(_Config) ->
    Result = mongoose_http_client:get(global, pool(), <<"some/path?sleep=true">>, []),
    ?assertEqual({error, request_timeout}, Result).

unstable_connection_test(_Config) ->
    Pid = self(),
    http_helper:stop(),
    spawn(fun() ->
                  R = mongoose_http_client:get(global, pool(), <<"some/path?sleep=true">>, []),
                  Pid ! R
          end),
    timer:sleep(10),
    http_helper:start(8080, '_', fun process_request/1),

    Result = receive R -> R after 5000 -> error(no_finished_message) end,
    ?assertEqual({ok, {<<"200">>, <<"OK">>}}, Result).

pool_timeout_test(_Config) ->
    Pid = self(),
    spawn(fun() ->
                  mongoose_http_client:get(global, pool(), <<"some/path?sleep=true">>, []),
                  Pid ! finished
          end),
    timer:sleep(10), % wait for the only pool worker to start handling the request
    Result = mongoose_http_client:get(global, pool(), <<"some/path">>, []),
    ?assertEqual({error, pool_timeout}, Result),
    receive finished -> ok after 1000 -> error(no_finished_message) end.

multiple_requests_test(_Config) ->
    Pid = self(),
    N = 5,
    [spawn(fun() ->
        Result = mongoose_http_client:get(global, pool(), <<"some/path">>, []),
        Pid ! Result
        end) || _ <- lists:seq(1, N)],
    receive_results(N, {ok, {<<"200">>, <<"OK">>}}).

receive_results(0, _) ->
    ok;
receive_results(N, Expected) ->
    receive
        Expected ->
            receive_results(N - 1, Expected)
    after 100 ->
        error({results_timeout, N, Expected})
    end.

pool() -> tmp_pool.
