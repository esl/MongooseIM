%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_service_SUITE).
-compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    [
        starts_service
    ].

services() -> [service_a, service_b].


init_per_testcase(_, C) ->
    mongoose_service:start(),
    ets:new(testservice, [named_table]),
    meck:new(services(), [non_strict]),
    lists:map(fun(S) ->
                  meck:expect(S, start, fun(_Opts) -> increment(S), done end)
              end, services()),
    lists:map(fun(S) ->
                  meck:expect(S, stop, fun() -> decrement(S) end)
              end, services()),
    C.

end_per_testcase(_, C) ->
    meck:unload(services()),
    mongoose_service:stop(),
    C.

starts_service(_C) ->
    {ok, done} = mongoose_service:start_service(service_a, []),
    {error, already_started} = mongoose_service:start_service(service_a, []),
    ?assertEqual(1, read(service_a)),
    {ok, done} = mongoose_service:start_service(service_b, []),
    {error, already_started} = mongoose_service:start_service(service_b, []),
    ?assertEqual(1, read(service_b)),
    ok = mongoose_service:stop_service(service_a),
    ?assertEqual(0, read(service_a)),
    {error, not_running} = mongoose_service:stop_service(service_a),
    ?assertEqual(0, read(service_a)),
    ok = mongoose_service:stop_service(service_b),
    ?assertEqual(0, read(service_b)),
    {error, not_running} = mongoose_service:stop_service(service_b),
    ?assertEqual(0, read(service_b)),
    ok.

increment(S) ->
    Ni = case ets:lookup(testservice, S) of
             [{S, I}] -> I + 1;
             [] -> 1
         end,
    ets:insert(testservice, {S, Ni}).

decrement(S) ->
    Ni = case ets:lookup(testservice, S) of
             [{S, I}] -> I - 1;
             [] -> -1
         end,
    ets:insert(testservice, {S, Ni}).

read(S) ->
    case ets:lookup(testservice, S) of
        [{S, I}] -> I;
        [] -> 0
    end.
