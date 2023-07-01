-module(mod_test_global_hook).
-behaviour(gen_mod).
-export([start/2, stop/1, hooks/0, node_cleanup/3]).

start(_, Opts) ->
    ok.

stop(_) ->
    ok.

hooks() ->
    [{node_cleanup, global, fun ?MODULE:node_cleanup/3, #{}, 50}].

node_cleanup(Acc, _, _) ->
    Res = maps:get(?MODULE, Acc, 0) + 1, %% Number of times executed
    {ok, maps:put(?MODULE, Res, Acc)}.
