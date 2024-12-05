-module(async_helper).

-export([start/2, start/4]).
-export([stop_all/1]).

start(Config, MFAs) ->
    lists:foldl(
        fun({M, F, A}, Acc) -> start(Acc, M, F, A) end,
        Config,
        MFAs).

start(Config, M, F, A) ->
    {ok, Pid} = start_it({M, F, A}, [test_server_loc]),
    Helpers = proplists:get_value(?MODULE, Config, []),
    NewHelpers = [{{M, F, A}, Pid} | Helpers],
    lists:keystore(?MODULE, 1, Config, {?MODULE, NewHelpers}).

start_it({M, F, A}, Keys) ->
    Dict = [{Key, get(Key)} || Key <- Keys],
    Self = self(),
    Pid = spawn(
        fun() ->
            put(?MODULE, Dict),
            erlang:apply(M, F, A),
            Self ! started,
            loop()
        end),
    receive
        started -> {ok, Pid}
    after timer:seconds(1) ->
        ct:fail("async start timed out on ~p", [{M, F, A}])
    end.

loop() ->
    receive
        stop -> exit(stop);
        _ -> loop()
    end.

stop_all(Config) ->
    Helpers = proplists:get_value(?MODULE, Config, []),
    Refs = lists:flatmap(fun({_, Pid}) -> stop(Pid) end, Helpers),
    [wait(Ref) || Ref <- Refs].

stop(Pid) ->
    Refs = [monitor(process, P) || P <- [Pid | links(Pid)]],
    Pid ! stop,
    Refs.

links(Pid) ->
    case erlang:process_info(Pid, links) of
        undefined -> [];
        {links, Links} -> Links
    end.

wait(Ref) ->
    receive
        {'DOWN', Ref, process, _, _} ->
            ok
    end.
