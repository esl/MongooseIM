-module(async_helper).
-compile([export_all]).

start(Config, M, F, A) ->
    {ok, P} = start(M, F, A),
    Helpers = [{M, F, A, P} | proplists:get_value(async_helpers, Config, [])],
    lists:keystore(async_helpers, 1, Config, {async_helpers, Helpers}).

start(Config, MFAs) when is_list(MFAs) ->
    lists:foldl(fun ({M,F,A}, ConfigAcc) ->
                        start(ConfigAcc, M, F, A)
                end, Config, MFAs).

stop_all(Config) ->
    Helpers = proplists:get_value(async_helpers, Config, []),
    Refs = [ monitor_and_stop(P) || {_,_,_,P} <- Helpers ],
    [ receive_down_message(R) || R <- Refs ],
    ok.

monitor_and_stop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! stop,
    Ref.

receive_down_message(Ref) ->
    receive
        {'DOWN', Ref, process, _, _} -> ok
    end.

start(M, F, A) ->
    Self = self(),
    P = spawn(fun () ->
                      erlang:apply(M, F, A),
                      Self ! started,
                      helper_loop()
              end),
    receive
        started ->
            %ct:pal("started", []),
            {ok, P}
    after timer:seconds(1) ->
              ct:fail("async start timeout")
    end.

helper_loop() ->
    receive
        stop -> exit(normal);
        _    -> helper_loop()
    end.
