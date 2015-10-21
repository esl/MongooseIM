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
    [ P ! stop || {_,_,_,P} <- proplists:get_value(async_helpers, Config, []) ],
    ok.

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
