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
        stop -> exit(stop_and_kill_linked_processes);
        _    -> helper_loop()
    end.

% @doc Waits `TimeLeft` for `Fun` to return `Expected Value`, then returns `ExpectedValue`
% If no value is returned or the result doesn't match  `ExpectedValue` error is raised

wait_until(Fun, ExpectedValue) ->
    wait_until(Fun, ExpectedValue, #{}).

%% Example: wait_until(fun () -> ... end, SomeVal, #{time_left => timer:seconds(2)})
wait_until(Fun, ExpectedValue, Opts) ->
    DefValidator = fun(NewValue) -> ExpectedValue =:= NewValue end,
    Defaults = #{validator => DefValidator,
                 time_left => timer:seconds(5),
                 sleep_time => 100,
                 history => []},
    do_wait_until(Fun, maps:merge(Defaults, Opts)).

do_wait_until(_Fun, #{
                time_left := TimeLeft,
                history := History
               }) when TimeLeft =< 0 ->
    error({badmatch, lists:reverse(History)});

do_wait_until(Fun, #{validator := Validator} = Opts) ->
    try Fun() of
        Value -> case Validator(Value) of
                   true -> {ok, Value};
                   _ -> wait_and_continue(Fun, Value, Opts)
               end
    catch Error:Reason ->
            wait_and_continue(Fun, {Error, Reason}, Opts)
    end.

wait_and_continue(Fun, FunResult, #{time_left := TimeLeft,
                                    sleep_time := SleepTime,
                                    history := History} = Opts) ->
    timer:sleep(SleepTime),
    do_wait_until(Fun, Opts#{time_left => TimeLeft - SleepTime,
                             history => [FunResult | History]}).

