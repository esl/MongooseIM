-module(async_helper).

-export([start/2, start/4]).
-export([stop_all/1]).
-export([wait_until/2, wait_until/3]).

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
    Refs = [stop(Pid) || {_, Pid} <- Helpers],
    [wait(Ref) || Ref <- Refs].

stop(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! stop,
    Ref.

wait(Ref) ->
    receive
        {'DOWN', Ref, process, _, _} ->
            ok
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
