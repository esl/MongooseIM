-define(assertReceivedMatch(Expect), ?assertReceivedMatch(Expect, timer:seconds(5))).
-define(assertReceivedMatch(Expect, Timeout),
    ((fun() ->
        receive
            Expect = __Result__ -> __Result__
        after
            Timeout ->
                __Reason__ =
                    receive
                        __Result__ -> __Result__
                    after
                        0 -> timeout
                    end,

                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, __Reason__}],
                ct:print("assertReceivedMatch_failed: ~p~n", [__Args__]),
                erlang:error({assertReceivedMatch_failed, __Args__})
        end
      end)())).
