-module(spamctl_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    [ burst_ctl ].

init_per_suite(C) ->
    application:ensure_all_started(lager),
    C.

end_per_suite(_C) ->
    ok.

burst_ctl(_C) ->
    % initialise with 10 msgs over 2 seconds
    State = #{maxrate => 10,
            span => 2,
            rate => 0,
            decision => ok,
            lasttime => now_to_usec()},
    State1 = proc_msgs(State, 8),
    State2 = proc_msgs(State1, 1),
    #{decision := ok} = State2,
    State3 = proc_msgs(State2, 1),
    #{decision := ok} = State3,
    State4 = proc_msgs(State3, 1),
    #{decision := excess} = State4,
    State5 = proc_msgs(State4, 1),
    #{decision := excess} = State5,
    timer:sleep(2100),
    State7 = proc_msgs(State5, 8),
    #{decision := ok} = State7,
    ok.

proc_msgs(State, 0) ->
    State;
proc_msgs(State, Y) ->
    M = #xmlel{name = <<"message">>,
               attrs = [{<<"type">>, <<"chat">>}, {<<"to">>, <<"bob37.76184@localhost">>}],
               children = [#xmlel{name = <<"body">>,
                                  attrs = [],
                                  children = [{xmlcdata, <<Y/integer>>}]}]},
    NState = case mod_spamctl:control(State, <<"message">>, [M]) of
                 {stop, S} -> S;
                 S -> S
             end,
    proc_msgs(NState, Y - 1).


now_to_usec() ->
    {MSec, Sec, USec} = os:timestamp(),
    (MSec * 1000000 + Sec) * 1000000 + USec.
