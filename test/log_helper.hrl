%% Assertions for expected logs. Naming follows eunit.hrl
%% For set-up and tear-down, see log_helper.erl

-define(assertNoLog(LevelPattern, MsgPattern),
        ?assertNoLog(LevelPattern, MsgPattern, 0)).

-define(assertNoLog(LevelPattern, MsgPattern, Timeout),
        case ?receiveLog(LevelPattern, MsgPattern, Timeout) of
            no_log -> ok;
            _ -> ct:fail("Received unexpected log")
        end).

-define(assertLog(LevelPattern, MsgPattern),
        ?assertLog(LevelPattern, MsgPattern, 0)).

-define(assertLog(LevelPattern, MsgPattern, Timeout),
        case ?receiveLog(LevelPattern, MsgPattern, Timeout) of
            no_log -> ct:fail("Expected log not received");
            _ -> ok
        end).

-define(receiveLog(LevelPattern, MsgPattern),
        ?receiveLog(LevelPattern, MsgPattern, 0)).

-define(receiveLog(LevelPattern, MsgPattern, Timeout),
        ?wrap(receive
                  {log, #{level := Level = LevelPattern,
                          msg := {report, Msg = MsgPattern}}} ->
                      ct:log("Received ~p log: ~p", [Level, Msg]),
                      {Level, Msg}
              after
                  Timeout -> no_log
              end)).

%% Wrap in a fun to avoid unsafe variables
-define(wrap(Expr), (fun() -> Expr end)()).
