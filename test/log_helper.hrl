%% Assertions for expected logs. Naming follows eunit.hrl
%% For set-up and tear-down, see log_helper.erl

-define(assertNoLog(LevelPattern, MsgPattern),
        case ?receiveLog(LevelPattern, MsgPattern) of
            no_log -> ok;
            _ -> ct:fail("Received unexpected log")
        end).

-define(assertLog(LevelPattern, MsgPattern),
        case ?receiveLog(LevelPattern, MsgPattern) of
            no_log -> ct:fail("Expected log not received");
            _ -> ok
        end).

-define(receiveLog(LevelPattern, MsgPattern),
        ?wrap(receive
                  {log, #{level := Level = LevelPattern,
                          msg := {report, Msg = MsgPattern}}} ->
                      ct:log("Received ~p log: ~p", [Level, Msg]),
                      {Level, Msg}
              after
                  0 -> no_log
              end)).

%% Wrap in a fun to avoid unsafe variables
-define(wrap(Expr), (fun() -> Expr end)()).
