%% Reports slow tasks
-module(mongoose_progress).
-export([run/2]).

-include("mongoose.hrl").

run(F, Info) ->
    ProgressPid = spawn_link(fun() -> report_progress(Info, 0) end),
    try
        F()
    after
        ProgressPid ! stop
    end.

report_progress(Info, Total) ->
    receive
        stop -> ok
    after 5000 ->
              Info2 = apply_info(Info),
              ?LOG_WARNING(Info2#{what => report_progress, total_time => Total}),
              report_progress(Info, Total + 5000)
    end.

apply_info(Info) when is_map(Info) -> Info;
apply_info(Info) when is_function(Info) -> Info().
