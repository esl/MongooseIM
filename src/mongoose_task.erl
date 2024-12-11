%% @doc Helper module for slow tasks.
-module(mongoose_task).
-export([run_tracked/2]).

%% @doc Run a task, log if it takes too long
-spec run_tracked(Info :: map(), F :: fun(() -> Res)) -> Res
    when Res :: term().
run_tracked(Info, F) ->
    %% Reuse code from CETS.
    %% `cets_long' module does not use any CETS functionality and
    %% can be used, even if CETS tables are not used.
    cets_long:run_tracked(Info, F).
