%% Helper to log long running operations.
-module(mongoose_long).
-export([run_spawn/2, run_tracked/2]).

-include_lib("kernel/include/logger.hrl").

%% Extra logging information
-type log_info() :: map().
-type task_result() :: term().
-type task_fun() :: fun(() -> task_result()).
-export_type([log_info/0]).

%% Spawn a new process to do some memory-intensive task
%% This allows to reduce GC on the parent process
%% Wait for function to finish
%% Handles errors
%% Returns result from the function or crashes (i.e. forwards the error)
-spec run_spawn(log_info(), task_fun()) -> task_result().
run_spawn(Info, F) ->
    Pid = self(),
    Ref = make_ref(),
    proc_lib:spawn_link(fun() ->
        try run_tracked(Info, F) of
            Res ->
                Pid ! {result, Ref, Res}
        catch
            Class:Reason:Stacktrace ->
                Pid ! {forward_error, Ref, {Class, Reason, Stacktrace}}
        end
    end),
    receive
        {result, Ref, Res} ->
            Res;
        {forward_error, Ref, {Class, Reason, Stacktrace}} ->
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% Run function Fun.
%% Logs errors.
%% Logs if function execution takes too long.
%% Does not catches the errors - the caller would have to catch
%% if they want to prevent an error.
-spec run_tracked(log_info(), task_fun()) -> task_result().
run_tracked(Info, Fun) ->
    Parent = self(),
    Start = erlang:system_time(millisecond),
    ?LOG_INFO(Info#{what => long_task_started}),
    Pid = spawn_mon(Info, Parent, Start),
    try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            Log = Info#{
                what => long_task_failed,
                class => Class,
                reason => Reason,
                stacktrace => Stacktrace
            },
            ?LOG_ERROR(Log),
            erlang:raise(Class, Reason, Stacktrace)
    after
        Diff = diff(Start),
        ?LOG_INFO(Info#{what => long_task_finished, time_ms => Diff}),
        Pid ! stop
    end.

spawn_mon(Info, Parent, Start) ->
    spawn_link(fun() -> run_monitor(Info, Parent, Start) end).

run_monitor(Info, Parent, Start) ->
    Mon = erlang:monitor(process, Parent),
    monitor_loop(Mon, Info, Start, Parent).

monitor_loop(Mon, Info, Start, Parent) ->
    receive
        {'DOWN', MonRef, process, _Pid, Reason} when Mon =:= MonRef ->
            ?LOG_ERROR(Info#{what => long_task_failed, reason => Reason}),
            ok;
        stop ->
            ok
    after 1000 ->
        Diff = diff(Start),
        ?LOG_INFO(Info#{what => long_task_progress, time_ms => Diff, stacktrace => erlang:process_info(Parent, current_stacktrace)}),
        monitor_loop(Mon, Info, Start, Parent)
    end.

diff(Start) ->
    erlang:system_time(millisecond) - Start.
