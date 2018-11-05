#!/usr/bin/env bash
#
# Usage: kill_processes_on_exit PID_TO_MONITOR PIDS_TO_KILL
#
# Waits for PID_TO_MONITOR to terminate, then terminates PIDS_TO_KILL/
set -e

if [ "$#" = 0 ]; then
    echo "Usage: kill_processes_on_exit PID_TO_MONITOR PIDS_TO_KILL"
    exit 1
fi

PID_TO_MONITOR=$1
# Consume PID_TO_MONITOR
shift

function verbose_print
{
    if [ "$VERBOSE" = "1" ]; then
        echo "$1"
    fi
}

# If there are PIDS_TO_KILL
if [ "$#" -ne 0 ]; then
    verbose_print "Monitor $PID_TO_MONITOR"
    # Forward the error in /dev/null:
    # kill: kill PID failed: no such process
    # Monitor PID_TO_MONITOR
    (while kill -0 $PID_TO_MONITOR 2&> /dev/null; do sleep 1; done)
    # Ehen PID_TO_MONITOR is dead...
    verbose_print "kill_processes_on_exit: $@"
    # Kill the rest of arguments
    kill $@ 2&> /dev/null
fi
