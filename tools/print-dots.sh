#!/bin/sh

PIDFILE=/tmp/print-dots.pid
SLEEPTIME=10s

loop () {
    while true; do
        printf "."
        sleep $SLEEPTIME
    done
}

case $1 in
start)
    $0 loop > /dev/fd/1 &
    echo $! > $PIDFILE
    ;;
stop)
    [ -s $PIDFILE ] && (kill $(cat $PIDFILE); rm $PIDFILE)
    ;;
loop)
    loop
    ;;
monitor)
    PID_TO_MONITOR=$2
    ./tools/kill_processes_on_exit.sh "$PID_TO_MONITOR" "$(cat $PIDFILE)" &
esac
