#!/bin/sh

PIDFILE=/tmp/print-dots.pid
SLEEPTIME=10s

loop () {
    while true; do
        printf "."
        sleep $SLEEPTIME
    done
}

countdown_loop() {
    local n="${1:-5}"
    local text="${2:-wait}"
    local pid_to_monitor="${3:-$$}"
    local len="${#text}"
    while [ "$n" -gt 0 ] 2>/dev/null; do
        n=$((n-1))
        sleep 1
        ps -p "$pid_to_monitor" > /dev/null || break
        printf "\r%${len}c%15c\r%s %d sec. " " " " " "$text" "$n"
    done
}

case $1 in
start)
    $0 loop > /dev/fd/1 &
    echo $! > $PIDFILE
    ;;
start_countdown)
    shift
    $0 countdown_loop "$@" > /dev/fd/1 &
    echo $! > $PIDFILE
    ;;
stop)
    [ -s $PIDFILE ] && (kill $(cat $PIDFILE) 2>/dev/null; rm $PIDFILE)
    ;;
loop)
    loop
    ;;
countdown_loop)
    shift
    countdown_loop "$@"
    ;;
monitor)
    PID_TO_MONITOR=$2
    ./tools/kill_processes_on_exit.sh "$PID_TO_MONITOR" "$(cat $PIDFILE)" &
esac
