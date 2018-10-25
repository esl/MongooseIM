#!/usr/bin/env bash
# This script runs HTTP server for small test reports
# Usage:
# - ./tools/test_runner/show_reports.sh small
# - ./tools/test_runner/show_reports.sh big
set -e

# Go to the repo directory
cd "$( dirname "${BASH_SOURCE[0]}" )"/../..

# Usage: run_http_server_for PORT DIRECTORY
function run_http_server_for
{
    local PORT="$1"
    local DIRECTORY="$2"
    mkdir -p "$DIRECTORY"
    local ABS_DIRECTORY=$(./tools/abs_dirpath.sh "$DIRECTORY")
    cd "$DIRECTORY"

    echo "Running an HTTP server for $ABS_DIRECTORY"
    python -m SimpleHTTPServer "$PORT" >> http_server.log 2>> http_server.log &
    echo "Open http://localhost:$PORT in your browser"
}

case "$1" in
    small)
        run_http_server_for 18001 "_build/test/logs/"
        ;;
    big)
        run_http_server_for 18002 "big_tests/ct_report/"
        ;;
    *)
        echo "Pass one argument small or big"
esac
