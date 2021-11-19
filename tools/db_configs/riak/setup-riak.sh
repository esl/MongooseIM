#!/usr/bin/env bash

# Enable search
# Solr is sloow on CI
sed -e "s/^search = \(.*\)/search = on/" \
    -e "s/^search.solr.start_timeout = \(.*\)/search.solr.start_timeout = 2m/" \
    /etc/riak/riak.conf

# Enable ssl by appending settings from riak.conf.ssl
cat "/riak.conf.ssl" >> /etc/riak/riak.conf

function wait_for_port {
    PORT=$1
    while :
    do
        (echo > /dev/tcp/localhost/$PORT) >/dev/null 2>&1
        result=$?
        if [[ $result -eq 0 ]]; then
            break
        fi
        sleep 1
    done
}

function maybe_listen_for_schema_port {
    if [ -z "$SCHEMA_READY_PORT" ]; then
        echo "SCHEMA_READY_PORT not provided"
    else
        # Listen on a port to signal for healthcheck that we are ready
        echo "Listening for $SCHEMA_READY_PORT for healthcheck"
        perl -MIO::Socket::INET -ne 'BEGIN{$l=IO::Socket::INET->new(LocalPort => '${SCHEMA_READY_PORT}', Proto=>"tcp", Listen=>5, ReuseAddr=>1); $l=$l->accept}'
    fi
}

function wait_for_riak_and_setup {
    wait_for_port 8098
    set -e
    riak escript /setup_riak.escript
    maybe_listen_for_schema_port
}

wait_for_riak_and_setup &
