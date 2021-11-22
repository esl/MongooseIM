#!/usr/bin/env bash

function riak_solr_is_up
{
    curl 'http://localhost:8093/internal_solr/mam/admin/ping?wt=json' | grep '"status":"OK"'
}

set -e

# Wait for solr
for i in {1..60}; do
    if riak_solr_is_up; then
        exit 0
    fi
    echo -n "."
    sleep 1
done


echo "SOLR is not up"
exit 1
