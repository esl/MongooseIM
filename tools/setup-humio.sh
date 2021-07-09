#!/usr/bin/env bash

mkdir -p _build/mim1/rel/mongooseim/log

docker run -d \
    --name mongooseim-filebeat \
    -v "$(pwd)/_build/mim1/rel/mongooseim/log:/usr/lib/mongooseim/log" \
    -v="$(pwd)/priv/filebeat.mongooseim.humio.yml:/usr/share/filebeat/filebeat.yml:ro" \
    docker.elastic.co/beats/filebeat-oss:7.9.2 \
    filebeat -e -strict.perms=false -E output.elasticsearch.password="$HUMIO_PASSWORD"
