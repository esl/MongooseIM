#!/usr/bin/env bash

ELASTICSEARCH_PORT=${ELASTICSEARCH_PORT:-9200}
ELASTICSEARCH_URL=http://localhost:$ELASTICSEARCH_PORT
ELASTICSEARCH_PM_MAPPING="$(pwd)/priv/elasticsearch/pm.json"
ELASTICSEARCH_MUC_MAPPING="$(pwd)/priv/elasticsearch/muc.json"

echo "Putting ElasticSearch mappings"
(curl -X PUT $ELASTICSEARCH_URL/messages -d "@$ELASTICSEARCH_PM_MAPPING" -w "status: %{http_code}" | grep "status: 200" > /dev/null) || \
    echo "Failed to put PM mapping into ElasticSearch"
(curl -X PUT $ELASTICSEARCH_URL/muc_messages -d "@$ELASTICSEARCH_MUC_MAPPING" -w "status: %{http_code}" | grep "status: 200" > /dev/null) || \
    echo "Failed to put MUC mapping into ElasticSearch"
