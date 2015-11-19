#!/bin/bash


RIAK_INSTANCE=mongooseim-riak
RIAK_HOST="http://${1}:8098"
SSH_DOCKERMACHINE_ALIAS="docker_riak"

curl --insecure -X PUT $RIAK_HOST/search/schema/vcard \
    -H 'Content-Type:application/xml' \
    --data-binary @vcard_search_schema.xml

curl --insecure $RIAK_HOST/search/schema/vcard

curl -XPUT $RIAK_HOST/search/index/vcard \
    -H 'Content-Type: application/json' \
    -d '{"schema":"vcard"}'

curl -XPUT $RIAK_HOST/search/schema/mam \
    -H 'Content-Type:application/xml' \
    --data-binary @mam_search_schema.xml

curl $RIAK_HOST/search/schema/mam

curl -XPUT $RIAK_HOST/search/index/mam \
    -H 'Content-Type: application/json' \
    -d '{"schema":"mam"}'

sleep 20

ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec -ti '${RIAK_INSTANCE}' riak-admin bucket-type create users '{"props":{"datatype":"map"}}''

ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec -ti '${RIAK_INSTANCE}' riak-admin bucket-type activate users'

ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec -ti '${RIAK_INSTANCE}' riak-admin bucket-type create private '{"props":{"last_write_wins":true}}''

ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec -ti '${RIAK_INSTANCE}' riak-admin bucket-type activate private'

ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec -ti '${RIAK_INSTANCE}' riak-admin bucket-type create vcard '{"props":{"last_write_wins":true, "search_index":"vcard"}}''

ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec -ti '${RIAK_INSTANCE}' riak-admin bucket-type activate vcard'

ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec -ti '${RIAK_INSTANCE}' riak-admin bucket-type create mam_yz '{"props":{"datatype":"map", "search_index":"mam"}}''

ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec -ti '${RIAK_INSTANCE}' riak-admin bucket-type activate mam_yz'
