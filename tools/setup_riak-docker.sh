#!/bin/bash


RIAK_INSTANCE=mongooseim-riak
RIAK_HOST="http://${1}:8098"
SSH_DOCKERMACHINE_ALIAS="docker_riak"

echo "uploading search schemas to:" ${RIAK_HOST}

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

echo "creating users..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${RIAK_INSTANCE}' riak-admin bucket-type create users ''\{\"props\":\{\"datatype\":\"map\"\}\}'''
#ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${RIAK_INSTANCE}' riak-admin bucket-type create users '{"props":{"datatype":"map"}}''
echo "activating users..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${RIAK_INSTANCE}' riak-admin bucket-type activate users'
echo "creating private bucket..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${RIAK_INSTANCE}' riak-admin bucket-type create private ''\{\"props\":\{\"last_write_wins\":true\}\}'''
echo "activating private bucket..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${RIAK_INSTANCE}' riak-admin bucket-type activate private'
echo "creating vcard bucket..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${RIAK_INSTANCE}' riak-admin bucket-type create vcard ''\{\"props\":\{\"last_write_wins\":true, \"search_index\":\"vcard\"\}\}'''
echo "activating vcard bucket..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${RIAK_INSTANCE}' riak-admin bucket-type activate vcard'
echo "creating mam_yz bucket..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${RIAK_INSTANCE}' riak-admin bucket-type create mam_yz ''\{\"props\":\{\"datatype\":\"map\", \"search_index\":\"mam\"\}\}'''
echo "activating mam_yz bucket..."
ssh ${SSH_DOCKERMACHINE_ALIAS} 'docker exec '${RIAK_INSTANCE}' riak-admin bucket-type activate mam_yz'

