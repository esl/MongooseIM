#!/usr/bin/env bash

# cd to repo
cd "$(dirname "$0")/../"
source tools/common-vars.sh

source tools/db-versions.sh

rustfs_docker_name="mongooseim-rustfs"
rustfs_access_key="AKIAIAOAONIULXQGMOUA"
rustfs_secret_key="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"

$DOCKER rm -v -f "${rustfs_docker_name}" || echo "Skip removing previous container"

IMAGE="rustfs/rustfs:$RUSTFS_VERSION"
CURL_IMAGE="curlimages/curl:$CURL_IMAGE_VERSION"

$DOCKER run -d -p 9000:9000 \
    --name "${rustfs_docker_name}" \
    -e "RUSTFS_ACCESS_KEY=${rustfs_access_key}" \
    -e "RUSTFS_SECRET_KEY=${rustfs_secret_key}" \
    $IMAGE

tools/wait_for_service.sh "${rustfs_docker_name}" 9000

RUSTFS_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $rustfs_docker_name)

# Host curl may be too old to sign S3 requests (e.g. Ubuntu 22.04)
$DOCKER run --rm -i --entrypoint sh $CURL_IMAGE -s -- "http://${RUSTFS_IP}:9000" \
    < tools/prepare-rustfs-bucket.sh
