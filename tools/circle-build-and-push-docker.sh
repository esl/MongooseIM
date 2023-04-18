#!/bin/bash

set -e
SRC_ROOT="$(dirname "${BASH_SOURCE}")"
source $SRC_ROOT/circleci-prepare-mongooseim-docker.sh

cp ../mongooseim-*.tar.gz member

docker run --privileged --rm tonistiigi/binfmt --install all
docker buildx create --name builder --driver docker-container --bootstrap --use
docker login -u ${DOCKERHUB_USER} -p ${DOCKERHUB_PASS}

docker buildx build --platform linux/amd64,linux/arm64 \
             -f Dockerfile.member -t ${IMAGE_TAG} --push --provenance=false --progress=plain \
             --build-arg BUILD_DATE=`date -u +"%Y-%m-%dT%H:%M:%SZ"` \
	     --build-arg VCS_REF=${GIT_REF} \
	     --build-arg VCS_REF_DESC="${GIT_COMMIT_MSG}" \
	     --build-arg VERSION=${VERSION} \
	     .
