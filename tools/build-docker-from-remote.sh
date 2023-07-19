#!/bin/bash

set -e

# Setting COMMIT_HASH to the value of the first parameter or to local HEAD if it was not provided
COMMIT_HASH=${1:-$(git rev-parse HEAD)}

SRC_ROOT="$(dirname "${BASH_SOURCE}")"
source $SRC_ROOT/circleci-prepare-mongooseim-docker.sh

# Build a builder image (contains erlang and the build tools)
docker build -f Dockerfile.builder -t mongooseim-builder .

# Create a volume for the result tarballs
docker volume create mongooseim-builds || echo "Probably already created volume"

# Build MongooseIM release
docker run --rm -v mongooseim-builds:/builds -e TARBALL_NAME=mongooseim mongooseim-builder /build.sh MongooseIM https://github.com/esl/MongooseIM ${COMMIT_HASH} 

# Copy the latest build artifact
CID=$(docker run --rm -d -v mongooseim-builds:/builds busybox sleep 1000)
docker cp $CID:/builds/. ./member/
docker rm -f $CID

# Build a final image
docker build -f Dockerfile.member -t mongooseim .

cd ..
rm -rf mongooseim-docker
