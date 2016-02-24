#!/bin/bash

git clone -b demo https://github.com/studzien/mongooseim-docker.git
cd mongooseim-docker

echo "branch: ${TRAVIS_BRANCH}"
echo "tag: ${TRAVIS_TAG}"


export PROJECT=${TRAVIS_BRANCH}
export VOLUMES="`pwd`/projects/${PROJECT}"

mkdir -p "${VOLUMES}/builds"

echo "${PROJECT} ${TRAVIS_BRANCH} https://github.com/${TRAVIS_REPO_SLUG}" > ${VOLUMES}/builds/specs

make builder

docker exec -it ${PROJECT}-builder /build.sh

make member.build

docker images

docker login -e=${DOCKERHUB_EMAIL} -u ${DOCKERHUB_USER} -p ${DOCKERHUB_PASS}

docker tag ${PROJECT}-mongooseim ${DOCKERHUB_USER}/mongooseim

docker push ${DOCKERHUB_USER}/mongooseim

