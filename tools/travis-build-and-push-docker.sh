#!/bin/bash

make rel

export MIM_TAR=mongooseim-$TRAVIS_BRANCH.$TRAVIS_OTP_RELEASE.$(lsb_release -is | tr "A-Z" "a-z").$(lsb_release -rs).$(uname -m).tar.bz2

tar -cjh --transform='s,mongooseim,mongooseim-${$TRAVIS_BRANCH},S' -f ${MIM_TAR} rel/mongooseim

git clone -b demo https://github.com/studzien/mongooseim-docker.git
cd mongooseim-docker

echo "branch: ${TRAVIS_BRANCH}"
echo "tag: ${TRAVIS_TAG}"


export PROJECT=${TRAVIS_BRANCH}
export VOLUMES="`pwd`/projects/${PROJECT}"

DOCKERHUB_TAG=${TRAVIS_BRANCH}

if [ ${TRAVIS_BRANCH} = 'master' ]; then
	$DOCKERHUB_TAG="latest";
fi

mkdir -p "${VOLUMES}/builds"

echo "${PROJECT} ${TRAVIS_BRANCH} https://github.com/${TRAVIS_REPO_SLUG}" > ${VOLUMES}/builds/specs

make builder

docker exec -it ${PROJECT}-builder /build.sh

make member.build

docker images

docker login -e=${DOCKERHUB_EMAIL} -u ${DOCKERHUB_USER} -p ${DOCKERHUB_PASS}

docker tag ${PROJECT}-mongooseim ${DOCKERHUB_USER}/mongooseim:${DOCKERHUB_TAG}

docker push ${DOCKERHUB_USER}/mongooseim:${DOCKERHUB_TAG}

