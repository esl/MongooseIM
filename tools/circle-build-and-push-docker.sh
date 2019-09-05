#!/bin/bash

#MIM_TAR_FULL_NAME=mongooseim-$CIRCLE_BRANCH.OTP-$OTP_RELEASE.$(lsb_release -is | tr "A-Z" "a-z").$(lsb_release -rs).$(uname -m).tar.bz2
MONGOOSE_TGZ=mongooseim.tar.gz

BUILD_PATH=_build/prod/rel/mongooseim

#tar -cjh --transform="s,${BUILD_PATH},mongooseim-${CIRCLE_BRANCH},S" -f ${MIM_TAR_FULL_NAME} ${BUILD_PATH}
tar czh --transform="s,${BUILD_PATH},mongooseim,S" -f $MONGOOSE_TGZ ${BUILD_PATH}

export BUILDS=`pwd`

DOCKERHUB_TAG=${CIRCLE_BRANCH}
VERSION=`tools/generate_vsn.sh`
GIT_REF=`git rev-parse --short HEAD`

if [ ${CIRCLE_BRANCH} == 'master' ]; then
    DOCKERHUB_TAG="latest";
fi

echo "Tag: ${DOCKERHUB_TAG}"

IMAGE_TAG=${DOCKERHUB_REPO}/mongooseim:${DOCKERHUB_TAG}

git clone https://github.com/esl/mongooseim-docker.git
cd mongooseim-docker
git checkout 8105a0af3f673a92836af49ed797a16d66b51b2f

cp ../${MONGOOSE_TGZ} member

docker build -f Dockerfile.member -t ${IMAGE_TAG} \
             --build-arg BUILD_DATE=`date -u +"%Y-%m-%dT%H:%M:%SZ"` \
	     --build-arg VCS_REF=${GIT_REF} \
	     --build-arg VERSION=${VERSION} \
	     .

#docker login -u ${DOCKERHUB_USER} -p ${DOCKERHUB_PASS}

#docker push ${IMAGE_TAG}
