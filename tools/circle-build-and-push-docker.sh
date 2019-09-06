#!/bin/bash

MONGOOSE_TGZ=mongooseim.tar.gz

BUILD_PATH=_build/prod/rel/mongooseim

tar czh --transform="s,${BUILD_PATH},mongooseim,S" -f $MONGOOSE_TGZ ${BUILD_PATH}

export BUILDS=`pwd`

# We use commit hash by default, because branch name may contain characters
# that are illegal for Docker tags, e.g. '/'.
DOCKERHUB_TAG=${CIRCLE_SHA1}
VERSION=`tools/generate_vsn.sh`
GIT_REF=`git rev-parse --short HEAD`

if [ -n "$CIRCLE_PULL_REQUEST" ]; then
    # CircleCI doesn't provide PR number in env. var., so we need to extract it from PR URL
    # May not work with different service than GitHub
    # TODO: Possibly change it to something else during Tide integration
    PR_NUMBER=${CIRCLE_PULL_REQUEST##*/}
    DOCKERHUB_TAG="PR-${PR_NUMBER}"
elif [ ${CIRCLE_BRANCH} == 'master' ]; then
    DOCKERHUB_TAG="latest";
fi

# TODO: Add DOCKERHUB=${VERSION} when CircleCI handles weekly builds as well

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

docker login -u ${DOCKERHUB_USER} -p ${DOCKERHUB_PASS}

docker push ${IMAGE_TAG}
