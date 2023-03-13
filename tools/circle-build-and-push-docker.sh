#!/bin/bash

set -e
# From https://github.com/esl/mongooseim-docker/pull/49
MIM_DOCKER_VERSION=6b62e9cf26c523e35fe78c57cbae2e8832f555d2

# We use output of generate_vsn, because it does not contain illegal characters, returns
# git tag when building from tag itself, and is unique in any other case
VERSION=`tools/generate_vsn.sh`
DOCKERHUB_TAG=${VERSION}
GIT_REF=`git rev-parse HEAD`
GIT_COMMIT_MSG=`git log --format=%B -n 1 HEAD`

if [ -n "$CIRCLE_PULL_REQUEST" ]; then
    # CircleCI doesn't provide PR number in env. var., so we need to extract it from PR URL
    # May not work with different service than GitHub
    # TODO: Possibly change it to something else during Tide integration
    PR_NUMBER=${CIRCLE_PULL_REQUEST##*/}
    DOCKERHUB_TAG="PR-${PR_NUMBER}"
elif [ ${CIRCLE_BRANCH} == 'master' ]; then
    DOCKERHUB_TAG="latest";
fi

echo "Tag: ${DOCKERHUB_TAG}"

IMAGE_TAG=${DOCKERHUB_REPO}/mongooseim:${DOCKERHUB_TAG}

git clone https://github.com/esl/mongooseim-docker.git
cd mongooseim-docker
git checkout $MIM_DOCKER_VERSION

cp ../mongooseim-*.tar.gz member

docker run --privileged --rm tonistiigi/binfmt --install all
docker buildx create --name builder --driver docker-container --bootstrap --use
docker login -u ${DOCKERHUB_USER} -p ${DOCKERHUB_PASS}

docker buildx build --platform linux/amd64,linux/arm64 \
             -f Dockerfile.member -t ${IMAGE_TAG} --push --progress=plain \
             --build-arg BUILD_DATE=`date -u +"%Y-%m-%dT%H:%M:%SZ"` \
	     --build-arg VCS_REF=${GIT_REF} \
	     --build-arg VCS_REF_DESC="${GIT_COMMIT_MSG}" \
	     --build-arg VERSION=${VERSION} \
	     .

echo $IMAGE_TAG > ../image_tag
