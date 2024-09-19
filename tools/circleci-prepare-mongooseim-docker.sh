#!/bin/bash

set -e
# master from 2024-09-12
MIM_DOCKER_VERSION=cde33527fab41b512ed0d83b50a1d088e99c4ea2

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
elif [ "${CIRCLE_BRANCH}" == 'master' ]; then
    DOCKERHUB_TAG="latest";
fi

echo "Tag: ${DOCKERHUB_TAG}"

export IMAGE_TAG=${DOCKERHUB_ORG}/${DOCKERHUB_REPO}:${DOCKERHUB_TAG}

if [ -d mongooseim-docker ]; then
    # We already have mongooseim-docker, just fetch the required version
    cd mongooseim-docker
    git fetch
    git checkout "$MIM_DOCKER_VERSION"
else
    git clone https://github.com/esl/mongooseim-docker.git
    cd mongooseim-docker
    git checkout "$MIM_DOCKER_VERSION"
fi
