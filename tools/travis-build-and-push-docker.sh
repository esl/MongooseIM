#!/bin/bash

git clone -b demo https://github.com/studzien/mongooseim-docker.git
cd mongooseim-docker

export PROJECT=master
export VOLUMES="`pwd`/projects/${PROJECT}"

mkdir -p "${VOLUMES}/builds"

echo "${PROJECT} master https://github.com/esl/mongooseim" > ${VOLUMES}/builds/specs

make builder

docker exec -it ${PROJECT}-builder /build.sh

make member.build

docker images



