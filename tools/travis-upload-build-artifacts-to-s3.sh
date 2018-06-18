#!/bin/bash

source tools/travis-helpers.sh

set -euo pipefail

AWS_BUCKET="${AWS_BUCKET:-mongooseim-ct-results}"

ARTIFACTS_DIR=/tmp/build_artifacts
mkdir $ARTIFACTS_DIR

cp -r -a $TRAVIS_BUILD_DIR/_build ${ARTIFACTS_DIR}
cp -r $TRAVIS_BUILD_DIR/big_tests/_build ${ARTIFACTS_DIR}/big_tests_build
cp -r $TRAVIS_BUILD_DIR/tools/ssl ${ARTIFACTS_DIR}

ARTIFACT_NAME=${TRAVIS_BUILD_ID}-${TRAVIS_OTP_RELEASE}.bz2

echo "Creating artifact archive: ${ARTIFACT_NAME}"
time tar -cj -C /tmp -f /tmp/${ARTIFACT_NAME} build_artifacts

echo "Uploading the archive to S3"
time aws s3 cp /tmp/${ARTIFACT_NAME} s3://${AWS_BUCKET}/build_artifacts/${ARTIFACT_NAME}

