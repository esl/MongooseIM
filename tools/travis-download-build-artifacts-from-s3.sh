#!/bin/bash

source tools/travis-helpers.sh

set -euo pipefail

AWS_BUCKET="${AWS_BUCKET:-mongooseim-ct-results}"

ARTIFACT_NAME=${TRAVIS_BUILD_ID}-${TRAVIS_OTP_RELEASE}.bz2
echo "Artifact name: ${ARTIFACT_NAME}"

## If there is no build artifacts, we simply continue without them
## the test is still possible, only will take longer to complet

set +e
echo "sync build artifacts from S3"
time aws s3 cp s3://${AWS_BUCKET}/build_artifacts/${ARTIFACT_NAME} /tmp/${ARTIFACT_NAME}
if [ "$?" -ne 0 ]; then echo "missing build artifacts, continuing without them"; exit 0; fi
set -e

echo "extract"
time tar -xjf /tmp/${ARTIFACT_NAME} -C /tmp

mkdir -p ${TRAVIS_BUILD_DIR}/_build
mkdir -p ${TRAVIS_BUILD_DIR}/tools/ssl

ARTIFACTS_DIR=/tmp/build_artifacts

cp -r -a ${ARTIFACTS_DIR}/_build ${TRAVIS_BUILD_DIR}
cp -r ${ARTIFACTS_DIR}/ssl ${TRAVIS_BUILD_DIR}/tools
cp -r ${ARTIFACTS_DIR}/big_tests_build ${TRAVIS_BUILD_DIR}/big_tests/_build

echo "Listing ${TRAVIS_BUILD_DIR}"
ls -lth ${TRAVIS_BUILD_DIR}
echo "Listing ${TRAVIS_BUILD_DIR}/_build"
ls -lth ${TRAVIS_BUILD_DIR}/_build
echo "Listing ${TRAVIS_BUILD_DIR}/tools/ssl"
ls -lth ${TRAVIS_BUILD_DIR}/tools/ssl

