#!/usr/bin/env bash

set -e

cd tools/pkg/packages
PACKAGE_NAME=$(ls)
prefix=$(git rev-parse --short HEAD)

if which aws ; then
    echo "aws tool ready"
else
    curl "https://awscli.amazonaws.com/awscli-exe-linux-$(uname -m).zip" -o "awscliv2.zip"
    unzip -q awscliv2.zip
    sudo ./aws/install
fi

aws configure set aws_access_key_id $AWS_ACCESS_KEY_ID
aws configure set aws_secret_access_key $AWS_SECRET_ACCESS_KEY
aws configure set default.region $AWS_DEFAULT_REGION

if [ -n "$CIRCLE_TAG" ]; then
    aws s3 cp "${PACKAGE_NAME}" "s3://mim-packages/tags/${CIRCLE_TAG}/${PACKAGE_NAME}" --acl public-read --quiet
else
    aws s3 cp "${PACKAGE_NAME}" "s3://mim-packages/branches/${CIRCLE_BRANCH}/${prefix}/${PACKAGE_NAME}" --acl public-read --quiet
fi
