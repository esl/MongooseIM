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

generate_link() {
    local type=$1
    local identifier=$2
    local package_name=$3
    local link_package_name=$(echo "$package_name" | sed 's/^mongooseim/mim/')
    echo "Package link: https://esl.github.io/circleci-mim-results/s3_packages.html?prefix=${type}/${identifier}/${link_package_name}"
}

if [ -n "$CIRCLE_TAG" ]; then
    aws s3 cp "${PACKAGE_NAME}" "s3://mim-packages/tags/${CIRCLE_TAG}/${PACKAGE_NAME}" --acl public-read --quiet

    echo "$GH_RELEASE_TOKEN" | gh auth login --with-token
    gh release upload "${CIRCLE_TAG}" "${PACKAGE_NAME}" --repo "${CIRCLE_PROJECT_USERNAME}/${CIRCLE_PROJECT_REPONAME}"

    generate_link "tags" "$CIRCLE_TAG" "$PACKAGE_NAME"
else
    aws s3 cp "${PACKAGE_NAME}" "s3://mim-packages/branches/${CIRCLE_BRANCH}/${prefix}/${PACKAGE_NAME}" --acl public-read --quiet

    generate_link "branches" "$CIRCLE_BRANCH/$prefix" "$PACKAGE_NAME"
fi
