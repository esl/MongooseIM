#!/bin/bash

dir="$1"

[ -d "$dir" ] || { echo "ERROR: directory '${dir}' does not exist"; exit 1; }

dest_dir="${2:-$(basename "$dir")}"

[ -z "$AWS_ACCESS_KEY_ID" ] && { echo "ERROR: AWS_ACCESS_KEY_ID env is not set"; exit 1; }
[ -z "$AWS_SECRET_ACCESS_KEY" ] && { echo "ERROR: AWS_SECRET_ACCESS_KEY env is not set"; exit 1; }
[ -z "$AWS_DEFAULT_REGION" ] && { echo "ERROR: AWS_DEFAULT_REGION env is not set"; exit 1; }

[ -z "$GITHUB_RUN_ID" ] && { echo "ERROR: GITHUB_RUN_ID env is not set"; exit 1; }
[ -z "$GITHUB_RUN_ATTEMPT" ] && { echo "ERROR: GITHUB_RUN_ATTEMPT env is not set"; exit 1; }

which aws || { echo "aws tool is missing"; exit 1; }

aws configure set aws_access_key_id "$AWS_ACCESS_KEY_ID"
aws configure set aws_secret_access_key "$AWS_SECRET_ACCESS_KEY"
aws configure set default.region "$AWS_DEFAULT_REGION"
aws configure set default.s3.max_concurrent_requests 64

file_count=$(find "$dir" -type f | wc -l)
echo "Uploading ${file_count} files"

## $PRESET is not unique, so we have to add $RANDOM to avoid collisions
prefix="GH/${GITHUB_RUN_ID}/${GITHUB_RUN_ATTEMPT}/${PRESET}.${RANDOM}/${dest_dir}"

echo "directory '${dir}' is uploaded here:"
echo "   https://esl.github.io/circleci-mim-results/s3_reports.html?prefix=${prefix}"

time aws s3 cp "$dir" s3://circleci-mim-results/"${prefix}" --acl public-read --recursive --quiet
