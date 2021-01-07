#!/bin/bash

source tools/circleci-helpers.sh

set -euo pipefail

CT_REPORTS=$(ct_reports_dir)

if [ ! -d "${CT_REPORTS}" ]; then
    echo "Skip uploading, because $CT_REPORTS directory does not exist"
    exit 0
fi

echo "Uploading test results to s3"
echo $(s3_url ${CT_REPORTS})



FILE_COUNT=$(find "${CT_REPORTS}" -type f | wc -l)
echo "Uploading $FILE_COUNT files"
ls $CT_REPORTS

curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64-2.0.30.zip" -o "awscliv2.zip"
unzip -q awscliv2.zip
sudo ./aws/install

aws configure set aws_access_key_id $AWS_ACCESS_KEY_ID
aws configure set aws_secret_access_key $AWS_SECRET_ACCESS_KEY
aws configure set default.region $AWS_DEFAULT_REGION
aws configure set default.s3.max_concurrent_requests 64

time aws s3 cp ${CT_REPORTS} s3://circleci-mim-results/${CT_REPORTS} --acl public-read --recursive --quiet
