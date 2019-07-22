#!/bin/bash

source tools/circleci-helpers.sh

set -euo pipefail

CT_REPORTS=$(ct_reports_dir)

if [ ! -d "${CT_REPORTS}" ]; then
    echo "Skip uploading, because $CT_REPORTS directory does not exist"
    exit 0
fi

echo "Uploading test results to s3"
echo $(circleci_s3_url ${CT_REPORTS})


# XXX please, reduce number of files
FILE_COUNT=$(find "${CT_REPORTS}" -type f | wc -l)
echo "Uploading $FILE_COUNT files"
ls $CT_REPORTS


sudo pip install awscli --upgrade --user
sudo aws configure set aws_access_key_id $AWS_ACCESS_KEY_ID
sudo aws configure set aws_secret_access_key $AWS_SECRET_ACCESS_KEY
sudo aws configure set default.region $AWS_DEFAULT_REGION
sudo time aws s3 cp ${CT_REPORTS} s3://circleci-mim-results/${CT_REPORTS} --acl public-read --debug --recursive


