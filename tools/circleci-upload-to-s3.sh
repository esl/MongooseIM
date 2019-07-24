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

S3PP_COMMIT=6fd430a54e976d2d580042efdf82ac2fb66d5e57
wget -O tools/s3-parallel-put https://raw.githubusercontent.com/arcusfelis/s3-parallel-put/$S3PP_COMMIT/s3-parallel-put
chmod +x tools/s3-parallel-put

sudo pip install boto python-magic
sudo pip install awscli --upgrade --user
aws configure set aws_access_key_id $AWS_ACCESS_KEY_ID
aws configure set aws_secret_access_key $AWS_SECRET_ACCESS_KEY
aws configure set default.region $AWS_DEFAULT_REGION

sudo tools/s3-parallel-put --quiet --processes=64 --put=stupid \
                    --host=s3.$AWS_DEFAULT_REGION.amazonaws.com  --bucket_region=$AWS_DEFAULT_REGION \
                    --bucket=circleci-mim-results --prefix=${CT_REPORTS} ${CT_REPORTS} --grant=public-read


