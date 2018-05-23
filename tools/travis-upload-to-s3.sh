#!/bin/bash

source tools/travis-helpers.sh

set -euo pipefail

CT_REPORTS=$(ct_reports_dir)

if [ ! -d "${CT_REPORTS}" ]; then
    echo "Skip uploading, because $CT_REPORTS directory does not exist"
    exit 0
fi

echo "Uploading test results to s3"
echo $(s3_url ${CT_REPORTS})

echo "Installing awscli"
sudo time pip install boto python-magic
wget -O tools/s3-parallel-put https://raw.githubusercontent.com/arcusfelis/s3-parallel-put/e2f66d215eb6b3e376f9bafc4f52891ce671eff0/s3-parallel-put
chmod +x tools/s3-parallel-put


AWS_S3_HOST="${AWS_S3_HOST:-s3.amazonaws.com}"

echo "Uploading"
time tools/s3-parallel-put --quiet --bucket_region=$AWS_DEFAULT_REGION --processes=32 --put=stupid --bucket=${AWS_BUCKET:-mongooseim-ct-results} --prefix=${CT_REPORTS} ${CT_REPORTS}
