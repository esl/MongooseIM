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

echo "Installing s3-parallel-put based on boto"
sudo time pip install boto python-magic
# The fork of s3-parallel-put has some small optimizations.
S3PP_COMMIT=6fd430a54e976d2d580042efdf82ac2fb66d5e57
wget -O tools/s3-parallel-put https://raw.githubusercontent.com/arcusfelis/s3-parallel-put/$S3PP_COMMIT/s3-parallel-put
chmod +x tools/s3-parallel-put


# XXX please, reduce number of files
FILE_COUNT=$(find "${CT_REPORTS}" -type f | wc -l)
echo "Uploading $FILE_COUNT files"

AWS_BUCKET="${AWS_BUCKET:-mongooseim-ct-results}"
# We don't expect write conflicts, so we use put=stupid to reduce operations.
#
# Docs for the tool
# https://github.com/mishudark/s3-parallel-put
#
# 64 processes works better for our case, than 32 and 8 (default).
time tools/s3-parallel-put --quiet --processes=64 --put=stupid \
    --bucket_region=$AWS_DEFAULT_REGION --bucket=$AWS_BUCKET --prefix=${CT_REPORTS} ${CT_REPORTS}
