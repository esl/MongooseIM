#!/bin/bash

source tools/travis-helpers.sh

set -euo pipefail

CT_REPORTS=$(ct_reports_dir)

echo "Uploading test results to s3"
echo $(s3_url ${CT_REPORTS})

echo "Installing awscli"
sudo time pip install awscli --ignore-installed six

echo "Uploading"
time aws s3 cp --recursive --quiet ${CT_REPORTS} s3://${AWS_BUCKET:-mongooseim-ct-results}/${CT_REPORTS}
