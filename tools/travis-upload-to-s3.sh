#!/bin/bash

source tools/travis-helpers.sh

set -euo pipefail

CT_REPORTS=$(ct_reports_dir)

echo "Uploading test results to s3"
echo $(s3_url ${CT_REPORTS})
sudo pip install awscli --ignore-installed six
aws s3 sync --quiet ${CT_REPORTS} s3://${AWS_BUCKET:-mongooseim-ct-results}/${CT_REPORTS}
