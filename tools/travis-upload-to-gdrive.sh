#!/bin/bash

source tools/travis-helpers.sh

set -euo pipefail

echo "Uploading test results to google drive"
export GOPATH="/tmp/go"
go get github.com/prasmussen/gdrive

echo "${GDRIVE_SERVICE_ACCOUNT_CREDENTIALS}" | base64 --decode > /tmp/serviceAccountCredentials; \
    /tmp/go/bin/gdrive upload --config /tmp --service-account serviceAccountCredentials \
                       --recursive --no-progress --parent "${GDRIVE_PARENT_DIR:-root}" $(ct_reports_base_dir); \
    dd conv=notrunc if=/dev/urandom of=/tmp/serviceAccountCredentials bs=1 count=$(du /tmp/serviceAccountCredentials | cut -f1)
