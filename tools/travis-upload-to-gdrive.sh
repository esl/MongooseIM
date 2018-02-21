#!/bin/bash
# This script uploads small- and big-test results to Google Drive. It uses
# Google service account credentials serialized (base64) into
# GDRIVE_SERVICE_ACCOUNT_CREDENTIALS environment variable. The script creates a
# directory structure under GDRIVE_PARENT_DIR directory (should be set to gdrive
# id of a directory; 'root' by default). The actual results are stored in a
# tar.gz archive.

set -euo pipefail

source tools/travis-helpers.sh

echo "Uploading test results to google drive"
export GOPATH="/tmp/go"
go get github.com/prasmussen/gdrive

echo "${GDRIVE_SERVICE_ACCOUNT_CREDENTIALS}" | \
    base64 --decode > /tmp/serviceAccountCredentials

# Calls gdrive executable with set credentials
gdrive() {
    /tmp/go/bin/gdrive --config /tmp \
                       --service-account serviceAccountCredentials "$@"
}

# Gets ID of a file located under a specific parent.
gdrive_get_id() {
    local PARENT_ID="$1"
    local NAME="$2"
    gdrive list --query "'${PARENT_ID}' in parents and name = '${NAME}'" | \
        tail -n+2 | head -n1 | awk '{print $1}'
}

# Creates a new directory under a specific parent.
gdrive_mkdir() {
    local PARENT_ID="$1"
    local NAME="$2"
    gdrive mkdir --parent "${PARENT_ID}" "${NAME}" | awk '{print $2}'
}

# Ensures existance of a directory. If a directory on the path already exists,
# it is reused. This is needed because in Google Drive, a directory may have
# multiple children with the same name.
gdrive_mkdir_p() {
    local PARENT="$1"

    local OLD_IFS="$IFS"
    export IFS='/'

    for NAME in $2; do
        local DIR_ID=$(gdrive_get_id "${PARENT}" "${NAME}")
        if [ -z "${DIR_ID}" ]; then
            DIR_ID=$(gdrive_mkdir "${PARENT}" "${NAME}")
        fi
        PARENT="${DIR_ID}"
    done

    export IFS="${OLD_IFS}"
    echo "${PARENT}"
}

CT_REPORTS_PATH="$(ct_reports_dir)"
CT_REPORT_ARCHIVE="/tmp/$(basename ${CT_REPORTS_PATH}).tar.gz"
ROOT_DIR="${GDRIVE_PARENT_DIR:-root}"
PARENT_DIR="$(gdrive_mkdir_p ${ROOT_DIR} $(dirname ${CT_REPORTS_PATH}))"

tar -czf "${CT_REPORT_ARCHIVE}" \
    -C "$(dirname ${CT_REPORTS_PATH})" \
    "$(basename ${CT_REPORTS_PATH})"

gdrive upload --no-progress --parent "${PARENT_DIR}" "${CT_REPORT_ARCHIVE}"
