#!/bin/bash

source tools/travis-helpers.sh

set -euo pipefail

echo "Uploading test results to google drive"
export GOPATH="/tmp/go"
go get github.com/prasmussen/gdrive

echo "${GDRIVE_SERVICE_ACCOUNT_CREDENTIALS}" | base64 --decode > /tmp/serviceAccountCredentials

gdrive() {
    /tmp/go/bin/gdrive --config /tmp --service-account serviceAccountCredentials "$@"
}

gdrive_get_id() {
    gdrive list --query "'$1' in parents and name = '$2'" | tail -n+2 | head -n1 | awk '{print $1}'
}

gdrive_mkdir() {
    gdrive mkdir --parent "$1" "$2" | awk '{print $2}'
}

gdrive_mkdir_p() {
    local PARENT="$1"
    local OLD_IFS="$IFS"
    export IFS='/'

    for NAME in $2; do
        local DIR_ID=$(gdrive_get_id "${PARENT}" "${NAME}")
        if [ -z "${DIR_ID}" ]; then DIR_ID=$(gdrive_mkdir "${PARENT}" "${NAME}"); fi
        PARENT="${DIR_ID}"
    done

    export IFS="${OLD_IFS}"
    echo "${PARENT}"
}

CT_REPORTS_PATH="$(ct_reports_dir)"
CT_REPORT_ARCHIVE="/tmp/$(basename ${CT_REPORTS_PATH}).tar.gz"
ROOT_DIR="${GDRIVE_PARENT_DIR:-root}"
PARENT_DIR="$(gdrive_mkdir_p ${ROOT_DIR} $(dirname ${CT_REPORTS_PATH}))"

tar -czf "${CT_REPORT_ARCHIVE}" -C "$(dirname ${CT_REPORTS_PATH})" "$(basename ${CT_REPORTS_PATH})"
gdrive upload --no-progress --parent "${PARENT_DIR}" "${CT_REPORT_ARCHIVE}"
