#!/usr/bin/env bash
# This script uploads small- and big-test results to Google Drive. It uses
# Google service account credentials serialized (base64) into
# GDRIVE_SERVICE_ACCOUNT_CREDENTIALS environment variable. The script creates a
# directory structure under GDRIVE_PARENT_DIR directory (should be set to gdrive
# id of a directory; 'root' by default). The actual results are stored in a
# tar.gz archive.


# Debug failing commands
error_handling()
{
set -euo pipefail
}
error_handling

source tools/travis-helpers.sh

# gdrive 2.1.0 would return "No valid arguments given" error.
# we need to compile from source.
# https://github.com/gdrive-org/gdrive/issues/242
install_gdrive() {
    echo "Installing gdrive..."
    export GOPATH="/tmp/go"
    go get github.com/prasmussen/gdrive
}

# Calls gdrive executable with set credentials
export PATH="/tmp/go/bin:$PATH"

if ! hash gdrive; then
    install_gdrive
fi

JUST_INSTALL="${JUST_INSTALL:-0}"
if test 1 = "$JUST_INSTALL"; then
    echo "Exit after installing..."
    exit 0
fi

echo "Uploading test results to google drive"

MIM_GDRIVE_OPTS=""

gdrive() {
    local EXIT_CODE=0
    set +e
    command gdrive --config /tmp --service-account serviceAccountCredentials "$@"
    local EXIT_CODE=$?
    set -e
    if [ $EXIT_CODE != 0 ]; then
        >&2 echo FAILED command args: "$@"
        exit $EXIT_CODE
    fi
}

echo "${GDRIVE_SERVICE_ACCOUNT_CREDENTIALS}" | \
    base64 --decode > /tmp/serviceAccountCredentials

# Gets ID of a file located under a specific parent.
gdrive_get_id() {
    error_handling
    local PARENT_ID="$1"
    local NAME="$2"
    >&2 echo "gdrive_get_id PARENT_ID=$PARENT_ID"
    gdrive list --query "'${PARENT_ID}' in parents and name = '${NAME}'" | \
        tail -n+2 | head -n1 | awk '{print $1}'
}

# Creates a new directory under a specific parent.
gdrive_mkdir() {
    error_handling
    local PARENT_ID="$1"
    local NAME="$2"
    gdrive mkdir --parent "${PARENT_ID}" "${NAME}" | awk '{print $2}'
}

# Ensures existance of a directory. If a directory on the path already exists,
# it is reused. This is needed because in Google Drive, a directory may have
# multiple children with the same name.
gdrive_mkdir_p() {

    error_handling
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

CT_REPORTS_DIR="$(dirname ${CT_REPORTS_PATH})"

echo "CT_REPORTS_PATH=$CT_REPORTS_PATH"
test -d "$CT_REPORTS_DIR" || { echo "CT_REPORTS_DIR=$CT_REPORTS_DIR is not a directory"; exit 1; }

tar -czf "${CT_REPORT_ARCHIVE}" \
    -C "$CT_REPORTS_DIR" \
    "$(basename ${CT_REPORTS_PATH})"

gdrive upload --no-progress --parent "${PARENT_DIR}" "${CT_REPORT_ARCHIVE}"
FILE_ID=$(gdrive_get_id "${PARENT_DIR}" "$(basename ${CT_REPORT_ARCHIVE})")
echo "Uploaded with FILE_ID=$FILE_ID"
gdrive share "$FILE_ID"
gdrive info "$FILE_ID"

