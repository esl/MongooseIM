#!/bin/bash

set -eo pipefail

PLATFORM=$(uname -m)
[ $PLATFORM == "x86_64" ] && PLATFORM=linux
BASE_URL="https://uploader.codecov.io/latest/$PLATFORM"

# Source: https://docs.codecov.com/docs/codecov-uploader#integrity-checking-the-uploader
curl https://keybase.io/codecovsecurity/pgp_keys.asc | gpg --no-default-keyring --keyring trustedkeys.gpg --import
curl -Os "${BASE_URL}/codecov"
curl -Os "${BASE_URL}/codecov.SHA256SUM"
curl -Os "${BASE_URL}/codecov.SHA256SUM.sig"
gpgv codecov.SHA256SUM.sig codecov.SHA256SUM
shasum -a 256 -c codecov.SHA256SUM

chmod +x codecov
./codecov -t ${CODECOV_TOKEN} -e PRESET
