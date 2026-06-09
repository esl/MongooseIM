#!/usr/bin/env bash

set -eo pipefail

[ $(uname -m) == "x86_64" ] && PLATFORM=linux || PLATFORM=linux-arm64
BASE_URL="https://cli.codecov.io/latest/$PLATFORM"

# Source: https://docs.codecov.com/docs/codecov-uploader#integrity-checking-the-uploader
curl https://keybase.io/codecovsecops/pgp_keys.asc | gpg --import
curl -Os "${BASE_URL}/codecov"
curl -Os "${BASE_URL}/codecov.SHA256SUM"
curl -Os "${BASE_URL}/codecov.SHA256SUM.sig"
gpg --verify codecov.SHA256SUM.sig codecov.SHA256SUM
shasum -a 256 -c codecov.SHA256SUM

chmod +x codecov
./tools/retry.sh ./codecov upload-coverage \
                 -f codecov.json --disable-search -n $CIRCLE_JOB --plugin noop -Z
