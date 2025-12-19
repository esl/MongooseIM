#!/bin/bash

set -eo pipefail

if [ -n "${CODECOV_SKIP_UPLOAD:-}" ]; then
  echo "Skipping Codecov upload (CODECOV_SKIP_UPLOAD is set)"
  exit 0
fi

# When CircleCI reruns only failed tests, we end up with partial coverage.
# The suite-selection logic writes selected suites to files; if they exist and are non-empty,
# avoid uploading partial coverage.
if [ -s "selected_small_suites" ] || [ -s "big_tests/selected_suites" ]; then
  echo "Skipping Codecov upload (selected suites rerun detected)"
  exit 0
fi

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
./tools/retry.sh ./codecov -t ${CODECOV_TOKEN} -e PRESET --nonZero
